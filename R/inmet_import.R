#' Title
#'
#' @param station Name or code of a station. Defaults to `"all"`.
#' @param include_metadata Logical indicating if additional metadata should be
#' appended to the final table
#' @param metadata_list If `TRUE` returns a named list with the lalala
#'
#' @returns Either a `data.table` or a named `list` of two `data.table`.
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom parallel mclapply
# inmet_import <- function(
#     station = "all",
#     include_metadata = TRUE,
#     metadata_list = TRUE,
#     metadata_condense = TRUE) {
# # browser()
#   path_files <- find_path_csv()
#
#   available_stations <- c(
#     unique(inmet_metadata$name_station),
#     unique(inmet_metadata$code_station)
#   )
#
#   if (length(station) == 1 && station == "all") {
#
#     ls <- parallel::mclapply(path_files, inmet_import_csv, include_metadata, metadata_list)
#
#     if (metadata_list) {
#       dat <- lapply(ls, "[[", 1)
#       metadata <- lapply(ls, "[[", 2)
#
#       dat <- data.table::rbindlist(dat)
#       metadata <- unique(data.table::rbindlist(metadata))
#
#       if (metadata_condense) {
#         id_variables <- c("source", "name_region", "name_state", "code_station", "name_station")
#         metadata <- metadata[,
#                              .(min_date = min(date_start), max_date = max(date_end)),
#                              by = id_variables]
#       }
#
#       out <- list(inmet = dat, metadata = metadata)
#
#       return(out)
#     } else {
#       dat <- data.table::rbindlist(ls)
#       return(dat)
#     }
#
#   }
#
#   if (all(station %in% available_stations)) {
#
#     metadata_path <- parallel::mclapply(path_files, get_metadata, include_path = TRUE)
#     metadata_path <- data.table::rbindlist(metadata_path)
#
#     sub_station <- metadata_path[code_station %in% station | name_station %in% station]
#     sub_path <- sub_station[["path"]]
#     ls <- parallel::mclapply(sub_path, inmet_import_csv, include_metadata, metadata_list)
#
#     if (metadata_list) {
#       dat <- lapply(ls, "[[", 1)
#       metadata <- lapply(ls, "[[", 2)
#
#       dat <- data.table::rbindlist(dat)
#       metadata <- unique(data.table::rbindlist(metadata))
#
#       if (metadata_condense) {
#         id_variables <- c("source", "name_region", "name_state", "code_station", "name_station")
#         metadata <- metadata[,
#                              .(min_date = min(date_start), max_date = max(date_end)),
#                              by = id_variables]
#       }
#
#       out <- list(inmet = dat, metadata = metadata)
#
#       return(out)
#     } else {
#       dat <- data.table::rbindlist(ls)
#       return(dat)
#     }
#
#   }
#
# }

#' Import INMET Meteorological Data
#'
#' Imports meteorological data for specified stations from INMET CSV files.
#' Supports importing data for all stations or specific stations.
#'
#' @param station Character vector specifying stations to import.
#'   Can be station names or codes. Defaults to `"all"` to import all stations.
#' @param include_metadata Logical. If `TRUE`, includes station metadata with the data.
#' @param metadata_list Logical. If `TRUE`, returns a list with data and metadata.
#'   If `FALSE`, returns only the meteorological data.
#' @param metadata_condense Logical. If `TRUE`, condenses metadata to unique
#'   station information with min and max dates.
#'
#' @return
#' \itemize{
#'   \item If `metadata_list = TRUE`: A list with two data.tables
#'     \itemize{
#'       \item `inmet`: Meteorological data
#'       \item `metadata`: Station metadata
#'     }
#'   \item If `metadata_list = FALSE`: A single data.table with meteorological data
#' }
#'
#' @details
#' \enumerate{
#'   \item Finds all CSV files in the default or specified directory
#'   \item Imports data for all stations or specified stations
#'   \item Optionally includes and processes station metadata
#'   \item Supports parallel processing for efficient data import
#' }
#'
#' @examples
#' \dontrun{
#' # Import all station data
#' all_data <- inmet_import()
#'
#' # Import specific stations
#' specific_stations <- inmet_import(
#'   station = c("A001", "STATION_NAME"),
#'   include_metadata = TRUE
#' )
#'
#' # Import without metadata
#' data_only <- inmet_import(metadata_list = FALSE)
#' }
#'
#' @importFrom data.table rbindlist
#' @importFrom parallel mclapply
#'
#' @export
inmet_import <- function(
    station = "all",
    include_metadata = TRUE,
    metadata_list = TRUE,
    metadata_condense = TRUE
) {
  # Validate input parameters
  if (!is.character(station)) {
    cli::cli_abort("Station must be a character vector")
  }

  # Find CSV files
  path_files <- find_path_csv()

  if (length(path_files) == 0) {
    cli::cli_alert_danger("No CSV files found")
    return(NULL)
  }

  # Validate available stations
  available_stations <- c(
    unique(inmet_metadata$name_station),
    unique(inmet_metadata$code_station)
  )

  # Import all stations
  if (length(station) == 1 && station == "all") {
    return(.import_all_stations(
      path_files,
      include_metadata,
      metadata_list,
      metadata_condense
    ))
  }

  # Import specific stations
  if (all(station %in% available_stations)) {
    return(.import_specific_stations(
      path_files,
      station,
      include_metadata,
      metadata_list,
      metadata_condense
    ))
  }

  # Handle invalid stations
  invalid_stations <- setdiff(station, available_stations)
  cli::cli_alert_warning(
    "Invalid stations: {paste(invalid_stations, collapse = ', ')}"
  )

  return(NULL)
}

# Internal function to import all stations
.import_all_stations <- function(
    path_files,
    include_metadata,
    metadata_list,
    metadata_condense
) {

  # Parallel import of CSV files
  ls <- parallel::mclapply(
    path_files,
    inmet_import_csv,
    include_metadata,
    metadata_list
  )

  # Process imported data
  if (metadata_list) {
    return(.process_metadata_list(ls, metadata_condense))
  } else {
    return(data.table::rbindlist(ls))
  }
}

# Internal function to import specific stations
.import_specific_stations <- function(
    path_files,
    station,
    include_metadata,
    metadata_list,
    metadata_condense
) {
  # Get metadata for all files
  metadata_path <- parallel::mclapply(
    path_files,
    get_metadata,
    include_path = TRUE
  )
  metadata_path <- data.table::rbindlist(metadata_path)

  # Filter stations
  sub_station <- metadata_path[
    code_station %in% station | name_station %in% station
  ]
  sub_path <- sub_station[["path"]]

  # Parallel import of selected station files
  ls <- parallel::mclapply(
    sub_path,
    inmet_import_csv,
    include_metadata,
    metadata_list
  )

  # Process imported data
  if (metadata_list) {
    return(.process_metadata_list(ls, metadata_condense))
  } else {
    return(data.table::rbindlist(ls))
  }
}

# Internal function to process metadata list
.process_metadata_list <- function(ls, metadata_condense) {
  dat <- lapply(ls, "[[", 1)
  metadata <- lapply(ls, "[[", 2)

  dat <- data.table::rbindlist(dat)
  metadata <- unique(data.table::rbindlist(metadata))

  if (metadata_condense) {
    id_variables <- c(
      "source", "name_region", "name_state",
      "code_station", "name_station"
    )
    metadata <- metadata[,
                         .(min_date = min(date_start), max_date = max(date_end)),
                         by = id_variables
    ]
  }

  return(list(inmet = dat, metadata = metadata))
}

#' Read INMET's csv files
#'
#' @description
#' A helper function to import INMET's csv files. For internal use.
#'
#'
#' @param path A string defining the path to the csv file. Should be used with
#' `find_path_csv()`.
#' @param skip Passed onto `data.table::fread()`
#'
#' @returns A `data.table`
#'
#' @importFrom data.table fread setnames
inmet_read_csv <- function(path, skip = 8) {

  dat <- data.table::fread(
    file = path,
    skip = skip,
    # colClasses = c("Date", "character", rep("numeric", 18)),
    na.strings = "-9999",
    encoding = "Latin-1"
  )

  col_names <- get_colnames("csv")
  data.table::setnames(dat, names(dat), col_names)

  return(dat)

}

#' Clean INMET data
#'
#' @param dat A `data.table` imported using `inmet_read_csv()`
#' @param include_metadata Logical indicating if additional metadata should be
#' appended to the final table
#'
#' @returns A `data.table`
#'
#' @importFrom lubridate day hour month ymd_hm
#' @importFrom data.table setDT := is.data.table
inmet_clean_data <- function(dat, include_metadata = TRUE) {

  if (!is.data.table(dat)) {
    data.table::setDT(dat)
  }

  as_numeric_comma <- function(x) {
    x <- stringr::str_replace(x, ",", ".")
    x <- as.numeric(x)
    return(x)
  }

  dat[, data_ymd := as.Date(data_ymd)]

  num_cols <- names(dat)[3:20]
  dat[, (num_cols) := lapply(.SD, as_numeric_comma), .SDcols = num_cols]

  dat[, drop_col := NULL]

  dat[, data_ymd_hms := lubridate::ymd_hm(paste(data_ymd, hora_utc))]
  dat[, data_hora := lubridate::hour(data_ymd_hms)]
  dat[, data_dia := lubridate::day(data_ymd)]
  dat[, data_mes := lubridate::month(data_ymd)]

  return(dat)

}

#' Import CSV from INMET
#'
#' @description
#' A simple wrapper function to help import CSV files from INMET.
#'
#' @param path A string defining the path to the csv file. Should be used with
#' `find_path_csv()`.
#' @param include_metadata Logical indicating if additional metadata should be
#' appended to the final table
#' @param metadata_list Logical indicating if the metadata should appear as a separate
#' table in a two element list. If `FALSE` uses `cbind` and the metadata is stacked.
#'
#' @returns A `data.table`
inmet_import_csv <- function(path, include_metadata = TRUE, metadata_list = FALSE) {

  dat <- inmet_read_csv(path)
  dat <- inmet_clean_data(dat)

  metadata <- get_metadata(path)

  if (include_metadata) {

    if (metadata_list) {
      out <- list(
        inmet = cbind(dat, metadata[, .(code_station, name_station)]),
        metadata = metadata
        )

    } else {
      out <- cbind(dat, metadata)
    }
    return(out)
  } else {
    out <- cbind(dat, metadata[, .(code_station, name_station)])
  }

  return(out)

}

#' Title
#'
#' @param data_dir Lalalala
#'
#' @returns A `character` vector
find_path_csv <- function(data_dir = NULL) {

  if (is.null(data_dir)) {
    data_dir <- here("data-raw", "data", "zipped_files")
  }

  path_files <- list.files(
    data_dir,
    pattern = "\\.CSV$|\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  return(path_files)

}
