get_metadata <- function(path, include_path = FALSE, verbose = FALSE) {
  # Validate input
  if (!file.exists(path)) {
    cli::cli_alert_danger("File not found: {path}")
    return(NULL)
  }

  # Expected column names
  col_names <- get_colnames("metadata")

  # Extract filename without extension
  name_file <- tools::file_path_sans_ext(basename(path))

  # Split filename and create metadata
  tryCatch({
    # Use stringr for robust splitting
    metadata_parts <- stringr::str_split(name_file, "_", simplify = TRUE)

    # Validate number of parts
    if (length(metadata_parts) != length(col_names)) {
      cli::cli_alert_warning(
        "Filename does not match expected format: {name_file}"
      )
      return(NULL)
    }

    # Create data.table with proper column names
    metadata_station <- data.table::data.table(
      matrix(metadata_parts, nrow = 1, dimnames = list(NULL, col_names))
    )

    metadata_station[, drop_col := NULL]

    # Optionally include file path
    if (include_path) {
      metadata_station[, path := path]
    }

    # Optional verbose output
    if (verbose) {
      cli::cli_alert_info("Metadata extracted successfully:")
      print(metadata_station)
    }

    return(metadata_station)

  }, error = function(e) {
    cli::cli_alert_danger("Error processing metadata: {e$message}")
    return(NULL)
  })
}

# get_metadata <- function(p, include_path = FALSE) {
#
#   id_variables <- c(
#     "source", "name_region", "name_state",
#     "code_station", "name_station"
#   )
#
#   metadata <- inmet_metadata[path == p,
#                        .(min_date = min(date_start), max_date = max(date_end)),
#                        by = id_variables]
#
#   if (include_path) {
#     return(metadata)
#   } else {
#     metadata[, path := NULL]
#     return(metadata)
#   }
#
# }



get_metadata_csv <- function(path, include_path = FALSE, col_names = "en") {

  stopifnot(col_names %in% c("en", "pt"))

  x <- data.table::fread(
    path,
    nrows = 8,
    header = FALSE,
    col.names = c("V1", "V2"),
    encoding = "Latin-1"
    )

  y <- x[["V2"]]

  if (col_names == "en") {
    header <- get_colnames("metadata_csv_header")
  } else {
    header <- c(
      "regiao", "uf", "estacao", "codigo_wmo", "latitude", "longitude", "altitude", "data_de_fundacao"
      )
  }

  dat <- data.table::as.data.table(t(y))
  data.table::setnames(dat, header)

  if (col_names == "en") {
    as_numeric_comma <- \(x) as.numeric(stringr::str_replace(x, "\\,", "\\."))
    num_cols <- c("st_lat", "st_lng", "st_altitude")
    dat[, (num_cols) := lapply(.SD, as_numeric_comma), .SDcols = num_cols]
    dat[, st_date_start := as.Date(st_date_start, format = "%Y-%m-%d")]
  } else {
    as_numeric_comma <- \(x) as.numeric(stringr::str_replace(x, "\\,", "\\."))
    num_cols <- c("latitude", "longitude", "altitude")
    dat[, (num_cols) := lapply(.SD, as_numeric_comma), .SDcols = num_cols]
    dat[, data_de_fundacao := as.Date(data_de_fundacao, format = "%Y-%m-%d")]
  }

  if (include_path) {
    dtpath <- data.table::data.table(file_path = path)
    dat <- cbind(dtpath, dat)
  }

  return(dat)

}


# get_metadata <- function(station = NULL, city = station) {
#
#   if (!is.null(station)) {
#
#     available_stations <- unique(inmet_metadata$name_station)
#
#     stopifnot(station %in% available_stations)
#
#     meta = inmet_metadata[name_station == station]
#
#
#   }
#
#   if (!is.null(city)) {
#
#     available_cities <- unique(inmet_metadata$name_city)
#
#     stopifnot(city %in% available_cities)
#
#     meta = inmet_metadata[name_city == city]
#
#
#   }
#
#     id_variables <- c(
#       "source", "name_region", "name_state",
#       "code_station", "name_station"
#     )
#
#     meta <- meta[,
#                  .(min_date = min(date_start), max_date = max(date_end)),
#                  by = id_variables]
#
#     return(meta)
#
# }
