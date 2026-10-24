#' List Available INMET Meteorological Stations
#'
#' Retrieves metadata for all available meteorological stations
#'
#' @param data_dir Directory containing CSV files
#' @param unique_stations Logical, return only unique stations
#' @param verbose Provide detailed logging
#' @return A data.table with station metadata
#' @importFrom data.table .N
#' @export
inmet_list_stations <- function(
    data_dir = NULL,
    unique_stations = TRUE,
    verbose = TRUE
) {
  # Find CSV file paths
  path_files <- find_path_csv(data_dir)

  # Validate file paths
  if (length(path_files) == 0) {
    cli::cli_alert_danger("No CSV files found")
    return(NULL)
  }

  # Progress bar for metadata extraction
  cli::cli_progress_bar(
    total = length(path_files),
    format = "{cli::pb_spin} Extracting station metadata ({cli::pb_current}/{cli::pb_total})"
  )

  # Extract metadata with error handling
  path_metadata <- lapply(path_files, function(path) {
    tryCatch({
      metadata <- get_metadata(path, include_path = FALSE)
      cli::cli_progress_update()
      return(metadata)
    }, error = function(e) {
      cli::cli_alert_warning("Error extracting metadata from {basename(path)}: {e$message}")
      cli::cli_progress_update()
      return(NULL)
    })
  })

  cli::cli_progress_done()

  # Remove NULL entries
  path_metadata <- Filter(Negate(is.null), path_metadata)

  # Combine metadata
  metadata_path <- tryCatch(
    data.table::rbindlist(path_metadata),
    error = function(e) {
      cli::cli_alert_danger("Error combining metadata: {e$message}")
      return(NULL)
    }
  )

  # Validate metadata
  if (is.null(metadata_path) || nrow(metadata_path) == 0) {
    cli::cli_alert_danger("No station metadata found")
    return(NULL)
  }

  # Optional unique stations
  if (unique_stations) {
    metadata_path <- unique(
      metadata_path,
      by = c("code_station", "name_station")
    )
  }

  # Verbose output
  if (verbose) {
    cli::cli_alert_success(
      "Found {nrow(metadata_path)} {if(unique_stations) 'unique '}stations"
    )

    # Optional detailed station summary
    station_summary <- metadata_path[,
                                     .(
                                       total_stations = .N,
                                       states = paste(unique(name_state), collapse = ", ")
                                     )
    ]

    cli::cli_alert_info(
      "Stations across {length(unique(metadata_path$name_state))} states"
    )
  }

  return(metadata_path)
}

#' Filter INMET Stations
#'
#' Advanced filtering of station metadata
#'
#' @param stations Station metadata from inmet_list_stations()
#' @param state Filter by state name
#' @param region Filter by region name
#' @param date_start Filter stations active after this date
#' @return Filtered data.table of stations
#' @export
inmet_filter_stations <- function(
    stations = NULL,
    state = NULL,
    region = NULL,
    date_start = NULL
) {
  # If no stations provided, get all stations
  if (is.null(stations)) {
    stations <- inmet_list_stations(unique_stations = TRUE)
  }

  # Filter by state
  if (!is.null(state)) {
    stations <- stations[name_state %in% state]
  }

  # Filter by region
  if (!is.null(region)) {
    stations <- stations[name_region %in% region]
  }

  # Filter by start date
  if (!is.null(date_start)) {
    # Convert to Date if not already
    date_start <- as.Date(date_start)

    stations <- stations[as.Date(st_date_start_operation) >= date_start]
  }

  # Validate filtered results
  if (nrow(stations) == 0) {
    cli::cli_alert_warning("No stations match the specified criteria")
    return(NULL)
  }

  return(stations)
}
