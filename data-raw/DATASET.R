## code to prepare `DATASET` dataset goes here
source("R/utils.R")

get_dim_station <- function(st_dir = NULL, verbose = FALSE) {
  # Use cli for informative messages
  cli::cli_h1("Retrieving Dimension Stations")

  # Default directory with error handling
  if (is.null(st_dir)) {
    tryCatch({
      st_dir <- here::here("data-raw", "dim_stations")
    }, error = function(e) {
      cli::cli_alert_danger("Error creating default directory path: {e$message}")
      stop("Cannot determine default directory")
    })
  }

  # Validate directory existence
  if (!dir.exists(st_dir)) {
    cli::cli_alert_danger("Directory does not exist: {st_dir}")
    stop("Invalid directory path")
  }

  # Find CSV files with error handling
  csv_files <- list.files(st_dir, pattern = "\\.csv$", full.names = TRUE)

  # Validate file count
  if (length(csv_files) != 2) {
    cli::cli_alert_warning(
      "Expected 2 CSV files, found {length(csv_files)}: {paste(csv_files, collapse = ', ')}"
    )
    stop("Incorrect number of CSV files")
  }

  # Optimized file reading with error handling
  safely_fread <- function(file) {
    tryCatch({
      data.table::fread(file,
                        showProgress = verbose,
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8")
    }, error = function(e) {
      cli::cli_alert_danger("Error reading file {basename(file)}: {e$message}")
      return(NULL)
    })
  }
  # Read csv files
  ls_dim_station <- lapply(csv_files, safely_fread)
  # Remove any NULL entries from failed reads
  ls_dim_station <- Filter(Negate(is.null), ls_dim_station)

  # Combine data tables
  dim_station <- data.table::rbindlist(ls_dim_station, fill = TRUE)

  # Get and set column names with error handling
  tryCatch({
    col_names <- get_colnames("station", return_type = "vector")
    data.table::setnames(dim_station, names(dim_station), col_names)
  }, error = function(e) {
    cli::cli_alert_danger("Error setting column names: {e$message}")
    stop("Column name assignment failed")
  })

  # Date parsing with error handling
  tryCatch({
    dim_station[, st_date_start_operation := lubridate::dmy(st_date_start_operation)]
  }, error = function(e) {
    cli::cli_alert_warning("Error parsing date column: {e$message}")
    # Optionally, keep original column or handle differently
  })

  # Optional verbose output
  if (verbose) {
    cli::cli_alert_success("Dimension stations data loaded successfully")
    cli::cli_alert_info("Rows: {nrow(dim_station)}, Columns: {ncol(dim_station)}")
  }

  return(data.table::setDT(dim_station))
}

dim_station <- get_dim_station()

usethis::use_data(dim_station, overwrite = TRUE, internal = TRUE)
