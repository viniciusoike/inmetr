get_colnames <- function(x, validate = TRUE, case_sensitive = FALSE, return_type = "vector") {
  # Comprehensive column name definitions with additional metadata
  col_names <- list(
    csv = list(
      names = c(
        "data_ymd",
        "hora_utc",
        "precipitacao_total_mm",
        "pressao_atm_hora_mB",
        "pressao_atm_max_mB",
        "pressao_atm_min_mB",
        "radiacao_global_kj_m2",
        "temp_bulbo_hora",
        "temp_orvalho_hora",
        "temp_max",
        "temp_min",
        "temp_orvalho_max",
        "temp_orvalho_min",
        "umidade_rel_max",
        "umidade_rel_min",
        "umidade_rel_hora",
        "vento_direcao",
        "vento_rajada_max",
        "vento_velocidade",
        "drop_col"
      ),
      description = "Meteorological CSV data columns",
      type = "numeric"
    ),
    metadata = list(
      names = c(
        "source",
        "name_region",
        "name_state",
        "code_station",
        "name_station",
        "date_start",
        "drop_col",
        "date_end"
      ),
      description = "Metadata columns for station information",
      type = "categorical"
    ),
    metadata_fixed = list(
      names = c(
        "source",
        "name_region",
        "name_state",
        "code_station",
        "name_station_full",
        "date_start",
        "drop_col",
        "date_end"
      ),
      description = "Metadata columns for station information",
      type = "categorical"
    ),
    metadata_csv_header = list(
      names = c(
        "name_region",
        "name_state",
        "name_station_full",
        "code_station",
        "st_lat",
        "st_lng",
        "st_altitude",
        "st_date_start"
      ),
      description = "Metadata columns for station information",
      type = "categorical"
    ),
    station = list(
      names = c(
        "name_station_full",
        "name_state",
        "st_situation",
        "st_lat",
        "st_lng",
        "st_altitude",
        "st_date_start_operation",
        "code_station"
      ),
      description = "Station-specific details",
      type = "mixed"
    )
  )

  # Input validation with enhanced error handling
  return_type <- match.arg(return_type)

  # Flexible matching with optional case sensitivity
  if (!case_sensitive) {
    x <- tolower(x)
    input_names <- tolower(names(col_names))
  } else {
    input_names <- names(col_names)
  }

  # Validate input
  if (validate) {
    tryCatch({
      # Check if input matches any predefined column set
      match_index <- match(x, input_names)

      if (is.na(match_index)) {
        cli::cli_alert_danger(
          "Invalid column set. Available sets: {paste(names(col_names), collapse = ', ')}"
        )
        stop("Invalid column set")
      }
    }, error = function(e) {
      cli::cli_alert_danger("Error in column set validation: {e$message}")
      return(NULL)
    })
  }

  # Retrieve column names
  result <- col_names[match(x, input_names)]

  # Return based on specified type
  if (return_type == "vector") {
    result <- result[[1]]$names
  }

  return(result)
}


#' Unzip INMET Meteorological Data Files
#'
#' Extracts ZIP files containing meteorological data
#'
#' @param data_dir Directory containing ZIP files
#' @param overwrite Logical, whether to overwrite existing extraction directories
#' @return Vector of extraction directories
inmet_unzip_data <- function(data_dir = NULL, overwrite = FALSE) {
  # Set default directory if not provided
  if (is.null(data_dir)) {
    data_dir <- here::here("data-raw", "data", "zipped_files")
  }

  # Find ZIP files
  zip_files <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE)

  # Validate ZIP files
  if (length(zip_files) == 0) {
    cli::cli_alert_danger("No ZIP files found in {data_dir}")
    return(NULL)
  }

  # Progress bar for unzipping
  cli::cli_progress_bar(
    total = length(zip_files),
    format = "{cli::pb_spin} Unzipping data [{cli::pb_current}/{cli::pb_total}]"
  )

  # Output directories
  out_dirs <- character(length(zip_files))

  # Unzip loop
  for (i in seq_along(zip_files)) {
    # Extract year from filename
    current_year <- gsub("\\.zip", "", basename(zip_files[[i]]))

    # Create output directory
    out_dir <- here::here(data_dir, paste0("inmet_", current_year))

    # Handle existing directories
    if (dir.exists(out_dir)) {
      if (overwrite) {
        unlink(out_dir, recursive = TRUE)
      } else {
        cli::cli_alert_warning("Directory {out_dir} already exists. Skipping.")
        next
      }
    }

    # Create directory and unzip
    dir.create(out_dir, recursive = TRUE)
    utils::unzip(zip_files[[i]], exdir = out_dir)

    # Store output directory
    out_dirs[i] <- out_dir

    # Update progress
    cli::cli_progress_update()
  }

  # Finish progress bar
  cli::cli_progress_done()

  # Return extraction directories
  return(out_dirs)
}

#' Find CSV Files in a Directory
#'
#' Locates CSV files recursively within a specified directory
#'
#' @param data_dir Directory to search for CSV files
#' @return Character vector of full file paths to CSV files
#' @keywords internal
find_path_csv <- function(data_dir = NULL) {
  # Set default directory if not provided
  if (is.null(data_dir)) {
    data_dir <- here::here("data-raw", "data", "zipped_files")
  }

  # Validate input
  if (!dir.exists(data_dir)) {
    cli::cli_alert_danger("Directory does not exist: {data_dir}")
    return(character(0))
  }

  # Find CSV files recursively
  path_files <- list.files(
    data_dir,
    pattern = "\\.CSV$|\\.csv$",  # Case-insensitive CSV extension
    recursive = TRUE,  # Search subdirectories
    full.names = TRUE  # Return full file paths
  )

  # Validate found files
  if (length(path_files) == 0) {
    cli::cli_alert_warning("No CSV files found in {data_dir}")
    return(character(0))
  }

  # Optional logging
  cli::cli_alert_success("Found {length(path_files)} CSV files")

  return(path_files)
}

fix_station_names <- function(y) {

  x_replacement <- c(
    "ARQ\\." = "Arquipelago",
    "PRES\\." = "Presidente",
    "MAL\\." = "Marechal",
    "MAL\\," = "Marechal",
    "S\\.J\\." = "Sao Jose",
    "S\\. G\\." = "Sao Gabriel",
    "S\\,J\\." = "Sao Jose",
    "S\\, G\\." = "Sao Gabriel",
    "COOPA-DF" = "COOPA/DF",
    "ARCO VERDE" = "ARCOVERDE",
    "BEBDOURO" = "BEBEDOURO",
    "MARAMBAIA" = "RIO DE JANEIRO - MARAMBAIA",
    "^PAMPULHA" = "BELO HORIZONTE - PAMPULHA",
    "MIR\\," = "MIRANTE ",
    "C, DO MATO DENTRO" = "CONCEICAO DO MATO DENTRO",
    "LAG\\," = "LAG.",
    "XAV\\," = "XAVANTINA"
  )

  y_replacement <- c(
    "^Pef" = "PEF",
    "^Dtcea" = "DTCEA",
    "Df$" = "DF"
  )

  br_replacement <- c(
    " Da " = " da ",
    " Do " = " do ",
    " Dos " = " dos ",
    " Das " = " das ",
    " De " = " de "
  )

  y = stringr::str_replace_all(y, x_replacement)
  y = stringr::str_to_title(y)
  y = stringr::str_replace_all(y, br_replacement)

  # Add a space between letter and (
  y = stringr::str_replace(y, "(?<=[A-z])\\(", " (")
  # Remove ) and replace ( for ' - ' (used to separate station name from city name)
  y = stringr::str_replace(y, "\\(", " - ")
  y = stringr::str_remove(y, "\\)$")
  # Add a space between a - in between letters (e.g. As-Za -> As - Za)
  y = stringr::str_replace(y, "(?<=[A-z])-(?=[A-z])", " - ")
  # Deal with an exception
  y = stringr::str_replace(y, "GUARDA MOR", "GUARDA-MOR")
  # Add a space between a - when there is a letter to the left  (e.g. As- Za -> As - Za)
  y = stringr::str_replace(y, "(?<=[A-z])-(?= )", " -")
  # Remove excess whitespaces
  y = stringr::str_squish(y)
  y = stringr::str_replace_all(y, y_replacement)

  return(y)

}
