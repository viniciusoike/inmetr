.connection <- new.env(parent = emptyenv())

.get_connection <- function() {
  if (is.null(.connection$con) || !DBI::dbIsValid(.connection$con)) {
    .connection$con <- DBI::dbConnect(
      duckdb::duckdb(shared_home = FALSE),
      dbdir = ":memory:"
    )
  }
  return(.connection$con)
}

.onUnload <- function(libpath) {
  if (!is.null(.connection$con) && DBI::dbIsValid(.connection$con)) {
    DBI::dbDisconnect(.connection$con, shutdown = TRUE)
  }
  return(invisible(NULL))
}

.read_sql <- function(paths, schema, call = rlang::caller_env()) {
  con <- .get_connection()
  quoted <- vapply(
    paths,
    function(path) {
      return(as.character(DBI::dbQuoteString(con, normalizePath(path))))
    },
    character(1)
  )
  expected_names <- vapply(schema, `[[`, character(1), "name")
  hive_types <- if ("year" %in% expected_names) {
    ", hive_types = {'year': INTEGER}"
  } else {
    ""
  }
  sql <- sprintf(
    "SELECT * FROM read_parquet([%s], hive_partitioning = true%s)",
    paste(quoted, collapse = ", "),
    hive_types
  )
  actual <- DBI::dbGetQuery(con, paste("DESCRIBE", sql))
  expected_types <- vapply(schema, `[[`, character(1), "type")
  if (
    !identical(actual$column_name, expected_names) ||
      !identical(actual$column_type, expected_types)
  ) {
    cli::cli_abort(
      "Parquet columns or types do not match the data manifest.",
      call = call
    )
  }
  return(sql)
}

#' Read INMET observations lazily
#'
#' Downloads the selected yearly Parquet files to the cache once and returns
#' a lazy table. Filter and summarise it with dplyr, then call
#' [dplyr::collect()] to bring the result into R.
#'
#' Values are INMET's as published, except that `-9999` and blanks become
#' `NA`. The release applies no outlier filter or coverage threshold. Its
#' last year is usually partial: the `2026-10-24` release ends on
#' 28 February 2025.
#'
#' @section Hourly table:
#' One row per station and hour in INMET's files, including hours where every
#' measurement is missing.
#'
#' | Column | Meaning |
#' | --- | --- |
#' | `code_station` | INMET station code |
#' | `timestamp_utc` | Observation time, UTC |
#' | `date_local`, `month`, `day`, `hour_local` | Local date and its parts, at the station's fixed standard UTC offset (no daylight saving) |
#' | `year` | UTC year; `years` selects on it |
#' | `precip_mm` | Precipitation in the hour, mm |
#' | `pressure_station_hpa` | Station pressure, hPa |
#' | `pressure_max_hpa`, `pressure_min_hpa` | Pressure extremes in the previous hour, hPa |
#' | `radiation_kj_m2` | Global radiation, kJ/m² |
#' | `temp_dry_bulb_c` | Dry-bulb air temperature, °C |
#' | `temp_dew_point_c` | Dew-point temperature, °C |
#' | `temp_max_c`, `temp_min_c` | Air-temperature extremes in the previous hour, °C |
#' | `temp_dew_point_max_c`, `temp_dew_point_min_c` | Dew-point extremes in the previous hour, °C |
#' | `rel_humidity_pct` | Relative humidity, % |
#' | `rel_humidity_max_pct`, `rel_humidity_min_pct` | Relative-humidity extremes in the previous hour, % |
#' | `wind_direction_deg` | Wind direction, degrees |
#' | `wind_speed_ms` | Wind speed, m/s |
#' | `wind_gust_ms` | Maximum gust in the previous hour, m/s |
#' | `code_muni`, `name_muni`, `abbrev_state` | Station's IBGE municipality code, name, and state |
#'
#' @section Daily table:
#' The pipeline aggregates hourly rows by station and `date_local`. Every
#' available hour contributes, and an aggregate is `NA` only when all of its
#' hourly inputs are missing, so a daily sum is never zero for lack of data.
#'
#' | Column | Meaning |
#' | --- | --- |
#' | `code_station` | INMET station code |
#' | `date` | Local date |
#' | `year`, `month`, `day` | Parts of the local date; `years` selects on `year` |
#' | `day_of_year` | Day of the year, 1–366 |
#' | `week`, `weekday` | ISO week (1–53) and ISO weekday (Monday = 1) |
#' | `n_hours` | Hours with at least one non-missing measurement, 0–24 |
#' | `temp_mean_c` | Mean dry-bulb temperature, °C |
#' | `temp_min_c`, `temp_max_c` | Lowest and highest hourly air-temperature extreme, °C; see below |
#' | `temp_dew_point_mean_c` | Mean dew-point temperature, °C |
#' | `rel_humidity_mean_pct` | Mean relative humidity, % |
#' | `rel_humidity_min_pct`, `rel_humidity_max_pct` | Lowest and highest hourly relative-humidity extreme, %; see below |
#' | `precip_total_mm` | Total precipitation, mm |
#' | `radiation_total_kj_m2` | Total global radiation, kJ/m² |
#' | `pressure_station_mean_hpa` | Mean station pressure, hPa |
#' | `wind_speed_mean_ms` | Mean wind speed, m/s |
#' | `wind_gust_max_ms` | Highest gust, m/s |
#' | `code_muni`, `name_muni`, `abbrev_state` | Station's IBGE municipality code, name, and state |
#'
#' Three features of the daily table need care.
#'
#' * **Empty days.** INMET's files list hours with no measurements, so the
#'   daily table has rows with `n_hours == 0` and every measurement `NA`.
#'   Counting rows per station-year therefore counts calendar days, not days
#'   with data. Filter on `n_hours` first.
#' * **`n_hours` counts hours with any measurement.** A day can have
#'   `n_hours == 24` while one variable is missing all day. Coverage rules for
#'   a single variable need the hourly table; the example below counts hours
#'   with `temp_max_c`.
#' * **Extremes fall back to the hourly reading.** For each hour,
#'   `temp_max_c` uses the hourly maximum when present and `temp_dry_bulb_c`
#'   otherwise. `temp_min_c`, `rel_humidity_max_pct` and
#'   `rel_humidity_min_pct` work the same way. When the extreme field is
#'   missing for long spells, the daily extremes come from the hourly reading
#'   alone.
#'
#' @param table Either `"daily"` or `"hourly"`.
#' @param years Years to download; `NULL` selects all available years.
#' @return A lazy dbplyr table backed by DuckDB.
#' @export
#' @examplesIf interactive()
#' daily <- read_inmet("daily", 2024)
#' dplyr::collect(dplyr::filter(daily, .data$code_muni == 3550308L))
#'
#' # Hours per day with a measured temp_max_c, to drop days that rely on the
#' # dry-bulb fallback
#' read_inmet("hourly", 2021) |>
#'   dplyr::filter(.data$code_station == "A409") |>
#'   dplyr::summarise(
#'     n_hours_temp_max = sum(as.integer(!is.na(.data$temp_max_c)), na.rm = TRUE),
#'     .by = c("code_station", "date_local")
#'   ) |>
#'   dplyr::collect()
read_inmet <- function(table = c("daily", "hourly"), years = NULL) {
  call <- sys.call()
  table <- rlang::arg_match(table)
  manifest <- .manifest(call = call)
  entries <- manifest$tables[[table]]$files
  available <- as.integer(vapply(
    entries,
    function(entry) return(sub(".*year=([0-9]{4})/.*", "\\1", entry$path)),
    character(1)
  ))
  if (is.null(years)) {
    size <- sum(vapply(entries, `[[`, numeric(1), "size")) / 1024^2
    cli::cli_inform(
      "Downloading all {table} years may require {round(size, 1)} MB."
    )
    years <- available
  }
  if (
    !is.numeric(years) ||
      !length(years) ||
      anyNA(years) ||
      any(!is.finite(years)) ||
      any(years != floor(years)) ||
      any(!years %in% available)
  ) {
    cli::cli_abort(
      "{.arg years} must contain available years: {paste(available, collapse = ', ')}.",
      call = call
    )
  }
  selected <- entries[available %in% unique(as.integer(years))]
  paths <- vapply(selected, .ensure_file, character(1), call = call)
  sql <- .read_sql(paths, manifest$tables[[table]]$schema, call = call)
  return(dplyr::tbl(.get_connection(), dbplyr::sql(sql)))
}

#' Read INMET station metadata
#' @return A tibble of station metadata.
#' @export
#' @examplesIf interactive()
#' inmet_stations()
inmet_stations <- function() {
  call <- sys.call()
  manifest <- .manifest(call = call)
  entry <- manifest$tables$stations$files[[1]]
  path <- .ensure_file(entry, call = call)
  sql <- .read_sql(path, manifest$tables$stations$schema, call = call)
  return(tibble::as_tibble(DBI::dbGetQuery(.get_connection(), sql)))
}
