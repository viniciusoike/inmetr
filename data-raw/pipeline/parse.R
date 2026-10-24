# Published CSV schema ----

inmet_variables <- c(
  "precip_mm",
  "pressure_station_hpa",
  "pressure_max_hpa",
  "pressure_min_hpa",
  "radiation_kj_m2",
  "temp_dry_bulb_c",
  "temp_dew_point_c",
  "temp_max_c",
  "temp_min_c",
  "temp_dew_point_max_c",
  "temp_dew_point_min_c",
  "rel_humidity_max_pct",
  "rel_humidity_min_pct",
  "rel_humidity_pct",
  "wind_direction_deg",
  "wind_gust_ms",
  "wind_speed_ms"
)

read_inmet_header <- function(path) {
  lines <- iconv(
    readLines(path, n = 9L, encoding = "bytes", warn = FALSE),
    from = "ISO-8859-1",
    to = "UTF-8"
  )
  if (length(lines) != 9L) {
    cli::cli_abort("Incomplete INMET header in {.file {path}}.")
  }
  keys <- vapply(strsplit(lines[1:8], ";", fixed = TRUE), `[[`, "", 1L)
  valid_keys <- c(
    "UF:",
    "CODIGO (WMO):",
    "LATITUDE:",
    "LONGITUDE:",
    "ALTITUDE:"
  )
  if (!identical(keys[c(2, 4:7)], valid_keys)) {
    cli::cli_abort("Unknown INMET metadata header in {.file {path}}.")
  }
  fields <- strsplit(lines[[9]], ";", fixed = TRUE)[[1]]
  reference <- iconv(
    readLines(
      here::here("data-raw", "pipeline", "tests", "fixtures", "old.CSV"),
      n = 9L,
      encoding = "bytes"
    )[[9]],
    from = "ISO-8859-1",
    to = "UTF-8"
  )
  reference <- strsplit(reference, ";", fixed = TRUE)[[1]]
  if (
    length(fields) != 19L ||
      !identical(tolower(fields[3:19]), tolower(reference[3:19])) ||
      !identical(fields[1:2], c("DATA (YYYY-MM-DD)", "HORA (UTC)")) &&
        !identical(fields[1:2], c("Data", "Hora UTC"))
  ) {
    cli::cli_abort("Unknown INMET header in {.file {path}}.")
  }
  values <- vapply(
    strsplit(lines[1:8], ";", fixed = TRUE),
    function(x) if (length(x) >= 2L) x[[2]] else NA_character_,
    ""
  )
  if (anyNA(values[c(2, 4:7)])) {
    cli::cli_abort("Missing station metadata in {.file {path}}.")
  }
  return(list(
    code_station = values[[4]],
    name_region = values[[1]],
    abbrev_state = values[[2]],
    name_station = values[[3]],
    lat = as.numeric(sub(",", ".", values[[5]], fixed = TRUE)),
    lon = as.numeric(sub(",", ".", values[[6]], fixed = TRUE)),
    altitude_m = as.numeric(sub(",", ".", values[[7]], fixed = TRUE)),
    date_start_operation = values[[8]],
    format = if (fields[[1]] == "Data") "new" else "old"
  ))
}

read_inmet_csv <- function(path) {
  metadata <- read_inmet_header(path)
  dat <- data.table::fread(
    path,
    skip = 8L,
    header = TRUE,
    sep = ";",
    dec = ",",
    na.strings = c("-9999", ""),
    encoding = "Latin-1",
    showProgress = FALSE
  )
  if (ncol(dat) != 20L || any(!is.na(dat[[20L]]) & dat[[20L]] != "")) {
    cli::cli_abort(
      "Unexpected CSV column count or nonempty final column in {.file {path}}."
    )
  }
  data.table::setnames(
    dat,
    c("date_utc", "hour_utc", inmet_variables, "trailing")
  )
  dat[, trailing := NULL]
  date_pattern <- if (metadata$format == "new") {
    "^[0-9]{4}/[0-9]{2}/[0-9]{2}$"
  } else {
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  }
  hour_pattern <- if (metadata$format == "new") {
    "^[0-2][0-9]00 UTC$"
  } else {
    "^[0-2][0-9]:00$"
  }
  if (
    any(!grepl(date_pattern, dat$date_utc)) ||
      any(!grepl(hour_pattern, dat$hour_utc))
  ) {
    cli::cli_abort("Unknown date/hour format in {.file {path}}.")
  }
  hour <- as.integer(substr(dat$hour_utc, 1L, 2L))
  if (any(hour > 23L)) {
    cli::cli_abort("Invalid UTC hour in {.file {path}}.")
  }
  date <- as.Date(gsub("/", "-", dat$date_utc, fixed = TRUE))
  if (anyNA(date)) {
    cli::cli_abort("Invalid UTC date in {.file {path}}.")
  }
  dat[, timestamp_utc := as.POSIXct(date, tz = "UTC") + hour * 3600]
  dat[, c("date_utc", "hour_utc") := NULL]
  dat[, code_station := metadata$code_station]
  for (column in inmet_variables) {
    data.table::set(dat, j = column, value = as.numeric(dat[[column]]))
  }
  data.table::setcolorder(
    dat,
    c("code_station", "timestamp_utc", inmet_variables)
  )
  return(dat)
}
