source(here::here("data-raw", "pipeline", "parse.R"))
source(here::here("data-raw", "pipeline", "write.R"))
source(here::here("data-raw", "pipeline", "manifest.R"))

testthat::test_that("writer preserves local-year partitions and output contract", {
  root <- tempfile("writer-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "reports"))
  source_file <- here::here(
    "data-raw",
    "pipeline",
    "tests",
    "fixtures",
    "old.CSV"
  )
  lines <- readLines(source_file, encoding = "bytes")
  first <- strsplit(lines[[10]], ";", fixed = TRUE)[[1]]
  first[1:2] <- c("2021-01-01", "01:00")
  first[c(10, 11)] <- "-9999"
  missing <- first
  missing[[2]] <- "03:00"
  missing[3:19] <- "-9999"
  input <- file.path(root, "input.CSV")
  writeLines(
    c(
      lines[1:9],
      paste0(paste(first, collapse = ";"), ";"),
      paste0(paste(first, collapse = ";"), ";"),
      paste0(paste(missing, collapse = ";"), ";")
    ),
    input,
    useBytes = TRUE
  )

  stations <- data.table::data.table(
    code_station = "A001",
    tz_offset_hours = -3L,
    code_muni = 5300108L,
    name_muni = "Brasília",
    abbrev_state = "DF"
  )
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = file.path(root, "test.duckdb")
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  stats <- write_hourly_year(
    con,
    input,
    stations,
    2021L,
    root,
    file.path(root, "reports")
  )
  testthat::expect_equal(stats$duplicate_rows, 1)
  write_daily(con, root)
  daily_paths <- list.files(
    file.path(root, "daily"),
    "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  testthat::expect_setequal(
    basename(dirname(daily_paths)),
    c("year=2020", "year=2021")
  )
  daily <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet('%s') ORDER BY date",
      file.path(root, "daily", "year=*", "*.parquet")
    )
  )
  testthat::expect_equal(daily$year, c(2020L, 2021L))
  testthat::expect_equal(daily$temp_min_c[[1]], 20.1)
  testthat::expect_equal(daily$temp_max_c[[1]], 20.1)
  testthat::expect_true(is.na(daily$precip_total_mm[[2]]))
  testthat::expect_equal(daily$n_hours, c(1L, 0L))
  testthat::expect_equal(daily$n_hours_temp_mean[[1]], 1L)
  testthat::expect_equal(daily$n_hours_temp_max[[1]], 1L)
  testthat::expect_equal(daily$n_hours_temp_min[[1]], 1L)
  testthat::expect_equal(daily$n_hours_precip_total[[1]], 1L)
  testthat::expect_equal(daily$n_hours_temp_mean[[2]], 0L)
  testthat::expect_equal(daily$n_hours_precip_total[[2]], 0L)

  DBI::dbWriteTable(
    con,
    "stations",
    data.table::data.table(
      code_station = "A001",
      name_station = "Brasília",
      status = "operating",
      lat = -15.8,
      lon = -47.9,
      altitude_m = 1160,
      date_start_operation = as.Date("2000-05-07"),
      first_obs = as.POSIXct("2021-01-01 01:00:00", tz = "UTC"),
      last_obs = as.POSIXct("2021-01-01 03:00:00", tz = "UTC"),
      tz_offset_hours = -3L,
      code_muni = 5300108L,
      name_muni = "Brasília",
      abbrev_state = "DF",
      name_region = "CO",
      muni_match = "polygon"
    )
  )
  DBI::dbExecute(
    con,
    sprintf(
      "COPY stations TO '%s' (FORMAT parquet)",
      file.path(root, "stations.parquet")
    )
  )
  manifest <- write_manifest(con, root, "test", "test-sha")
  testthat::expect_equal(manifest$schema_version, 3L)
  for (table in names(manifest$tables)) {
    testthat::expect_equal(
      manifest$tables[[table]]$schema[[which(vapply(
        manifest$tables[[table]]$schema,
        function(column) return(column$name == "code_muni"),
        logical(1)
      ))]]$type,
      "INTEGER"
    )
  }
  for (table in names(manifest$tables)) {
    for (entry in manifest$tables[[table]]$files) {
      path <- file.path(root, entry$path)
      testthat::expect_silent(assert_parquet_schema(con, path, table))
      testthat::expect_equal(
        entry$sha256,
        digest::digest(file = path, algo = "sha256")
      )
      n <- DBI::dbGetQuery(
        con,
        sprintf("SELECT count(*) AS n FROM read_parquet('%s')", path)
      )$n
      testthat::expect_equal(entry$row_count, n)
    }
  }
  conflicting <- first
  conflicting[[3]] <- "2"
  writeLines(
    c(
      lines[1:9],
      paste0(paste(first, collapse = ";"), ";"),
      paste0(paste(conflicting, collapse = ";"), ";")
    ),
    input,
    useBytes = TRUE
  )
  testthat::expect_error(
    write_hourly_year(
      con,
      input,
      stations,
      2021L,
      root,
      file.path(root, "reports")
    ),
    "Conflicting station-hour duplicates"
  )
})

testthat::test_that("daily coverage counts each aggregate's contributors", {
  root <- tempfile("coverage-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "reports"))
  source_file <- here::here(
    "data-raw",
    "pipeline",
    "tests",
    "fixtures",
    "old.CSV"
  )
  header <- readLines(source_file, encoding = "bytes")[1:9]
  missing <- rep("-9999", 17)
  make_row <- function(date, hour, values) {
    return(paste(c(date, hour, values, ""), collapse = ";"))
  }
  rows <- character()
  # 24 hours spanning two UTC dates, all on local 2021-06-02, with only
  # precipitation measured. Zero precipitation is data, not a missing sum.
  for (hour in 3:23) {
    values <- missing
    values[[1]] <- "0"
    rows <- c(rows, make_row("2021-06-02", sprintf("%02d:00", hour), values))
  }
  for (hour in 0:2) {
    values <- missing
    values[[1]] <- "0"
    rows <- c(rows, make_row("2021-06-03", sprintf("%02d:00", hour), values))
  }
  # 24 hours with no measurement at all.
  for (hour in 3:23) {
    rows <- c(rows, make_row("2021-06-04", sprintf("%02d:00", hour), missing))
  }
  for (hour in 0:2) {
    rows <- c(rows, make_row("2021-06-05", sprintf("%02d:00", hour), missing))
  }
  # The extreme is missing in the first hour and present in the second, so
  # both contributors come from the documented fallback.
  fallback <- missing
  fallback[c(6, 14)] <- c("20", "50")
  rows <- c(rows, make_row("2021-06-06", "03:00", fallback))
  fallback[c(6, 14)] <- c("21", "60")
  fallback[c(8, 9, 12, 13)] <- c("25", "18", "70", "40")
  rows <- c(rows, make_row("2021-06-06", "04:00", fallback))
  input <- file.path(root, "input.CSV")
  writeLines(c(header, rows), input, useBytes = TRUE)

  stations <- data.table::data.table(
    code_station = "A001",
    tz_offset_hours = -3L,
    code_muni = 5300108L,
    name_muni = "Brasília",
    abbrev_state = "DF"
  )
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = file.path(root, "test.duckdb")
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  write_hourly_year(
    con,
    input,
    stations,
    2021L,
    root,
    file.path(root, "reports")
  )
  write_daily(con, root)
  daily <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet('%s') ORDER BY date",
      file.path(root, "daily", "year=*", "*.parquet")
    )
  )
  coverage <- c(
    "n_hours_temp_mean",
    "n_hours_temp_min",
    "n_hours_temp_max",
    "n_hours_temp_dew_point_mean",
    "n_hours_rel_humidity_mean",
    "n_hours_rel_humidity_min",
    "n_hours_rel_humidity_max",
    "n_hours_precip_total",
    "n_hours_radiation_total",
    "n_hours_pressure_station_mean",
    "n_hours_wind_speed_mean",
    "n_hours_wind_gust_max"
  )
  testthat::expect_true(all(coverage %in% names(daily)))
  testthat::expect_true(all(vapply(daily[coverage], is.integer, logical(1))))
  testthat::expect_equal(
    daily$date,
    as.Date(c(
      "2021-06-02",
      "2021-06-04",
      "2021-06-06"
    ))
  )
  testthat::expect_equal(daily$n_hours, c(24L, 0L, 2L))
  testthat::expect_equal(
    unlist(daily[1, coverage], use.names = FALSE),
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 24L, 0L, 0L, 0L, 0L)
  )
  testthat::expect_equal(daily$precip_total_mm[[1]], 0)
  testthat::expect_equal(
    unlist(daily[2, coverage], use.names = FALSE),
    rep(0L, length(coverage))
  )
  testthat::expect_true(is.na(daily$precip_total_mm[[2]]))
  testthat::expect_equal(daily$n_hours_temp_min[[3]], 2L)
  testthat::expect_equal(daily$n_hours_temp_max[[3]], 2L)
  testthat::expect_equal(daily$n_hours_rel_humidity_min[[3]], 2L)
  testthat::expect_equal(daily$n_hours_rel_humidity_max[[3]], 2L)
  testthat::expect_equal(daily$temp_min_c[[3]], 18)
  testthat::expect_equal(daily$temp_max_c[[3]], 25)
  testthat::expect_true(all(
    as.matrix(daily[coverage]) >= 0L &
      as.matrix(daily[coverage]) <= daily$n_hours
  ))
})
