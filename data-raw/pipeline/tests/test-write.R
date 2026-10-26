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
  testthat::expect_equal(manifest$schema_version, 2L)
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
