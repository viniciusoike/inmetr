source(here::here("data-raw", "pipeline", "parse.R"))

testthat::test_that("both published formats parse and preserve missing values", {
  files <- here::here(
    "data-raw",
    "pipeline",
    "tests",
    "fixtures",
    c("old.CSV", "new.CSV", "alternate.CSV")
  )
  old <- read_inmet_csv(files[[1]])
  new <- read_inmet_csv(files[[2]])
  alternate <- read_inmet_csv(files[[3]])
  testthat::expect_equal(old$code_station, "A001")
  testthat::expect_equal(
    format(old$timestamp_utc, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2001-01-01 00:00:00"
  )
  testthat::expect_equal(
    format(new$timestamp_utc, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2020-01-01 00:00:00"
  )
  testthat::expect_equal(old$precip_mm, 0.8)
  testthat::expect_true(is.na(new$radiation_kj_m2))
  testthat::expect_equal(old$wind_speed_ms, 2.8)
  testthat::expect_equal(alternate$code_station, "A001")
})

testthat::test_that("invalid measurement tokens identify their source", {
  file <- here::here("data-raw", "pipeline", "tests", "fixtures", "new.CSV")
  lines <- readLines(file, encoding = "bytes")
  lines[[10]] <- sub(";20;", ";bad-token;", lines[[10]], fixed = TRUE)
  temp <- tempfile(fileext = ".CSV")
  writeLines(lines, temp, useBytes = TRUE)
  testthat::expect_error(
    read_inmet_csv(temp),
    paste0(basename(temp), ".*temp_dry_bulb_c.*bad-token")
  )
})

testthat::test_that("unknown headers fail before data are imported", {
  file <- here::here("data-raw", "pipeline", "tests", "fixtures", "new.CSV")
  lines <- iconv(
    readLines(file, encoding = "bytes"),
    from = "ISO-8859-1",
    to = "UTF-8"
  )
  lines[[9]] <- sub("Hora UTC", "Bad hour", lines[[9]], fixed = TRUE)
  temp <- tempfile(fileext = ".CSV")
  writeLines(lines, temp, useBytes = TRUE)
  testthat::expect_error(read_inmet_csv(temp), "Unknown INMET header")
})

testthat::test_that("published F altitude is missing, not a parse warning", {
  testthat::expect_true(is.na(parse_metadata_number("F", allow_missing = TRUE)))
  testthat::expect_error(
    parse_metadata_number("F"),
    "Invalid station coordinate"
  )
})
