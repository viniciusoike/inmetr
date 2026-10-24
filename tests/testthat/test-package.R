test_that("cache downloads once and validates files", {
  assets <- local_fixture()
  dat <- read_inmet("daily", 2024)
  expect_s3_class(dat, "tbl_dbi")
  expect_equal(dplyr::collect(dat)$temp_mean_c, 21)
  expect_type(dplyr::collect(dat)$year, "integer")
  expect_type(dplyr::collect(dat)$code_muni, "integer")
  unlink(file.path(assets, "daily-2024.parquet"))
  expect_equal(dplyr::collect(read_inmet("daily", 2024))$temp_mean_c, 21)
  expect_error(read_inmet("daily", 2022), "2023.*2024")
  expect_error(read_inmet("daily", 1e12), "2023.*2024")
})

test_that("file URLs work offline and damaged cached files are replaced", {
  assets <- local_fixture()
  options(inmetr.base_url = paste0("file://", assets))
  expect_equal(nrow(inmet_stations()), 3L)
  path <- file.path(inmet_cache_dir(), "2026-10-24", "stations.parquet")
  writeBin(charToRaw("bad"), path)
  expect_equal(nrow(inmet_stations()), 3L)
  expect_equal(
    file.info(path)$size,
    file.info(file.path(assets, "stations.parquet"))$size
  )
})

test_that("hash mismatch deletes the downloaded file", {
  assets <- local_fixture()
  writeBin(charToRaw("bad"), file.path(assets, "daily-2024.parquet"))
  expect_error(read_inmet("daily", 2024), "SHA-256")
  expect_false(file.exists(file.path(
    inmet_cache_dir(),
    "2026-10-24",
    "daily",
    "year=2024",
    "part-0.parquet"
  )))
})

test_that("downloads publish only validated files", {
  assets <- local_fixture()
  manifest_path <- file.path(inmet_cache_dir(), "2026-10-24", "manifest.json")
  writeLines("{incomplete", file.path(assets, "manifest.json"))
  expect_error(inmet_stations(), "Invalid.*manifest.json")
  expect_false(file.exists(manifest_path))
  expect_length(
    list.files(
      dirname(manifest_path),
      pattern = "^manifest.json-",
      all.files = TRUE
    ),
    0L
  )
})

test_that("download failures leave no partial cache file", {
  assets <- local_fixture()
  unlink(file.path(assets, "stations.parquet"))
  expect_error(inmet_stations(), "Could not copy")
  cache <- file.path(inmet_cache_dir(), "2026-10-24")
  expect_false(file.exists(file.path(cache, "stations.parquet")))
  expect_length(
    list.files(cache, pattern = "^stations.parquet-", recursive = TRUE),
    0L
  )
})

test_that("an interrupted copy removes its temporary file", {
  local_fixture()
  testthat::local_mocked_bindings(
    file.copy = function(from, to, ...) {
      writeBin(charToRaw("partial"), to)
      return(FALSE)
    },
    .package = "base"
  )
  error <- tryCatch(inmet_stations(), error = identity)
  expect_match(conditionMessage(error), "Could not copy")
  cache <- file.path(inmet_cache_dir(), "2026-10-24")
  expect_length(
    list.files(cache, pattern = "^manifest.json-", all.files = TRUE),
    0L
  )
  expect_false(file.exists(file.path(cache, "manifest.json")))
})

test_that("download errors keep the caller and restore the timeout", {
  withr::local_options(inmetr.base_url = "file:///definitely/not/inmetr")
  timeout <- getOption("timeout")
  invoke <- function() {
    return(inmetr:::.fetch_asset("manifest.json", tempfile()))
  }
  error <- tryCatch(invoke(), error = identity)
  expect_match(conditionMessage(error), "Could not download")
  expect_identical(conditionCall(error), quote(invoke()))
  expect_identical(getOption("timeout"), timeout)
})

test_that("downloads raise the timeout without lowering an existing one", {
  assets <- local_fixture()
  options(inmetr.base_url = "https://example.invalid/inmetr")
  for (timeout in c(60, 7200)) {
    withr::local_options(timeout = timeout)
    testthat::local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        expect_gte(getOption("timeout"), 3600)
        expect_equal(getOption("timeout"), max(3600, timeout))
        file.copy(file.path(assets, basename(url)), destfile)
        return(0L)
      },
      .package = "utils"
    )
    inmetr:::.fetch_asset("manifest.json", tempfile())
    expect_equal(getOption("timeout"), timeout)
  }
})

test_that("manifest entries require valid path, size, and SHA-256", {
  assets <- local_fixture()
  manifest_path <- file.path(assets, "manifest.json")
  original <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  cases <- list(
    missing_hash = function(entry) {
      entry$sha256 <- NULL
      return(entry)
    },
    bad_hash = function(entry) {
      entry$sha256 <- "bad"
      return(entry)
    },
    bad_size = function(entry) {
      entry$size <- -1
      return(entry)
    }
  )
  for (change in cases) {
    manifest <- original
    entry <- manifest$tables$stations$files[[1]]
    entry <- change(entry)
    manifest$tables$stations$files[[1]] <- entry
    jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
    unlink(file.path(inmet_cache_dir(), "2026-10-24"), recursive = TRUE)
    error <- tryCatch(inmet_stations(), error = identity)
    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), "manifest")
    expect_identical(conditionCall(error), quote(inmet_stations()))
    expect_false(file.exists(file.path(
      inmet_cache_dir(),
      "2026-10-24",
      "stations.parquet"
    )))
  }
})

test_that("manifest errors report exported readers", {
  assets <- local_fixture()
  writeLines("{incomplete", file.path(assets, "manifest.json"))
  error <- tryCatch(inmet_stations(), error = identity)
  expect_identical(conditionCall(error), quote(inmet_stations()))
  error <- tryCatch(read_inmet("daily", 2024), error = identity)
  expect_identical(conditionCall(error), quote(read_inmet("daily", 2024)))
})

test_that("cache option errors report exported readers", {
  local_fixture()
  options(inmetr.cache_dir = "")
  error <- tryCatch(inmet_stations(), error = identity)
  expect_identical(conditionCall(error), quote(inmet_stations()))
  error <- tryCatch(read_inmet("daily", 2024), error = identity)
  expect_identical(conditionCall(error), quote(read_inmet("daily", 2024)))
})

test_that("a corrupt cached manifest is downloaded again once", {
  local_fixture()
  manifest_path <- file.path(inmet_cache_dir(), "2026-10-24", "manifest.json")
  dir.create(dirname(manifest_path), recursive = TRUE)
  writeLines("{incomplete", manifest_path)
  expect_equal(nrow(inmet_stations()), 3L)
  expect_silent(jsonlite::fromJSON(manifest_path))
})

test_that("cached data use size checks and size mismatches download again", {
  assets <- local_fixture()
  inmet_stations()
  path <- file.path(inmet_cache_dir(), "2026-10-24", "stations.parquet")
  entry <- jsonlite::fromJSON(
    file.path(assets, "manifest.json"),
    simplifyVector = FALSE
  )$tables$stations$files[[1]]
  original <- readBin(path, "raw", n = file.info(path)$size)
  changed <- original
  changed[[1]] <- as.raw(bitwXor(as.integer(changed[[1]]), 1L))
  writeBin(changed, path)
  expect_identical(inmetr:::.ensure_file(entry), path)
  expect_identical(readBin(path, "raw", n = 1L), changed[[1]])
  writeBin(charToRaw("bad"), path)
  expect_identical(inmetr:::.ensure_file(entry), path)
  expect_identical(readBin(path, "raw", n = 1L), original[[1]])
})

test_that("manifest paths are checked before cache files are touched", {
  local_fixture()
  outside <- tempfile("outside-")
  writeLines("keep", outside)
  entry <- list(path = "../../outside", size = 1, sha256 = "bad")
  expect_error(inmetr:::.ensure_file(entry), "Invalid manifest file path")
  expect_identical(readLines(outside), "keep")
  expect_error(
    inmetr:::.ensure_file(list(path = "hourly/year=2024/part-evil.parquet")),
    "Invalid manifest file path"
  )
  assets <- getOption("inmetr.base_url")
  manifest <- jsonlite::fromJSON(
    file.path(assets, "manifest.json"),
    simplifyVector = FALSE
  )
  manifest$tables$hourly$files[[1]]$path <- "../../outside"
  jsonlite::write_json(
    manifest,
    file.path(assets, "manifest.json"),
    auto_unbox = TRUE
  )
  expect_error(read_inmet("daily", 2024), "Invalid manifest file path")
})

test_that("DuckDB extension and secret storage stay outside the user home", {
  con <- inmetr:::.get_connection()
  settings <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('extension_directory') AS extension_directory, current_setting('secret_directory') AS secret_directory"
  )
  home <- normalizePath(path.expand("~"), winslash = "/")
  expect_false(startsWith(settings$extension_directory, paste0(home, "/")))
  expect_false(startsWith(settings$secret_directory, paste0(home, "/")))
})

test_that("lazy filtering and stations work offline", {
  local_fixture()
  expect_message(read_inmet("daily"), "MB")
  dat <- read_inmet("hourly", c(2023, 2024))
  result <- dat |>
    dplyr::filter(.data$year == 2024L, .data$code_muni == 3550308L) |>
    dplyr::collect()
  expect_equal(nrow(result), 1L)
  expect_equal(result$temp_dry_bulb_c, 21)
  stations <- inmet_stations()
  expect_s3_class(stations, "tbl_df")
  expect_equal(nrow(stations), 3L)
})

test_that("nearest accepts code and normalized names", {
  local_fixture()
  by_code <- inmet_nearest(3509502, n = 2)
  expect_equal(by_code$code_station, c("A701", "A702"))
  expect_equal(inmet_nearest("campinas", n = 1)$code_station, "A701")
  expect_equal(inmet_nearest("SAO PAULO", n = 1)$code_station, "A702")
  expect_equal(inmet_nearest("SÃO PAULO", n = 1)$code_station, "A702")
  expect_equal(inmet_nearest("Sa\u0303o Paulo", n = 1)$code_station, "A702")
  withr::local_locale(LC_CTYPE = "C")
  expect_equal(inmet_nearest("SÃO PAULO", n = 1)$code_station, "A702")
  expect_equal(nrow(inmet_nearest("Campinas", max_km = 1)), 1L)
  expect_equal(nrow(inmet_nearest("Campinas", max_km = 0.001)), 0L)
})

test_that("ambiguous names need a state", {
  local_fixture()
  expect_error(inmet_nearest("Bom Jesus"), "state.*code|code.*state")
  expect_s3_class(inmet_nearest("Bom Jesus", state = "RS"), "tbl_df")
  expect_error(inmet_nearest("Not A City"), "Unknown")
})

test_that("cache option and clear use the selected cache", {
  local_fixture()
  expect_equal(inmet_cache_dir(), getOption("inmetr.cache_dir"))
  inmet_stations()
  expect_true(file.exists(file.path(
    inmet_cache_dir(),
    "2026-10-24",
    "stations.parquet"
  )))
  inmet_cache_clear(ask = FALSE)
  expect_false(dir.exists(file.path(inmet_cache_dir(), "2026-10-24")))
})

test_that("cache clear defaults to asking in interactive sessions", {
  expect_identical(formals(inmet_cache_clear)$ask, quote(interactive()))
})

test_that("cache clear keeps files unless the answer is yes", {
  local_fixture()
  inmet_stations()
  path <- file.path(inmet_cache_dir(), "2026-10-24")
  testthat::local_mocked_bindings(
    .read_cache_confirmation = function() "no"
  )
  expect_false(inmet_cache_clear(ask = TRUE))
  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "stations.parquet")))
})

test_that("cache clear deletes files after a yes answer", {
  local_fixture()
  inmet_stations()
  path <- file.path(inmet_cache_dir(), "2026-10-24")
  testthat::local_mocked_bindings(
    .read_cache_confirmation = function() "yes"
  )
  expect_true(inmet_cache_clear(ask = TRUE))
  expect_false(dir.exists(path))
})

test_that("cache clear rejects invalid ask values before deleting files", {
  local_fixture()
  inmet_stations()
  path <- file.path(inmet_cache_dir(), "2026-10-24")
  for (value in list(NA, NULL, c(TRUE, FALSE), "yes")) {
    error <- tryCatch(inmet_cache_clear(ask = value), error = identity)
    expect_s3_class(error, "rlang_error")
    expect_match(conditionMessage(error), "`ask`")
    expect_identical(
      conditionCall(error),
      quote(inmet_cache_clear(ask = value))
    )
    expect_true(dir.exists(path))
  }
})
