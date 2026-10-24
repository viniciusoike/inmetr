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

test_that("file URLs and cached hashes work offline", {
  assets <- local_fixture()
  options(inmetr.base_url = paste0("file://", assets))
  expect_equal(nrow(inmet_stations()), 3L)
  path <- file.path(inmet_cache_dir(), "2026-10-24", "stations.parquet")
  writeBin(charToRaw("bad"), path)
  expect_error(inmet_stations(), "SHA-256")
  expect_false(file.exists(path))
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
  inmet_cache_clear()
  expect_false(dir.exists(file.path(inmet_cache_dir(), "2026-10-24")))
})
