test_that("published release downloads and queries end to end", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("INMETR_LIVE_TESTS"), "true"),
    "Set INMETR_LIVE_TESTS=true to run live release tests."
  )
  skip_if_offline("github.com")
  withr::local_options(
    inmetr.base_url = NULL,
    inmetr.cache_dir = withr::local_tempdir()
  )

  stations <- inmet_stations()
  expect_gt(nrow(stations), 0L)
  expect_true(all(c("code_station", "lat", "lon") %in% names(stations)))

  daily <- dplyr::collect(read_inmet("daily", years = 2000))
  expect_gt(nrow(daily), 0L)
  expect_true(all(daily$year == 2000L))
  expect_true(all(daily$code_station %in% stations$code_station))
})
