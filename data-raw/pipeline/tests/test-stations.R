source(here::here("data-raw", "pipeline", "stations.R"))

testthat::test_that("standard offsets keep mainland and offshore stations separate", {
  actual <- station_offset(
    c("PE", "PE", "AM", "AM", "AC", "PA"),
    c(-34.9, -32.4, -69.94, -60.0, -67.8, -54.7),
    c(-8.1, -3.8, -4.23, -3.1, -9.9, -2.4),
    c(
      "Recife",
      "Fernando de Noronha",
      "Tabatinga",
      "Manaus",
      "Rio Branco",
      "Santarém"
    ),
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  testthat::expect_equal(actual, c(-3L, -2L, -5L, -4L, -5L, -3L))
})
