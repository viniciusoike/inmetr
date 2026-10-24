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

testthat::test_that("municipalities use polygons or documented exceptions", {
  square <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  municipalities <- sf::st_sf(
    code_muni = 2605459L,
    name_muni = "Fernando de Noronha",
    abbrev_state = "PE",
    name_region = "Nordeste",
    geometry = sf::st_sfc(square, crs = 4674)
  )
  stations <- data.table::data.table(
    code_station = c("A001", "A302", "C891"),
    name_station = c("Inside", "Archipelago", "Criosfera"),
    lat = c(0.5, 1.1, 2),
    lon = c(0.5, 0.5, 2)
  )
  report_dir <- tempfile("spatial-")
  dir.create(report_dir)
  on.exit(unlink(report_dir, recursive = TRUE), add = TRUE)

  actual <- assign_station_municipalities(stations, municipalities, report_dir)
  testthat::expect_equal(actual$code_muni, c(2605459L, 2605459L, NA_integer_))
  testthat::expect_equal(actual$muni_match, c("polygon", "manual", "none"))
  testthat::expect_true(all(is.na(actual[
    code_station == "C891",
    c("name_muni", "abbrev_state", "name_region")
  ])))
  report <- data.table::fread(file.path(
    report_dir,
    "nearest-municipalities.csv"
  ))
  testthat::expect_setequal(report$code_station, c("A302", "C891"))
  testthat::expect_true(all(report$distance_km > 0))

  unknown <- data.table::data.table(
    code_station = "A999",
    name_station = "Nearby",
    lat = 1.001,
    lon = 0.5
  )
  testthat::expect_error(
    assign_station_municipalities(unknown, municipalities, report_dir),
    "A999.*Nearby.*1.001.*0.5.*Fernando de Noronha.*add.*exception table"
  )
  testthat::expect_equal(
    station_offset(NA_character_, -79.49417, -84, NA_character_, FALSE),
    -3L
  )
})
