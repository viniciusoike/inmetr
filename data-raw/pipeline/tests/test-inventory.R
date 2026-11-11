source(here::here("data-raw", "pipeline", "inventory.R"))

testthat::test_that("official inputs supersede legacy year directories", {
  root <- tempfile("inventory-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dirs <- c(
    "zipped_files/inmet_2024",
    "zipped_files/inmet_2025",
    "official/inmet_2025",
    "official/inmet_2026"
  )
  for (dir in dirs) {
    dir.create(file.path(root, dir), recursive = TRUE)
  }
  writeLines("legacy 2024", file.path(root, dirs[[1]], "a.CSV"))
  writeLines("legacy 2025", file.path(root, dirs[[2]], "a.CSV"))
  writeLines("official 2025", file.path(root, dirs[[3]], "a.CSV"))
  writeLines("official 2026", file.path(root, dirs[[4]], "a.CSV"))
  writeLines("sidecar", file.path(root, dirs[[3]], "._a.CSV"))

  inventory <- discover_input_files(root)
  testthat::expect_equal(inventory$files$year, c(2024L, 2025L, 2026L))
  testthat::expect_equal(
    inventory$files$source,
    c("legacy", "official", "official")
  )
  testthat::expect_equal(
    basename(dirname(inventory$files$path)),
    c("inmet_2024", "inmet_2025", "inmet_2026")
  )
  testthat::expect_equal(inventory$superseded$year, 2025L)
  testthat::expect_equal(inventory$superseded$n_files, 1L)
  testthat::expect_equal(
    basename(inventory$superseded$path),
    "inmet_2025"
  )
})

testthat::test_that("inventory rejects empty and outlier years", {
  root <- tempfile("inventory-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_1999"), recursive = TRUE)
  writeLines("old", file.path(root, "zipped_files", "inmet_1999", "a.CSV"))
  testthat::expect_error(discover_input_files(root), "year")
  unlink(file.path(root, "zipped_files", "inmet_1999"), recursive = TRUE)
  testthat::expect_error(discover_input_files(root), "No INMET")
})
