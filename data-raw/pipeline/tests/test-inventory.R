source(here::here("data-raw", "pipeline", "archive.R"))
source(here::here("data-raw", "pipeline", "inventory.R"))
source(here::here(
  "data-raw",
  "pipeline",
  "tests",
  "helper-official.R"
))

testthat::test_that("verified official inputs supersede legacy year directories", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2024"), recursive = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2024",
    file.path(root, "zipped_files", "inmet_2024", "a.CSV")
  )
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  add_official_year(root, 2025L, c("a.CSV" = "a\n", "b.csv" = "b\n"))
  official_2026 <- add_official_year(root, 2026L, c("a.CSV" = "c\n"))
  writeLines("sidecar", file.path(official_2026$target, "._a.CSV"))

  inventory <- discover_input_files(root)
  testthat::expect_equal(inventory$files$year, c(2024L, 2025L, 2025L, 2026L))
  testthat::expect_equal(
    inventory$files$source,
    c("legacy", "official", "official", "official")
  )
  testthat::expect_equal(
    basename(dirname(inventory$files$path)),
    c("inmet_2024", "inmet_2025", "inmet_2025", "inmet_2026")
  )
  testthat::expect_equal(inventory$superseded$year, 2025L)
  testthat::expect_equal(inventory$superseded$n_files, 1L)
  testthat::expect_equal(
    basename(inventory$superseded$path),
    "inmet_2025"
  )
})

testthat::test_that("inventory rejects an empty official year over legacy data", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  dir.create(file.path(root, "official", "inmet_2025"), recursive = TRUE)
  testthat::expect_error(discover_input_files(root), "no CSV files")
})

testthat::test_that("inventory requires provenance for official inputs", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  fixture <- add_official_year(root, 2025L, c("a.CSV" = "a\n"))
  unlink(file.path(root, "official", "provenance.json"))
  testthat::expect_error(discover_input_files(root), "provenance")
})

testthat::test_that("inventory rejects a partial official year", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  fixture <- add_official_year(root, 2025L, c("a.CSV" = "a\n", "b.CSV" = "b\n"))
  unlink(file.path(fixture$target, "b.CSV"))
  testthat::expect_error(discover_input_files(root), "verified archive")
})

testthat::test_that("inventory rejects mutated official content", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  fixture <- add_official_year(root, 2025L, c("a.CSV" = "a\n"))
  tamper_file(file.path(fixture$target, "a.CSV"))
  testthat::expect_error(discover_input_files(root), "verified archive")
})

testthat::test_that("inventory rejects unexpected official files", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  fixture <- add_official_year(root, 2025L, c("a.CSV" = "a\n"))
  writeLines("extra", file.path(fixture$target, "extra.CSV"))
  testthat::expect_error(discover_input_files(root), "verified archive")
})

testthat::test_that("inventory rejects missing or altered evidence", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  fixture <- add_official_year(root, 2025L, c("a.CSV" = "a\n"))
  unlink(fixture$archive)
  testthat::expect_error(discover_input_files(root), "archive")
  restore <- add_official_year(root, 2025L, c("a.CSV" = "a\n"))
  unlink(restore$inventory)
  testthat::expect_error(discover_input_files(root), "Inventory")
  add_official_year(root, 2025L, c("a.CSV" = "a\n"))
  unlink(restore$target, recursive = TRUE)
  testthat::expect_error(discover_input_files(root), "extracted directory")
})

testthat::test_that("legacy-only inputs remain supported", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_2025"), recursive = TRUE)
  writeLines(
    "legacy 2025",
    file.path(root, "zipped_files", "inmet_2025", "a.CSV")
  )
  inventory <- discover_input_files(root)
  testthat::expect_equal(inventory$files$source, "legacy")
  testthat::expect_equal(nrow(inventory$superseded), 0L)
})

testthat::test_that("inventory rejects empty and outlier years", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "zipped_files", "inmet_1999"), recursive = TRUE)
  writeLines("old", file.path(root, "zipped_files", "inmet_1999", "a.CSV"))
  testthat::expect_error(discover_input_files(root), "year")
  unlink(file.path(root, "zipped_files", "inmet_1999"), recursive = TRUE)
  testthat::expect_error(discover_input_files(root), "No INMET")
})
