source(here::here("data-raw", "pipeline", "parse.R"))
source(here::here("data-raw", "pipeline", "archive.R"))
source(here::here("data-raw", "pipeline", "acquire.R"))
source(here::here(
  "data-raw",
  "pipeline",
  "tests",
  "helper-official.R"
))

testthat::test_that("cached archives reuse verified bytes without the network", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- add_official_year(
    root,
    2025L,
    c("a.CSV" = "station a\n", "b.csv" = "station b\n")
  )
  inventory_before <- file_bytes(fixture$inventory)
  transport <- refusing_transport()

  result <- acquire_year(2025L, root, transport = transport)

  testthat::expect_equal(transport$calls(), 0L)
  testthat::expect_equal(file_bytes(fixture$inventory), inventory_before)
  testthat::expect_equal(
    result$entry$retrieved_at_utc,
    fixture$entry$retrieved_at_utc
  )
  testthat::expect_equal(
    result$entry$archive_sha256,
    fixture$entry$archive_sha256
  )
})

testthat::test_that("reuse rejects tampered bytes and keeps old evidence", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- add_official_year(root, 2025L, c("a.CSV" = "station a\n"))
  provenance_path <- file.path(root, "official", "provenance.json")
  provenance_before <- file_bytes(provenance_path)
  inventory_before <- file_bytes(fixture$inventory)
  tamper_file(file.path(fixture$target, "a.CSV"))

  testthat::expect_error(
    acquire_year(2025L, root, transport = refusing_transport()),
    "verified archive"
  )
  testthat::expect_equal(file_bytes(provenance_path), provenance_before)
  testthat::expect_equal(file_bytes(fixture$inventory), inventory_before)
})

testthat::test_that("reuse rejects missing and unexpected members", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- add_official_year(
    root,
    2025L,
    c("a.CSV" = "station a\n", "b.CSV" = "station b\n")
  )
  unlink(file.path(fixture$target, "b.CSV"))
  testthat::expect_error(
    acquire_year(2025L, root, transport = refusing_transport()),
    "verified archive"
  )
  writeLines("c", file.path(fixture$target, "c.CSV"))
  testthat::expect_error(
    acquire_year(2025L, root, transport = refusing_transport()),
    "verified archive"
  )
})

testthat::test_that("reuse ignores Apple sidecars and lowercase extensions", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- add_official_year(root, 2025L, c("a.csv" = "station a\n"))
  writeLines("sidecar", file.path(fixture$target, "._a.csv"))

  result <- acquire_year(2025L, root, transport = refusing_transport())

  testthat::expect_equal(result$entry$file_count, 1L)
  testthat::expect_equal(result$entry$year, 2025L)
})

testthat::test_that("refresh keeps prior metadata when the archive is unchanged", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- add_official_year(root, 2025L, c("a.CSV" = "station a\n"))
  transport <- fixture_transport(
    fixture$archive,
    list(
      last_modified = "Wed, 03 Jan 2024 00:00:00 GMT",
      etag = "\"unexpected\""
    )
  )

  result <- acquire_year(2025L, root, refresh = TRUE, transport = transport)

  testthat::expect_equal(transport$calls(), 1L)
  testthat::expect_equal(
    result$entry$retrieved_at_utc,
    fixture$entry$retrieved_at_utc
  )
  testthat::expect_equal(
    result$entry$last_modified,
    fixture$entry$last_modified
  )
  testthat::expect_equal(result$entry$etag, fixture$entry$etag)
  testthat::expect_match(result$entry$revalidated_at_utc, "^20[0-9]{2}-")
  testthat::expect_false(dir.exists(file.path(root, "official", "history")))
  testthat::expect_equal(
    digest::digest(file = fixture$archive, algo = "sha256"),
    fixture$entry$archive_sha256
  )
})

testthat::test_that("refresh stages revised archives and preserves prior bytes", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- add_official_year(
    root,
    2025L,
    c("a.CSV" = "old a\n", "b.CSV" = "old b\n")
  )
  inventory_before <- file_bytes(fixture$inventory)
  revised <- tempfile(fileext = ".zip")
  on.exit(unlink(revised), add = TRUE)
  zip_fixture(revised, c("a.CSV" = "new a\n", "b.CSV" = "new b\n"))
  transport <- fixture_transport(
    revised,
    list(
      last_modified = "Thu, 04 Jan 2024 00:00:00 GMT",
      etag = "\"revised\"",
      content_length = as.character(file.info(revised)$size)
    )
  )

  result <- acquire_year(2025L, root, refresh = TRUE, transport = transport)

  tag <- substr(fixture$entry$archive_sha256, 1, 8)
  history <- file.path(root, "official", "history")
  old_archive <- file.path(
    history,
    "archives",
    sprintf("inmet-2025-%s.zip", tag)
  )
  old_extract <- file.path(history, "extracts", sprintf("inmet_2025-%s", tag))
  old_inventory <- file.path(
    history,
    "inventory",
    sprintf("files-2025-%s.csv", tag)
  )
  testthat::expect_true(file.exists(old_archive))
  testthat::expect_equal(
    digest::digest(file = old_archive, algo = "sha256"),
    fixture$entry$archive_sha256
  )
  testthat::expect_true(dir.exists(old_extract))
  testthat::expect_equal(readLines(file.path(old_extract, "a.CSV")), "old a")
  testthat::expect_equal(
    file_bytes(old_inventory),
    inventory_before
  )
  testthat::expect_equal(
    readLines(file.path(root, "official", "inmet_2025", "a.CSV")),
    "new a"
  )
  testthat::expect_equal(
    digest::digest(
      file = file.path(root, "cache", "inmet-2025.zip"),
      algo = "sha256"
    ),
    digest::digest(file = revised, algo = "sha256")
  )
  testthat::expect_false(
    identical(result$entry$archive_sha256, fixture$entry$archive_sha256)
  )
  previous <- result$entry$previous[[1]]
  testthat::expect_equal(previous$archive_sha256, fixture$entry$archive_sha256)
  testthat::expect_match(previous$archive_path, "official/history/archives")
  testthat::expect_true(file.exists(file.path(root, previous$archive_path)))
  testthat::expect_true(file.exists(file.path(root, previous$inventory_path)))
  testthat::expect_equal(
    result$entry$last_modified,
    "Thu, 04 Jan 2024 00:00:00 GMT"
  )
  testthat::expect_equal(result$entry$etag, "\"revised\"")
  stored <- jsonlite::fromJSON(
    file.path(root, "official", "provenance.json"),
    simplifyVector = FALSE
  )
  testthat::expect_equal(
    stored[["2025"]]$previous[[1]]$archive_sha256,
    fixture$entry$archive_sha256
  )
  testthat::expect_equal(
    stored[["2025"]]$archive_bytes,
    file.info(revised)$size
  )
})

testthat::test_that("fresh downloads record metadata from the same response", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  source <- tempfile(fileext = ".zip")
  on.exit(unlink(source), add = TRUE)
  zip_fixture(source, c("a.CSV" = "fresh\n"))
  headers <- list(
    last_modified = "Fri, 05 Jan 2024 00:00:00 GMT",
    etag = "\"fresh\"",
    content_length = as.character(file.info(source)$size)
  )
  transport <- fixture_transport(source, headers)

  result <- acquire_year(2026L, root, transport = transport)

  testthat::expect_equal(transport$calls(), 1L)
  testthat::expect_true(file.exists(file.path(root, "cache", "inmet-2026.zip")))
  testthat::expect_equal(result$entry$last_modified, headers$last_modified)
  testthat::expect_equal(result$entry$etag, headers$etag)
  testthat::expect_equal(
    result$entry$archive_sha256,
    digest::digest(file = source, algo = "sha256")
  )
  testthat::expect_equal(result$entry$file_count, 1L)
  testthat::expect_true(file.exists(file.path(
    root,
    "official",
    "files-2026.csv"
  )))
  testthat::expect_match(result$entry$retrieved_at_utc, "^20[0-9]{2}-")
  stored <- jsonlite::fromJSON(
    file.path(root, "official", "provenance.json"),
    simplifyVector = FALSE
  )
  testthat::expect_equal(stored[["2026"]]$etag, "\"fresh\"")
})

testthat::test_that("acquisition arguments select refresh and years", {
  parsed <- parse_acquire_args(c("--refresh", "2025", "2026"), 2026L)
  testthat::expect_true(parsed$refresh)
  testthat::expect_equal(parsed$years, c(2025L, 2026L))
  testthat::expect_equal(
    parse_acquire_args(character(), 2026L)$years[[1]],
    2000L
  )
  testthat::expect_false(parse_acquire_args("2025", 2026L)$refresh)
  testthat::expect_error(parse_acquire_args("--bogus", 2026L), "option")
  testthat::expect_error(
    parse_acquire_args(c("2025", "2025"), 2026L),
    "distinct"
  )
})

testthat::test_that("response headers come from the final response block", {
  headers <- parse_response_headers(c(
    "HTTP/1.1 302 Found",
    "Location: https://example.test/2000.zip",
    "",
    "HTTP/1.1 200 OK",
    "Last-Modified: Wed, 02 Sep 2020 12:14:18 GMT",
    "ETag: \"abc\"",
    "Content-Length: 123",
    ""
  ))
  testthat::expect_equal(headers$last_modified, "Wed, 02 Sep 2020 12:14:18 GMT")
  testthat::expect_equal(headers$etag, "\"abc\"")
  testthat::expect_equal(headers$content_length, "123")
  testthat::expect_null(parse_response_headers(character())$last_modified)
})

testthat::test_that("truncated downloads abort before caching", {
  root <- local_official_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  source <- tempfile(fileext = ".zip")
  on.exit(unlink(source), add = TRUE)
  zip_fixture(source, c("a.CSV" = "fresh\n"))
  transport <- fixture_transport(
    source,
    list(content_length = as.character(file.info(source)$size + 1L))
  )
  testthat::expect_error(
    acquire_year(2026L, root, transport = transport),
    "declared"
  )
  testthat::expect_false(file.exists(file.path(
    root,
    "cache",
    "inmet-2026.zip"
  )))
  testthat::expect_false(dir.exists(file.path(root, "official", "inmet_2026")))
})
