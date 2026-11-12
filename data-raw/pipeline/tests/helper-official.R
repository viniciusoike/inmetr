# Fixtures for verified official inputs. Local files only, no network.

local_official_root <- function() {
  return(tempfile("official-root-"))
}

zip_fixture <- function(path, contents, store = FALSE) {
  if (!nzchar(Sys.which("zip"))) {
    testthat::skip("The zip command is unavailable.")
  }
  input <- tempfile("zip-input-")
  dir.create(input)
  on.exit(unlink(input, recursive = TRUE), add = TRUE)
  for (name in names(contents)) {
    writeBin(charToRaw(contents[[name]]), file.path(input, name))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  status <- utils::zip(
    zipfile = path,
    files = file.path(input, names(contents)),
    flags = if (store) "-j0qX" else "-j9qX"
  )
  if (!identical(status, 0L)) {
    testthat::stop("Could not create the ZIP fixture.")
  }
  return(path)
}

corrupt_zip_member <- function(path, content) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  target <- charToRaw(content)
  candidates <- which(bytes == target[[1L]])
  for (start in candidates) {
    end <- start + length(target) - 1L
    if (end <= length(bytes) && identical(bytes[start:end], target)) {
      bytes[[start]] <- as.raw(bitwXor(as.integer(bytes[[start]]), 1L))
      writeBin(bytes, path)
      return(invisible(path))
    }
  }
  testthat::stop("Could not locate member content in the stored ZIP fixture.")
}

evidence_snapshot <- function(root, year) {
  official <- file.path(root, "official")
  target <- file.path(official, sprintf("inmet_%d", year))
  return(list(
    cache = digest::digest(
      file = file.path(root, "cache", sprintf("inmet-%d.zip", year)),
      algo = "sha256"
    ),
    extract = file_hashes(target),
    inventory = file_bytes(file.path(official, sprintf("files-%d.csv", year))),
    provenance = file_bytes(file.path(official, "provenance.json")),
    history = dir.exists(file.path(official, "history"))
  ))
}

add_official_year <- function(
  root,
  year,
  contents,
  headers = list(
    last_modified = "Mon, 01 Jan 2024 00:00:00 GMT",
    etag = "\"fixture\""
  ),
  retrieved_at = "2026-01-01T00:00:00Z"
) {
  official <- file.path(root, "official")
  dir.create(official, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(root, "cache", sprintf("inmet-%d.zip", year))
  zip_fixture(archive, contents)
  target <- file.path(official, sprintf("inmet_%d", year))
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  files <- file.path(target, names(contents))
  for (index in seq_along(contents)) {
    writeBin(charToRaw(contents[[index]]), files[[index]])
  }
  inventory_table <- data.table::data.table(
    name = names(contents),
    size = unname(file.info(files)$size),
    sha256 = vapply(
      files,
      digest::digest,
      character(1),
      algo = "sha256",
      file = TRUE
    )
  )
  data.table::setorder(inventory_table, name)
  inventory <- file.path(official, sprintf("files-%d.csv", year))
  data.table::fwrite(inventory_table, inventory)
  provenance_path <- file.path(official, "provenance.json")
  provenance <- if (file.exists(provenance_path)) {
    jsonlite::fromJSON(provenance_path, simplifyVector = FALSE)
  } else {
    list()
  }
  entry <- list(
    year = year,
    source_url = "https://portal.inmet.gov.br/dadoshistoricos",
    archive_url = sprintf(
      "https://portal.inmet.gov.br/uploads/dadoshistoricos/%d.zip",
      year
    ),
    retrieved_at_utc = retrieved_at,
    archive_path = sprintf("cache/inmet-%d.zip", year),
    archive_bytes = unname(file.info(archive)$size),
    archive_sha256 = digest::digest(file = archive, algo = "sha256"),
    last_modified = headers$last_modified,
    etag = headers$etag,
    declared_start = "2024-01-01",
    declared_end = "2024-12-31",
    file_count = nrow(inventory_table),
    inventory_path = sprintf("official/files-%d.csv", year),
    inventory_sha256 = digest::digest(file = inventory, algo = "sha256")
  )
  provenance[[as.character(year)]] <- entry
  jsonlite::write_json(
    provenance,
    provenance_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  return(list(
    entry = entry,
    archive = archive,
    target = target,
    inventory = inventory,
    files = inventory_table
  ))
}

fixture_transport <- function(source, headers = list()) {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  return(list(
    download = function(url, destination) {
      calls$n <- calls$n + 1L
      file.copy(source, destination, overwrite = TRUE)
      return(list(headers = headers))
    },
    calls = function() calls$n
  ))
}

refusing_transport <- function() {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  return(list(
    download = function(url, destination) {
      calls$n <- calls$n + 1L
      testthat::fail("Acquisition used the network in a cache-reuse test.")
    },
    calls = function() calls$n
  ))
}

file_bytes <- function(path) {
  return(readBin(path, "raw", file.info(path)$size))
}

tamper_file <- function(path) {
  bytes <- file_bytes(path)
  last <- length(bytes)
  bytes[[last]] <- as.raw(bitwXor(as.integer(bytes[[last]]), 1L))
  writeBin(bytes, path)
  return(invisible(path))
}
