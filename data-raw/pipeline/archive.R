# Archive and provenance verification ----

# Shared by acquisition and the build inventory. An official year counts only
# when its provenance, cached archive, extracted CSVs and inventory evidence
# agree byte for byte. Nothing here downloads or extracts into the data tree.

`%||%` <- function(x, y) if (is.null(x)) y else x

inmet_csv_name <- function(path) {
  return(
    grepl("\\.[Cc][Ss][Vv]$", path) &
      !grepl("(^|/)\\._", path)
  )
}

scan_csv_files <- function(path) {
  files <- list.files(
    path,
    pattern = "\\.[Cc][Ss][Vv]$",
    recursive = TRUE,
    full.names = TRUE
  )
  return(sort(files[inmet_csv_name(files)]))
}

file_hashes <- function(path) {
  files <- scan_csv_files(path)
  if (!length(files)) {
    return(data.table::data.table(
      name = character(),
      size = numeric(),
      sha256 = character()
    ))
  }
  hashes <- data.table::data.table(
    name = basename(files),
    size = unname(file.info(files)$size),
    sha256 = vapply(
      files,
      digest::digest,
      character(1),
      algo = "sha256",
      file = TRUE
    )
  )
  data.table::setorder(hashes, name)
  return(hashes)
}

read_provenance <- function(official_dir) {
  path <- file.path(official_dir, "provenance.json")
  if (!file.exists(path)) {
    return(list())
  }
  provenance <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      cli::cli_abort(
        "Could not read {.file {path}}: {conditionMessage(error)}."
      )
    }
  )
  if (!is.list(provenance)) {
    cli::cli_abort("{.file {path}} must contain a JSON object keyed by year.")
  }
  return(provenance)
}

write_provenance <- function(provenance, official_dir) {
  path <- file.path(official_dir, "provenance.json")
  staging <- paste0(path, ".staging")
  on.exit(unlink(staging), add = TRUE)
  jsonlite::write_json(
    provenance,
    staging,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  if (!file.rename(staging, path)) {
    cli::cli_abort("Could not replace {.file {path}}.")
  }
  return(invisible(path))
}

write_inventory <- function(files, path) {
  staging <- paste0(path, ".staging")
  on.exit(unlink(staging), add = TRUE)
  data.table::fwrite(files, staging)
  if (!file.rename(staging, path)) {
    cli::cli_abort("Could not replace {.file {path}}.")
  }
  return(invisible(path))
}

relative_path <- function(root, path) {
  prefix <- paste0(normalizePath(root, winslash = "/"), "/")
  return(sub(
    paste0("^", prefix),
    "",
    normalizePath(path, winslash = "/")
  ))
}

archive_listing <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Archive {.file {path}} is missing.")
  }
  listing <- utils::unzip(path, list = TRUE)
  csv <- listing[inmet_csv_name(listing$Name), , drop = FALSE]
  if (!nrow(csv)) {
    cli::cli_abort("No CSV files in {.file {path}}.")
  }
  names <- basename(csv$Name)
  if (anyDuplicated(names)) {
    cli::cli_abort("Duplicate CSV names in {.file {path}}.")
  }
  start <- as.Date(
    sub(
      ".*_([0-9]{2}-[0-9]{2}-[0-9]{4})_A_.*",
      "\\1",
      names
    ),
    format = "%d-%m-%Y"
  )
  end <- as.Date(
    sub(
      ".*_A_([0-9]{2}-[0-9]{2}-[0-9]{4})\\.[Cc][Ss][Vv]$",
      "\\1",
      names
    ),
    format = "%d-%m-%Y"
  )
  return(list(
    names = names,
    sizes = as.numeric(csv$Length),
    declared_start = if (all(is.na(start))) {
      NULL
    } else {
      as.character(min(start, na.rm = TRUE))
    },
    declared_end = if (all(is.na(end))) {
      NULL
    } else {
      as.character(max(end, na.rm = TRUE))
    }
  ))
}

archive_members <- function(path, listing = archive_listing(path)) {
  unpacked <- tempfile("inmet-members-")
  dir.create(unpacked)
  on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = unpacked)
  members <- file_hashes(unpacked)
  if (
    nrow(members) != length(listing$names) ||
      !setequal(members$name, listing$names)
  ) {
    cli::cli_abort(
      "Archive {.file {path}} changed while its members were extracted."
    )
  }
  sizes <- stats::setNames(listing$sizes, listing$names)
  if (!all(members$size == sizes[members$name])) {
    cli::cli_abort(
      "Archive {.file {path}} member sizes do not match its listing."
    )
  }
  return(members)
}

verify_archive <- function(path, entry) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "Cached archive {.file {path}} is missing; run acquisition with {.code --refresh}."
    )
  }
  bytes <- unname(file.info(path)$size)
  if (!identical(as.numeric(entry$archive_bytes), bytes)) {
    cli::cli_abort(
      "Cached archive {.file {path}} has {bytes} bytes but provenance records {entry$archive_bytes}."
    )
  }
  sha256 <- digest::digest(file = path, algo = "sha256")
  if (!identical(entry$archive_sha256, sha256)) {
    cli::cli_abort(
      "Cached archive {.file {path}} does not match its recorded SHA-256; run {.code --refresh}."
    )
  }
  return(invisible(sha256))
}

verify_extraction <- function(path, expected, archive) {
  if (!dir.exists(path)) {
    cli::cli_abort("Extracted archive {.file {path}} is missing.")
  }
  files <- scan_csv_files(path)
  actual <- data.table::data.table(
    name = basename(files),
    size = unname(file.info(files)$size),
    sha256 = vapply(
      files,
      digest::digest,
      character(1),
      algo = "sha256",
      file = TRUE
    ),
    path = files
  )
  data.table::setorder(actual, name)
  if (nrow(actual) != nrow(expected) || !setequal(actual$name, expected$name)) {
    missing <- setdiff(expected$name, actual$name)
    unexpected <- setdiff(actual$name, expected$name)
    cli::cli_abort(c(
      "Extracted files in {.file {path}} do not match the verified archive {.file {archive}}.",
      "*" = if (length(missing)) {
        "Missing: {.file {missing}}."
      },
      "*" = if (length(unexpected)) {
        "Unexpected: {.file {unexpected}}."
      },
      "i" = "Move the directory aside or run {.code Rscript data-raw/acquire.R --refresh}."
    ))
  }
  compared <- merge(
    expected[, .(name, expected_size = size, expected_sha256 = sha256)],
    actual,
    by = "name"
  )
  changed <- compared[
    expected_size != size | expected_sha256 != sha256
  ]
  if (nrow(changed)) {
    cli::cli_abort(c(
      "Extracted CSV bytes in {.file {path}} do not match the verified archive {.file {archive}}.",
      "*" = "{nrow(changed)} extracted file(s) differ, starting with {.file {changed$name[[1]]}}.",
      "i" = "Nothing was overwritten. Restore the archive or run {.code Rscript data-raw/acquire.R --refresh}."
    ))
  }
  data.table::setcolorder(actual, c("name", "size", "sha256", "path"))
  return(actual)
}

verify_official_year <- function(data_root, year, provenance) {
  official_dir <- file.path(data_root, "official")
  target <- file.path(official_dir, sprintf("inmet_%d", year))
  entry <- provenance[[as.character(year)]]
  if (is.null(entry)) {
    cli::cli_abort(c(
      "Official input {.val {year}} has no provenance entry.",
      "i" = "Run {.code Rscript data-raw/acquire.R {year}} to verify or refresh it."
    ))
  }
  required <- c(
    "year",
    "archive_path",
    "archive_bytes",
    "archive_sha256",
    "inventory_path",
    "inventory_sha256",
    "file_count",
    "retrieved_at_utc"
  )
  absent <- setdiff(required, names(entry))
  if (length(absent)) {
    cli::cli_abort(c(
      "Provenance for official input {.val {year}} is incomplete.",
      "*" = "Missing: {.field {absent}}.",
      "i" = "Run {.code Rscript data-raw/acquire.R {year}} to rebuild the evidence."
    ))
  }
  if (!identical(as.integer(entry$year), as.integer(year))) {
    cli::cli_abort("Provenance entry {.val {year}} records another year.")
  }
  archive <- file.path(data_root, entry$archive_path)
  verify_archive(archive, entry)
  inventory_path <- file.path(data_root, entry$inventory_path)
  if (!file.exists(inventory_path)) {
    cli::cli_abort(
      "Inventory evidence {.file {inventory_path}} for {.val {year}} is missing."
    )
  }
  inventory_sha256 <- digest::digest(file = inventory_path, algo = "sha256")
  if (!identical(inventory_sha256, entry$inventory_sha256)) {
    cli::cli_abort(
      "Inventory evidence {.file {inventory_path}} does not match its provenance SHA-256."
    )
  }
  inventory <- data.table::fread(inventory_path)
  expected_columns <- c("name", "size", "sha256")
  if (!all(expected_columns %in% names(inventory))) {
    cli::cli_abort(
      "Inventory evidence {.file {inventory_path}} lacks {.field {expected_columns}}."
    )
  }
  if (!nrow(inventory)) {
    cli::cli_abort("Inventory evidence {.file {inventory_path}} is empty.")
  }
  if (anyDuplicated(inventory$name)) {
    cli::cli_abort(
      "Inventory evidence {.file {inventory_path}} repeats file names."
    )
  }
  if (!identical(as.integer(entry$file_count), nrow(inventory))) {
    cli::cli_abort(
      "Inventory evidence {.file {inventory_path}} has {nrow(inventory)} files but provenance records {entry$file_count}."
    )
  }
  listing <- archive_listing(archive)
  if (!setequal(inventory$name, listing$names)) {
    cli::cli_abort(
      "Inventory evidence {.file {inventory_path}} does not match the members of {.file {archive}}."
    )
  }
  sizes <- stats::setNames(listing$sizes, listing$names)
  if (!all(inventory$size == sizes[inventory$name])) {
    cli::cli_abort(
      "Inventory sizes in {.file {inventory_path}} do not match {.file {archive}}."
    )
  }
  actual <- verify_extraction(target, inventory, archive)
  return(actual$path)
}
