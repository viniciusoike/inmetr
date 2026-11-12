# Official archive acquisition ----

# Network access is confined to `http_transport()`. Everything else works on
# local files, so tests drive the same code with fixture archives.

source_page <- "https://portal.inmet.gov.br/dadoshistoricos"
base_url <- "https://portal.inmet.gov.br/uploads/dadoshistoricos"
user_agent <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0 Safari/537.36"
)

parse_acquire_args <- function(args, current_year) {
  refresh <- "--refresh" %in% args
  years <- args[args != "--refresh"]
  unknown <- years[startsWith(years, "-")]
  if (length(unknown)) {
    cli::cli_abort(c(
      "Unknown acquisition option{?s} {.arg {unknown}}.",
      "i" = "Use {.code Rscript data-raw/acquire.R [--refresh] [YEARS...]}."
    ))
  }
  years <- if (length(years)) {
    suppressWarnings(as.integer(years))
  } else {
    2000:current_year
  }
  if (
    anyNA(years) ||
      any(!years %in% 2000:(current_year + 1L)) ||
      anyDuplicated(years)
  ) {
    cli::cli_abort(
      "Pass distinct years between 2000 and {current_year + 1L}."
    )
  }
  return(list(refresh = refresh, years = sort(years)))
}

parse_response_headers <- function(lines) {
  if (!length(lines)) {
    return(list())
  }
  lines <- sub("\r$", "", lines)
  blocks <- split(lines, cumsum(lines == ""))
  blocks <- Filter(function(block) any(nzchar(block)), blocks)
  if (!length(blocks)) {
    return(list())
  }
  final <- blocks[[length(blocks)]]
  value <- function(name) {
    hit <- grep(paste0("^", name, ":"), final, ignore.case = TRUE, value = TRUE)
    if (!length(hit)) {
      return(NULL)
    }
    return(trimws(sub("^[^:]+:", "", hit[[length(hit)]])))
  }
  return(list(
    last_modified = value("Last-Modified"),
    etag = value("ETag"),
    content_length = value("Content-Length")
  ))
}

http_transport <- function() {
  if (!nzchar(Sys.which("curl"))) {
    cli::cli_abort("Acquisition needs the {.command curl} command-line tool.")
  }
  return(list(
    download = function(url, destination) {
      header_path <- tempfile("inmet-headers-")
      on.exit(unlink(header_path), add = TRUE)
      output <- suppressWarnings(system2(
        "curl",
        c(
          "-fL",
          "--silent",
          "--show-error",
          "--http1.1",
          "--max-time",
          "3600",
          "--retry",
          "3",
          "--retry-delay",
          "5",
          "-A",
          shQuote(user_agent),
          "-D",
          shQuote(header_path),
          "-o",
          shQuote(destination),
          shQuote(url)
        ),
        stdout = TRUE,
        stderr = TRUE
      ))
      if (!is.null(attr(output, "status"))) {
        cli::cli_abort(
          "Could not download {.url {url}}: {paste(output, collapse = ' ')}"
        )
      }
      headers <- if (file.exists(header_path)) {
        parse_response_headers(readLines(header_path, warn = FALSE))
      } else {
        list()
      }
      return(list(headers = headers))
    }
  ))
}

acquire_url <- function(year) {
  return(sprintf("%s/%d.zip", base_url, year))
}

download_snapshot <- function(year, cache_path, transport) {
  staging <- paste0(cache_path, ".staging")
  unlink(staging)
  response <- transport$download(acquire_url(year), staging)
  if (!file.exists(staging) || file.info(staging)$size == 0) {
    cli::cli_abort("Downloaded archive for {year} is empty.")
  }
  bytes <- unname(file.info(staging)$size)
  declared <- suppressWarnings(as.numeric(response$headers$content_length))
  if (length(declared) == 1L && !is.na(declared) && declared != bytes) {
    cli::cli_abort(
      "Downloaded {year} archive has {bytes} bytes but the response declared {declared}."
    )
  }
  return(list(
    path = staging,
    bytes = bytes,
    sha256 = digest::digest(file = staging, algo = "sha256"),
    headers = response$headers
  ))
}

provenance_entry <- function(
  year,
  archive,
  archive_path,
  inventory_path,
  retrieved_at,
  headers,
  listing,
  files,
  inventory_sha256,
  previous = NULL
) {
  return(list(
    year = year,
    source_url = source_page,
    archive_url = acquire_url(year),
    retrieved_at_utc = retrieved_at,
    archive_path = archive_path,
    archive_bytes = unname(file.info(archive)$size),
    archive_sha256 = digest::digest(file = archive, algo = "sha256"),
    last_modified = headers$last_modified %||% NA_character_,
    etag = headers$etag %||% NA_character_,
    declared_start = listing$declared_start,
    declared_end = listing$declared_end,
    file_count = nrow(files),
    inventory_path = inventory_path,
    inventory_sha256 = inventory_sha256,
    previous = previous
  ))
}

assert_entry_complete <- function(entry, year) {
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
      "i" = "Run {.code Rscript data-raw/acquire.R --refresh {year}}."
    ))
  }
  return(invisible(entry))
}

# Staging ----

# Every check on downloaded bytes runs here, under hidden staging names, before
# any current archive, extraction, inventory, provenance, or audit file moves.

stage_extraction <- function(year, archive, members, official_dir) {
  staging <- file.path(official_dir, sprintf(".staging-inmet_%d", year))
  unlink(staging, recursive = TRUE)
  dir.create(staging, recursive = TRUE)
  tryCatch(
    {
      utils::unzip(archive, exdir = staging)
      verify_extraction(staging, members, archive)
    },
    error = function(error) {
      unlink(staging, recursive = TRUE)
      cli::cli_abort(conditionMessage(error))
    }
  )
  return(staging)
}

stage_inventory <- function(year, files, official_dir) {
  path <- file.path(official_dir, sprintf(".staging-files-%d.csv", year))
  tryCatch(
    data.table::fwrite(files[, .(name, size, sha256)], path),
    error = function(error) {
      unlink(path)
      cli::cli_abort(conditionMessage(error))
    }
  )
  return(path)
}

stage_provenance <- function(provenance, official_dir, year) {
  path <- file.path(official_dir, sprintf(".staging-provenance-%d.json", year))
  tryCatch(
    {
      jsonlite::write_json(
        provenance,
        path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
      )
      staged <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      if (!identical(names(staged), names(provenance))) {
        cli::cli_abort("Could not stage provenance for {.val {year}}.")
      }
    },
    error = function(error) {
      unlink(path)
      cli::cli_abort(conditionMessage(error))
    }
  )
  return(path)
}

stage_snapshot <- function(year, archive, official_dir) {
  cleanup <- character()
  complete <- FALSE
  on.exit(
    if (!complete) {
      unlink(cleanup, recursive = TRUE)
    },
    add = TRUE
  )
  listing <- archive_listing(archive)
  members <- archive_members(archive, listing)
  if (any(members$size == 0)) {
    cli::cli_abort("Archive {.file {archive}} contains an empty CSV member.")
  }
  extract <- stage_extraction(year, archive, members, official_dir)
  cleanup <- c(cleanup, extract)
  files <- file_hashes(extract)
  if (!setequal(files$name, listing$names)) {
    cli::cli_abort("Extracted CSV names do not match the archive listing.")
  }
  inventory <- stage_inventory(year, files, official_dir)
  cleanup <- c(cleanup, inventory)
  audit_dir <- file.path(official_dir, sprintf(".staging-audit-%d", year))
  unlink(audit_dir, recursive = TRUE)
  dir.create(audit_dir, recursive = TRUE)
  cleanup <- c(cleanup, audit_dir)
  complete <- TRUE
  return(list(
    listing = listing,
    members = members,
    extract = extract,
    files = files,
    inventory = inventory,
    audit_dir = audit_dir
  ))
}

assert_inventory_members <- function(inventory, files, label = "Inventory") {
  compared <- merge(
    inventory,
    files,
    by = "name",
    suffixes = c("_inventory", "_files")
  )
  if (
    nrow(compared) != nrow(files) ||
      any(
        compared$size_inventory != compared$size_files |
          compared$sha256_inventory != compared$sha256_files
      )
  ) {
    cli::cli_abort("{label} does not match the verified archive members.")
  }
  return(invisible(inventory))
}

assert_entry_evidence <- function(
  entry,
  archive,
  inventory_path,
  files,
  listing
) {
  verify_archive(archive, entry)
  inventory <- data.table::fread(inventory_path)
  if (
    !identical(
      digest::digest(file = inventory_path, algo = "sha256"),
      entry$inventory_sha256
    )
  ) {
    cli::cli_abort(
      "Staged inventory does not match the prospective provenance."
    )
  }
  if (
    nrow(inventory) != nrow(files) ||
      !setequal(inventory$name, files$name) ||
      !setequal(files$name, listing$names)
  ) {
    cli::cli_abort(
      "Staged inventory does not match the verified archive members."
    )
  }
  assert_inventory_members(inventory, files, "Staged inventory")
  return(invisible(entry))
}

read_inventory_evidence <- function(path, entry) {
  if (!file.exists(path)) {
    return(NULL)
  }
  if (
    !identical(
      digest::digest(file = path, algo = "sha256"),
      entry$inventory_sha256
    )
  ) {
    cli::cli_abort(
      "Inventory evidence {.file {path}} does not match its provenance SHA-256."
    )
  }
  inventory <- data.table::fread(path)
  expected_columns <- c("name", "size", "sha256")
  if (!all(expected_columns %in% names(inventory))) {
    cli::cli_abort(
      "Inventory evidence {.file {path}} lacks {.field {expected_columns}}."
    )
  }
  if (!nrow(inventory)) {
    cli::cli_abort("Inventory evidence {.file {path}} is empty.")
  }
  if (anyDuplicated(inventory$name)) {
    cli::cli_abort(
      "Inventory evidence {.file {path}} repeats file names."
    )
  }
  if (!identical(as.integer(entry$file_count), nrow(inventory))) {
    cli::cli_abort(
      "Inventory evidence {.file {path}} has {nrow(inventory)} files but provenance records {entry$file_count}."
    )
  }
  return(inventory)
}

stage_revisions_copy <- function(official_dir, tag) {
  source <- file.path(official_dir, "revisions.csv")
  if (!file.exists(source)) {
    return(NULL)
  }
  destination <- file.path(
    official_dir,
    "history",
    "revisions",
    tag,
    "revisions.csv"
  )
  if (file.exists(destination)) {
    if (
      !identical(
        digest::digest(file = source, algo = "sha256"),
        digest::digest(file = destination, algo = "sha256")
      )
    ) {
      cli::cli_abort(
        "History already holds different revision summaries at {.file {destination}}."
      )
    }
    return(NULL)
  }
  staged <- file.path(official_dir, ".staging-revisions.csv")
  if (!file.copy(source, staged, overwrite = TRUE)) {
    cli::cli_abort("Could not stage {.file {source}} for history.")
  }
  return(staged)
}

# Commit ----

# Moves are recorded so a caught filesystem, install, provenance, or audit
# failure can restore the prior state. Rollback uses base rename directly so a
# failed install cannot block restoration.

new_commit <- function() {
  state <- new.env(parent = emptyenv())
  state$moves <- list()
  state$backups <- character()
  return(state)
}

commit_move <- function(state, from, to) {
  if (!file.exists(from) && !dir.exists(from)) {
    cli::cli_abort("Staged path {.file {from}} is missing.")
  }
  if (file.exists(to) || dir.exists(to)) {
    cli::cli_abort("Refusing to overwrite {.file {to}}.")
  }
  if (!file.rename(from, to)) {
    cli::cli_abort("Could not move {.file {from}} to {.file {to}}.")
  }
  state$moves[[length(state$moves) + 1L]] <- list(from = from, to = to)
  return(invisible(to))
}

commit_backup <- function(state, path) {
  backup <- paste0(path, ".replaced")
  unlink(backup, recursive = TRUE)
  commit_move(state, path, backup)
  state$backups <- c(state$backups, backup)
  return(invisible(backup))
}

rollback_commit <- function(state) {
  for (move in rev(state$moves)) {
    if (
      (file.exists(move$to) || dir.exists(move$to)) &&
        !file.exists(move$from) &&
        !dir.exists(move$from)
    ) {
      if (!file.rename(move$to, move$from)) {
        cli::cli_warn("Could not restore {.file {move$from}} during rollback.")
      }
    }
  }
  return(invisible(NULL))
}

preserve_previous_evidence <- function(
  state,
  year,
  official_dir,
  cache_path,
  target,
  inventory_path,
  entry,
  archive_valid,
  inventory,
  revisions_copy
) {
  tag <- substr(entry$archive_sha256, 1L, 8L)
  history <- file.path(official_dir, "history")
  revisions_history <- file.path(history, "revisions", tag)
  directories <- c(
    file.path(history, "archives"),
    file.path(history, "extracts"),
    file.path(history, "inventory"),
    revisions_history
  )
  for (directory in directories) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (archive_valid) {
    archived_archive <- file.path(
      history,
      "archives",
      sprintf("inmet-%d-%s.zip", year, tag)
    )
    if (file.exists(archived_archive)) {
      if (
        !identical(
          digest::digest(file = archived_archive, algo = "sha256"),
          entry$archive_sha256
        )
      ) {
        cli::cli_abort(
          "History already holds different bytes at {.file {archived_archive}}."
        )
      }
      commit_backup(state, cache_path)
    } else {
      commit_move(state, cache_path, archived_archive)
    }
  } else if (file.exists(cache_path)) {
    commit_backup(state, cache_path)
  }
  if (dir.exists(target)) {
    archived_extract <- file.path(
      history,
      "extracts",
      sprintf("inmet_%d-%s", year, tag)
    )
    if (file.exists(archived_extract) || dir.exists(archived_extract)) {
      cli::cli_abort(
        "History already holds an extraction at {.file {archived_extract}}."
      )
    }
    commit_move(state, target, archived_extract)
  }
  if (!is.null(inventory)) {
    archived_inventory <- file.path(
      history,
      "inventory",
      sprintf("files-%d-%s.csv", year, tag)
    )
    if (file.exists(archived_inventory)) {
      if (
        !identical(
          digest::digest(file = archived_inventory, algo = "sha256"),
          entry$inventory_sha256
        )
      ) {
        cli::cli_abort(
          "History already holds different inventory bytes at {.file {archived_inventory}}."
        )
      }
      commit_backup(state, inventory_path)
    } else {
      commit_move(state, inventory_path, archived_inventory)
    }
  }
  audit_names <- c(
    sprintf("revisions-%d-variables.csv", year),
    sprintf("revisions-%d-stations.csv", year)
  )
  for (name in audit_names) {
    source <- file.path(official_dir, name)
    if (!file.exists(source)) {
      next
    }
    destination <- file.path(revisions_history, name)
    if (file.exists(destination)) {
      if (
        !identical(
          digest::digest(file = source, algo = "sha256"),
          digest::digest(file = destination, algo = "sha256")
        )
      ) {
        cli::cli_abort(
          "History already holds different audit bytes at {.file {destination}}."
        )
      }
      commit_backup(state, source)
    } else {
      commit_move(state, source, destination)
    }
  }
  if (!is.null(revisions_copy)) {
    commit_move(
      state,
      revisions_copy,
      file.path(revisions_history, "revisions.csv")
    )
  }
  provenance_path <- file.path(official_dir, "provenance.json")
  if (file.exists(provenance_path)) {
    destination <- file.path(revisions_history, "provenance.json")
    if (file.exists(destination)) {
      if (
        !identical(
          digest::digest(file = provenance_path, algo = "sha256"),
          digest::digest(file = destination, algo = "sha256")
        )
      ) {
        cli::cli_abort(
          "History already holds different provenance bytes at {.file {destination}}."
        )
      }
      commit_backup(state, provenance_path)
    } else {
      commit_move(state, provenance_path, destination)
    }
  }
  return(invisible(NULL))
}

install_audit <- function(state, audit_dir, official_dir) {
  files <- list.files(audit_dir, full.names = TRUE)
  for (file in files) {
    destination <- file.path(official_dir, basename(file))
    if (file.exists(destination)) {
      commit_backup(state, destination)
    }
    commit_move(state, file, destination)
  }
  return(invisible(NULL))
}

prune_empty_history <- function(official_dir) {
  history <- file.path(official_dir, "history")
  if (!dir.exists(history)) {
    return(invisible(NULL))
  }
  directories <- list.dirs(history, recursive = TRUE, full.names = TRUE)
  for (directory in rev(directories)) {
    if (!length(list.files(directory, all.files = TRUE, no.. = TRUE))) {
      file.remove(directory)
    }
  }
  return(invisible(NULL))
}

commit_or_rollback <- function(state, official_dir, code) {
  tryCatch(
    code,
    error = function(error) {
      rollback_commit(state)
      prune_empty_history(official_dir)
      cli::cli_abort(conditionMessage(error))
    }
  )
  unlink(state$backups, recursive = TRUE)
  prune_empty_history(official_dir)
  return(invisible(NULL))
}

audit_revisions <- function(
  year,
  legacy,
  official,
  output_dir = dirname(official)
) {
  if (!dir.exists(legacy)) {
    return(NULL)
  }
  old <- file_hashes(legacy)
  new <- file_hashes(official)
  if (!nrow(old)) {
    return(NULL)
  }
  compared <- merge(
    old,
    new,
    by = "name",
    all = TRUE,
    suffixes = c("_legacy", "_official")
  )
  changed_files <- compared[
    is.na(sha256_legacy) |
      is.na(sha256_official) |
      sha256_legacy != sha256_official
  ]
  if (!nrow(changed_files)) {
    return(list(
      year = year,
      legacy_files = nrow(old),
      official_files = nrow(new),
      matched_files = nrow(compared) -
        sum(is.na(compared$sha256_legacy) | is.na(compared$sha256_official)),
      revised_files = 0L,
      added_files = sum(is.na(compared$sha256_legacy)),
      removed_files = sum(is.na(compared$sha256_official)),
      overlapping_hours = 0L,
      changed_values = 0L,
      legacy_only_hours = 0L,
      official_only_hours = 0L
    ))
  }
  parse_all <- function(path) {
    files <- scan_csv_files(path)
    dat <- data.table::rbindlist(
      lapply(files, read_inmet_csv),
      use.names = TRUE
    )
    data.table::setorder(dat, code_station, timestamp_utc)
    return(unique(dat, by = c("code_station", "timestamp_utc")))
  }
  old_rows <- parse_all(legacy)
  new_rows <- parse_all(official)
  merged <- merge(
    old_rows,
    new_rows,
    by = c("code_station", "timestamp_utc"),
    suffixes = c("_legacy", "_official")
  )
  variable_changes <- data.table::rbindlist(lapply(
    inmet_variables,
    function(variable) {
      old_value <- merged[[paste0(variable, "_legacy")]]
      new_value <- merged[[paste0(variable, "_official")]]
      changed <- xor(is.na(old_value), is.na(new_value)) |
        (!is.na(old_value) & !is.na(new_value) & old_value != new_value)
      return(data.table::data.table(
        year = year,
        variable = variable,
        changed_values = sum(changed)
      ))
    }
  ))
  changed_values <- sum(variable_changes$changed_values)
  flags <- data.table::as.data.table(lapply(
    inmet_variables,
    function(variable) {
      old_value <- merged[[paste0(variable, "_legacy")]]
      new_value <- merged[[paste0(variable, "_official")]]
      return(
        xor(is.na(old_value), is.na(new_value)) |
          (!is.na(old_value) & !is.na(new_value) & old_value != new_value)
      )
    }
  ))
  data.table::setnames(flags, inmet_variables)
  flags[, code_station := merged$code_station]
  station_changes <- flags[,
    .(
      overlapping_hours = .N,
      changed_values = sum(rowSums(.SD))
    ),
    by = code_station
  ]
  data.table::fwrite(
    variable_changes,
    file.path(output_dir, sprintf("revisions-%d-variables.csv", year))
  )
  data.table::fwrite(
    station_changes,
    file.path(output_dir, sprintf("revisions-%d-stations.csv", year))
  )
  cli::cli_inform(
    "Audited {year}: {sum(changed_files$sha256_legacy != changed_files$sha256_official, na.rm = TRUE)} revised files, {changed_values} changed values."
  )
  return(list(
    year = year,
    legacy_files = nrow(old),
    official_files = nrow(new),
    matched_files = nrow(compared) -
      sum(is.na(compared$sha256_legacy) | is.na(compared$sha256_official)),
    revised_files = sum(
      !is.na(compared$sha256_legacy) &
        !is.na(compared$sha256_official) &
        compared$sha256_legacy != compared$sha256_official
    ),
    added_files = sum(is.na(compared$sha256_legacy)),
    removed_files = sum(is.na(compared$sha256_official)),
    overlapping_hours = nrow(merged),
    changed_values = changed_values,
    legacy_only_hours = nrow(old_rows) - nrow(merged),
    official_only_hours = nrow(new_rows) - nrow(merged)
  ))
}

legacy_year_dir <- function(data_root, year) {
  return(file.path(data_root, "zipped_files", sprintf("inmet_%d", year)))
}

reuse_cached_year <- function(
  year,
  data_root,
  official_dir,
  cache_path,
  target,
  inventory_path,
  provenance,
  entry
) {
  if (is.null(entry)) {
    cli::cli_abort(c(
      "Cached archive {.file {cache_path}} has no provenance.",
      "i" = "Run {.code Rscript data-raw/acquire.R --refresh {year}}."
    ))
  }
  assert_entry_complete(entry, year)
  verify_archive(cache_path, entry)
  listing <- archive_listing(cache_path)
  members <- archive_members(cache_path, listing)
  inventory <- read_inventory_evidence(inventory_path, entry)
  if (!is.null(inventory)) {
    assert_inventory_members(inventory, members, "Inventory evidence")
  }
  if (dir.exists(target)) {
    if (!is.null(inventory)) {
      verify_extraction(target, inventory, inventory_path)
    } else {
      verify_extraction(target, members, cache_path)
    }
  }
  if (!is.null(inventory)) {
    if (!dir.exists(target)) {
      staged_extract <- stage_extraction(
        year,
        cache_path,
        members,
        official_dir
      )
      on.exit(unlink(staged_extract, recursive = TRUE), add = TRUE)
      state <- new_commit()
      commit_or_rollback(state, official_dir, {
        commit_move(state, staged_extract, target)
      })
    }
    return(list(entry = entry, audit = NULL))
  }
  files <- if (dir.exists(target)) {
    file_hashes(target)
  } else {
    members[, .(name, size, sha256)]
  }
  staged_inventory <- stage_inventory(year, files, official_dir)
  on.exit(unlink(staged_inventory), add = TRUE)
  staged_extract <- NULL
  if (!dir.exists(target)) {
    staged_extract <- stage_extraction(year, cache_path, members, official_dir)
    on.exit(unlink(staged_extract, recursive = TRUE), add = TRUE)
  }
  new_entry <- entry
  new_entry$file_count <- nrow(files)
  new_entry$inventory_path <- sprintf("official/files-%d.csv", year)
  new_entry$inventory_sha256 <- digest::digest(
    file = staged_inventory,
    algo = "sha256"
  )
  provenance[[as.character(year)]] <- new_entry
  staged_provenance <- stage_provenance(provenance, official_dir, year)
  on.exit(unlink(staged_provenance), add = TRUE)
  provenance_path <- file.path(official_dir, "provenance.json")
  state <- new_commit()
  commit_or_rollback(state, official_dir, {
    if (!dir.exists(target)) {
      commit_move(state, staged_extract, target)
    }
    if (file.exists(inventory_path)) {
      commit_backup(state, inventory_path)
    }
    commit_move(state, staged_inventory, inventory_path)
    if (file.exists(provenance_path)) {
      commit_backup(state, provenance_path)
    }
    commit_move(state, staged_provenance, provenance_path)
  })
  return(list(entry = new_entry, audit = NULL))
}

install_same_bytes <- function(
  year,
  data_root,
  official_dir,
  cache_path,
  target,
  inventory_path,
  provenance,
  entry,
  snapshot,
  now
) {
  staged <- stage_snapshot(year, snapshot$path, official_dir)
  on.exit(unlink(staged$extract, recursive = TRUE), add = TRUE)
  on.exit(unlink(staged$inventory), add = TRUE)
  on.exit(unlink(staged$audit_dir, recursive = TRUE), add = TRUE)
  inventory <- read_inventory_evidence(inventory_path, entry)
  if (!is.null(inventory)) {
    assert_inventory_members(inventory, staged$files, "Inventory evidence")
  }
  if (dir.exists(target)) {
    if (!is.null(inventory)) {
      verify_extraction(target, inventory, inventory_path)
    } else {
      verify_extraction(target, staged$members, snapshot$path)
    }
  }
  cached <- file.exists(cache_path) && file.info(cache_path)$size > 0
  cache_current <- cached &&
    identical(
      digest::digest(file = cache_path, algo = "sha256"),
      entry$archive_sha256
    )
  new_entry <- entry
  new_entry$year <- year
  new_entry$source_url <- source_page
  new_entry$archive_url <- acquire_url(year)
  new_entry$retrieved_at_utc <- entry$retrieved_at_utc
  new_entry$revalidated_at_utc <- now
  new_entry$archive_path <- sprintf("cache/inmet-%d.zip", year)
  new_entry$archive_bytes <- unname(file.info(snapshot$path)$size)
  new_entry$archive_sha256 <- snapshot$sha256
  new_entry$declared_start <- staged$listing$declared_start %||%
    entry$declared_start
  new_entry$declared_end <- staged$listing$declared_end %||% entry$declared_end
  new_entry$file_count <- nrow(staged$files)
  new_entry$inventory_path <- sprintf("official/files-%d.csv", year)
  new_entry$inventory_sha256 <- if (is.null(inventory)) {
    digest::digest(file = staged$inventory, algo = "sha256")
  } else {
    entry$inventory_sha256
  }
  provenance[[as.character(year)]] <- new_entry
  staged_provenance <- stage_provenance(provenance, official_dir, year)
  on.exit(unlink(staged_provenance), add = TRUE)
  provenance_path <- file.path(official_dir, "provenance.json")
  state <- new_commit()
  commit_or_rollback(state, official_dir, {
    if (!cache_current) {
      if (file.exists(cache_path)) {
        commit_backup(state, cache_path)
      }
      commit_move(state, snapshot$path, cache_path)
    }
    if (!dir.exists(target)) {
      commit_move(state, staged$extract, target)
    }
    if (is.null(inventory)) {
      if (file.exists(inventory_path)) {
        commit_backup(state, inventory_path)
      }
      commit_move(state, staged$inventory, inventory_path)
    }
    if (file.exists(provenance_path)) {
      commit_backup(state, provenance_path)
    }
    commit_move(state, staged_provenance, provenance_path)
  })
  return(list(entry = new_entry, audit = NULL))
}

refresh_revision <- function(
  year,
  data_root,
  official_dir,
  cache_path,
  target,
  inventory_path,
  provenance,
  entry,
  snapshot,
  now
) {
  cached <- file.exists(cache_path) && file.info(cache_path)$size > 0
  archive_valid <- FALSE
  if (cached) {
    archive_valid <- tryCatch(
      {
        verify_archive(cache_path, entry)
        TRUE
      },
      error = function(error) FALSE
    )
  }
  inventory <- read_inventory_evidence(inventory_path, entry)
  members <- NULL
  if (archive_valid) {
    members <- archive_members(cache_path, archive_listing(cache_path))
    if (!is.null(inventory)) {
      assert_inventory_members(inventory, members, "Inventory evidence")
    }
  }
  if (dir.exists(target)) {
    if (!is.null(inventory)) {
      verify_extraction(target, inventory, inventory_path)
    } else if (!is.null(members)) {
      verify_extraction(target, members, cache_path)
    } else {
      cli::cli_abort(c(
        "Existing extraction {.file {target}} cannot be verified against the remaining evidence.",
        "i" = "Move it aside before refreshing {.val {year}}."
      ))
    }
  }
  staged <- stage_snapshot(year, snapshot$path, official_dir)
  on.exit(unlink(staged$extract, recursive = TRUE), add = TRUE)
  on.exit(unlink(staged$inventory), add = TRUE)
  on.exit(unlink(staged$audit_dir, recursive = TRUE), add = TRUE)

  tag <- substr(entry$archive_sha256, 1L, 8L)
  history_archive <- file.path(
    official_dir,
    "history",
    "archives",
    sprintf("inmet-%d-%s.zip", year, tag)
  )
  archive_preserved <- archive_valid
  if (file.exists(history_archive)) {
    if (
      !identical(
        digest::digest(file = history_archive, algo = "sha256"),
        entry$archive_sha256
      )
    ) {
      cli::cli_abort(
        "History already holds different bytes at {.file {history_archive}}."
      )
    }
    archive_preserved <- TRUE
  }
  prior <- entry
  prior$previous <- NULL
  prior$archive_path <- if (archive_preserved) {
    sprintf("official/history/archives/inmet-%d-%s.zip", year, tag)
  } else {
    NA_character_
  }
  prior$archive_missing <- !archive_preserved
  prior$extract_path <- if (dir.exists(target)) {
    sprintf("official/history/extracts/inmet_%d-%s", year, tag)
  } else {
    NA_character_
  }
  prior$extract_missing <- !dir.exists(target)
  prior$inventory_path <- if (!is.null(inventory)) {
    sprintf("official/history/inventory/files-%d-%s.csv", year, tag)
  } else {
    NA_character_
  }
  prior$inventory_missing <- is.null(inventory)

  new_entry <- provenance_entry(
    year,
    snapshot$path,
    sprintf("cache/inmet-%d.zip", year),
    sprintf("official/files-%d.csv", year),
    retrieved_at = now,
    headers = snapshot$headers,
    listing = staged$listing,
    files = staged$files,
    inventory_sha256 = digest::digest(file = staged$inventory, algo = "sha256"),
    previous = c(list(prior), entry$previous %||% list())
  )
  assert_entry_evidence(
    new_entry,
    snapshot$path,
    staged$inventory,
    staged$files,
    staged$listing
  )
  provenance[[as.character(year)]] <- new_entry
  staged_provenance <- stage_provenance(provenance, official_dir, year)
  on.exit(unlink(staged_provenance), add = TRUE)
  audit <- audit_revisions(
    year,
    legacy_year_dir(data_root, year),
    staged$extract,
    output_dir = staged$audit_dir
  )
  write_revision_summary(
    data.table::rbindlist(list(audit)),
    official_dir,
    output_dir = staged$audit_dir
  )
  revisions_copy <- stage_revisions_copy(official_dir, tag)
  if (!is.null(revisions_copy)) {
    on.exit(unlink(revisions_copy), add = TRUE)
  }
  provenance_path <- file.path(official_dir, "provenance.json")
  state <- new_commit()
  commit_or_rollback(state, official_dir, {
    preserve_previous_evidence(
      state,
      year,
      official_dir,
      cache_path,
      target,
      inventory_path,
      entry,
      archive_valid,
      inventory,
      revisions_copy
    )
    commit_move(state, snapshot$path, cache_path)
    commit_move(state, staged$extract, target)
    commit_move(state, staged$inventory, inventory_path)
    install_audit(state, staged$audit_dir, official_dir)
    commit_move(state, staged_provenance, provenance_path)
  })
  return(list(entry = new_entry, audit = audit))
}

install_new_year <- function(
  year,
  data_root,
  official_dir,
  cache_path,
  target,
  inventory_path,
  provenance,
  snapshot,
  now,
  cached
) {
  staged <- stage_snapshot(year, snapshot$path, official_dir)
  on.exit(unlink(staged$extract, recursive = TRUE), add = TRUE)
  on.exit(unlink(staged$inventory), add = TRUE)
  on.exit(unlink(staged$audit_dir, recursive = TRUE), add = TRUE)
  if (dir.exists(target)) {
    verify_extraction(target, staged$members, snapshot$path)
  }
  if (file.exists(inventory_path)) {
    orphan <- data.table::fread(inventory_path)
    expected_columns <- c("name", "size", "sha256")
    if (!all(expected_columns %in% names(orphan))) {
      cli::cli_abort(
        "Orphan inventory {.file {inventory_path}} lacks {.field {expected_columns}}."
      )
    }
    compared <- merge(
      orphan,
      staged$files,
      by = "name",
      suffixes = c("_orphan", "_staged")
    )
    if (
      nrow(compared) != nrow(staged$files) ||
        any(
          compared$size_orphan != compared$size_staged |
            compared$sha256_orphan != compared$sha256_staged
        )
    ) {
      cli::cli_abort(c(
        "Orphan inventory {.file {inventory_path}} does not match the downloaded archive.",
        "i" = "Move it aside before acquiring {.val {year}}."
      ))
    }
  }
  new_entry <- provenance_entry(
    year,
    snapshot$path,
    sprintf("cache/inmet-%d.zip", year),
    sprintf("official/files-%d.csv", year),
    retrieved_at = now,
    headers = snapshot$headers,
    listing = staged$listing,
    files = staged$files,
    inventory_sha256 = digest::digest(file = staged$inventory, algo = "sha256")
  )
  assert_entry_evidence(
    new_entry,
    snapshot$path,
    staged$inventory,
    staged$files,
    staged$listing
  )
  provenance[[as.character(year)]] <- new_entry
  staged_provenance <- stage_provenance(provenance, official_dir, year)
  on.exit(unlink(staged_provenance), add = TRUE)
  audit <- audit_revisions(
    year,
    legacy_year_dir(data_root, year),
    staged$extract,
    output_dir = staged$audit_dir
  )
  write_revision_summary(
    data.table::rbindlist(list(audit)),
    official_dir,
    output_dir = staged$audit_dir
  )
  provenance_path <- file.path(official_dir, "provenance.json")
  state <- new_commit()
  commit_or_rollback(state, official_dir, {
    if (!cached) {
      if (file.exists(cache_path)) {
        commit_backup(state, cache_path)
      }
      commit_move(state, snapshot$path, cache_path)
    }
    if (dir.exists(target)) {
      commit_backup(state, target)
    }
    commit_move(state, staged$extract, target)
    if (file.exists(inventory_path)) {
      commit_backup(state, inventory_path)
    }
    commit_move(state, staged$inventory, inventory_path)
    install_audit(state, staged$audit_dir, official_dir)
    if (file.exists(provenance_path)) {
      commit_backup(state, provenance_path)
    }
    commit_move(state, staged_provenance, provenance_path)
  })
  return(list(entry = new_entry, audit = audit))
}

acquire_year <- function(
  year,
  data_root,
  refresh = FALSE,
  transport = http_transport()
) {
  official_dir <- file.path(data_root, "official")
  cache_dir <- file.path(data_root, "cache")
  dir.create(official_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_path <- file.path(cache_dir, sprintf("inmet-%d.zip", year))
  target <- file.path(official_dir, sprintf("inmet_%d", year))
  inventory_path <- file.path(official_dir, sprintf("files-%d.csv", year))
  provenance <- read_provenance(official_dir)
  key <- as.character(year)
  entry <- provenance[[key]]
  cached <- file.exists(cache_path) && file.info(cache_path)$size > 0
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  snapshot <- if (refresh || !cached) {
    download_snapshot(year, cache_path, transport)
  } else {
    NULL
  }
  if (!is.null(snapshot)) {
    on.exit(unlink(snapshot$path), add = TRUE)
  }
  if (is.null(snapshot)) {
    return(reuse_cached_year(
      year,
      data_root,
      official_dir,
      cache_path,
      target,
      inventory_path,
      provenance,
      entry
    ))
  }
  if (!is.null(entry)) {
    assert_entry_complete(entry, year)
    if (identical(snapshot$sha256, entry$archive_sha256)) {
      return(install_same_bytes(
        year,
        data_root,
        official_dir,
        cache_path,
        target,
        inventory_path,
        provenance,
        entry,
        snapshot,
        now
      ))
    }
    return(refresh_revision(
      year,
      data_root,
      official_dir,
      cache_path,
      target,
      inventory_path,
      provenance,
      entry,
      snapshot,
      now
    ))
  }
  if (cached) {
    cached_sha256 <- digest::digest(file = cache_path, algo = "sha256")
    if (!identical(snapshot$sha256, cached_sha256)) {
      cli::cli_abort(c(
        "Cached archive {.file {cache_path}} has no provenance and differs from the download.",
        "i" = "Move it aside before acquiring {.val {year}}."
      ))
    }
  }
  return(install_new_year(
    year,
    data_root,
    official_dir,
    cache_path,
    target,
    inventory_path,
    provenance,
    snapshot,
    now,
    cached
  ))
}

write_revision_summary <- function(
  audit,
  official_dir,
  output_dir = official_dir
) {
  if (!length(audit) || !nrow(audit)) {
    return(invisible(NULL))
  }
  revisions_path <- file.path(official_dir, "revisions.csv")
  if (file.exists(revisions_path) && file.info(revisions_path)$size > 0) {
    previous <- data.table::fread(revisions_path)
    audit <- data.table::rbindlist(list(
      previous[!year %in% audit$year],
      audit
    ))
    data.table::setorder(audit, year)
  }
  data.table::fwrite(audit, file.path(output_dir, "revisions.csv"))
  return(invisible(NULL))
}

acquire_years <- function(
  years,
  data_root,
  refresh = FALSE,
  transport = http_transport()
) {
  audit <- list()
  for (year in years) {
    cli::cli_inform("Acquiring {year} from the official INMET archive.")
    result <- acquire_year(
      year,
      data_root,
      refresh = refresh,
      transport = transport
    )
    audit[[as.character(year)]] <- result$audit
  }
  audit <- data.table::rbindlist(Filter(Negate(is.null), audit))
  cli::cli_alert_success(
    "Verified {length(years)} official INMET years under {.path {data_root}}."
  )
  return(invisible(audit))
}
