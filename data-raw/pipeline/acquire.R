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
  data_root,
  retrieved_at,
  headers,
  previous = NULL
) {
  listing <- archive_listing(archive)
  return(list(
    year = year,
    source_url = source_page,
    archive_url = acquire_url(year),
    retrieved_at_utc = retrieved_at,
    archive_path = relative_path(data_root, archive),
    archive_bytes = unname(file.info(archive)$size),
    archive_sha256 = digest::digest(file = archive, algo = "sha256"),
    last_modified = headers$last_modified %||% NA_character_,
    etag = headers$etag %||% NA_character_,
    declared_start = listing$declared_start,
    declared_end = listing$declared_end,
    file_count = length(listing$names),
    previous = previous
  ))
}

extract_archive <- function(year, archive, members, official_dir) {
  target <- file.path(official_dir, sprintf("inmet_%d", year))
  if (dir.exists(target)) {
    cli::cli_abort(
      "Extracted archive {.file {target}} already exists; move it aside first."
    )
  }
  staging <- file.path(official_dir, sprintf(".staging-inmet_%d", year))
  unlink(staging, recursive = TRUE)
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  unpacked <- tempfile("inmet-unpack-")
  dir.create(unpacked)
  on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
  utils::unzip(archive, exdir = unpacked)
  files <- scan_csv_files(unpacked)
  if (!identical(sort(basename(files)), sort(members$name))) {
    cli::cli_abort("Extracted CSV names do not match the archive listing.")
  }
  if (!all(file.copy(files, file.path(staging, basename(files))))) {
    cli::cli_abort("Could not stage extracted CSVs for {year}.")
  }
  if (!file.rename(staging, target)) {
    cli::cli_abort("Could not finalize extraction at {.file {target}}.")
  }
  return(target)
}

preserve_current_year <- function(
  year,
  cache_path,
  target,
  inventory_path,
  official_dir,
  data_root,
  entry
) {
  if (is.null(entry)) {
    cli::cli_abort(c(
      "Cached archive {.file {cache_path}} has no provenance, so revised bytes cannot be audited.",
      "i" = "Record its provenance first or move the archive aside."
    ))
  }
  verify_archive(cache_path, entry)
  old_sha256 <- entry$archive_sha256
  tag <- substr(old_sha256, 1, 8)
  history <- file.path(official_dir, "history")
  archive_history <- file.path(history, "archives")
  extract_history <- file.path(history, "extracts")
  inventory_history <- file.path(history, "inventory")
  revisions_history <- file.path(history, "revisions", tag)
  directories <- c(
    archive_history,
    extract_history,
    inventory_history,
    revisions_history
  )
  for (directory in directories) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  archived_archive <- file.path(
    archive_history,
    sprintf("inmet-%d-%s.zip", year, tag)
  )
  if (file.exists(archived_archive)) {
    preserved <- digest::digest(file = archived_archive, algo = "sha256")
    if (!identical(preserved, old_sha256)) {
      cli::cli_abort(
        "History already holds different bytes at {.file {archived_archive}}."
      )
    }
    unlink(cache_path)
  } else if (!file.rename(cache_path, archived_archive)) {
    cli::cli_abort("Could not preserve {.file {cache_path}} in history.")
  }
  if (dir.exists(target)) {
    archived_extract <- file.path(
      extract_history,
      sprintf("inmet_%d-%s", year, tag)
    )
    if (file.exists(archived_extract)) {
      cli::cli_abort(
        "History already holds an extraction at {.file {archived_extract}}."
      )
    }
    if (!file.rename(target, archived_extract)) {
      cli::cli_abort("Could not preserve {.file {target}} in history.")
    }
  }
  archived_inventory <- file.path(
    inventory_history,
    sprintf("files-%d-%s.csv", year, tag)
  )
  if (
    file.exists(inventory_path) &&
      !file.copy(inventory_path, archived_inventory, overwrite = TRUE)
  ) {
    cli::cli_abort("Could not preserve {.file {inventory_path}} in history.")
  }
  audit_names <- c(
    sprintf("revisions-%d-variables.csv", year),
    sprintf("revisions-%d-stations.csv", year),
    "revisions.csv"
  )
  for (name in audit_names) {
    source <- file.path(official_dir, name)
    if (
      file.exists(source) &&
        !file.copy(source, file.path(revisions_history, name), overwrite = TRUE)
    ) {
      cli::cli_abort("Could not preserve {.file {source}} in history.")
    }
  }
  prior <- entry
  prior$previous <- NULL
  prior$archive_path <- relative_path(data_root, archived_archive)
  if (file.exists(archived_inventory)) {
    prior$inventory_path <- relative_path(data_root, archived_inventory)
  }
  return(list(
    previous = c(list(prior), entry$previous %||% list()),
    archived_archive = archived_archive,
    archived_inventory = archived_inventory
  ))
}

audit_revisions <- function(year, legacy, official) {
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
    file.path(dirname(official), sprintf("revisions-%d-variables.csv", year))
  )
  data.table::fwrite(
    station_changes,
    file.path(dirname(official), sprintf("revisions-%d-stations.csv", year))
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
  if (is.null(snapshot)) {
    if (is.null(entry)) {
      cli::cli_abort(c(
        "Cached archive {.file {cache_path}} has no provenance.",
        "i" = "Run {.code Rscript data-raw/acquire.R --refresh {year}}."
      ))
    }
    verify_archive(cache_path, entry)
  } else if (
    cached &&
      identical(
        snapshot$sha256,
        digest::digest(file = cache_path, algo = "sha256")
      )
  ) {
    unlink(snapshot$path)
    if (is.null(entry)) {
      entry <- provenance_entry(
        year,
        cache_path,
        data_root,
        retrieved_at = now,
        headers = snapshot$headers
      )
    } else {
      verify_archive(cache_path, entry)
      entry$revalidated_at_utc <- now
    }
  } else {
    preserved <- if (cached) {
      preserve_current_year(
        year,
        cache_path,
        target,
        inventory_path,
        official_dir,
        data_root,
        entry
      )
    } else {
      list(previous = entry$previous %||% NULL)
    }
    if (!file.rename(snapshot$path, cache_path)) {
      cli::cli_abort("Could not cache {.file {cache_path}}.")
    }
    entry <- provenance_entry(
      year,
      cache_path,
      data_root,
      retrieved_at = now,
      headers = snapshot$headers,
      previous = preserved$previous
    )
  }

  listing <- archive_listing(cache_path)
  members <- archive_members(cache_path, listing)
  if (dir.exists(target)) {
    verify_extraction(target, members, cache_path)
  } else {
    extract_archive(year, cache_path, members, official_dir)
    verify_extraction(target, members, cache_path)
  }
  files <- file_hashes(target)
  write_inventory(files, inventory_path)
  entry$year <- year
  entry$source_url <- source_page
  entry$archive_url <- acquire_url(year)
  entry$archive_path <- relative_path(data_root, cache_path)
  entry$archive_bytes <- unname(file.info(cache_path)$size)
  entry$archive_sha256 <- digest::digest(file = cache_path, algo = "sha256")
  entry$declared_start <- listing$declared_start %||% entry$declared_start
  entry$declared_end <- listing$declared_end %||% entry$declared_end
  entry$file_count <- nrow(files)
  entry$inventory_path <- relative_path(data_root, inventory_path)
  entry$inventory_sha256 <- digest::digest(
    file = inventory_path,
    algo = "sha256"
  )
  provenance[[key]] <- entry
  write_provenance(provenance, official_dir)
  audit <- audit_revisions(
    year,
    file.path(data_root, "zipped_files", sprintf("inmet_%d", year)),
    target
  )
  return(list(entry = entry, audit = audit))
}

write_revision_summary <- function(audit, official_dir) {
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
  data.table::fwrite(audit, revisions_path)
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
  write_revision_summary(audit, file.path(data_root, "official"))
  cli::cli_alert_success(
    "Verified {length(years)} official INMET years under {.path {data_root}}."
  )
  return(invisible(audit))
}
