# Acquire official INMET archives ----

# Downloads INMET's yearly historical archives into a cache, verifies their
# SHA-256, extracts each year into data-raw/data/official/inmet_YYYY, and
# audits the result against the legacy extracts in data-raw/data/zipped_files.
# Existing files are never overwritten. Nothing is uploaded.

dependencies <- c("cli", "data.table", "digest", "here", "jsonlite")
missing <- dependencies[
  !vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop(paste0(
    "Install acquisition dependencies: ",
    paste(missing, collapse = ", ")
  ))
}
`%||%` <- function(x, y) if (is.null(x)) y else x
source(here::here("data-raw", "pipeline", "parse.R"))

args <- commandArgs(trailingOnly = TRUE)
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- if (length(args)) {
  suppressWarnings(as.integer(args))
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
years <- sort(years)

source_page <- "https://portal.inmet.gov.br/dadoshistoricos"
base_url <- "https://portal.inmet.gov.br/uploads/dadoshistoricos"
user_agent <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0 Safari/537.36"
)
data_root <- here::here("data-raw", "data")
cache_dir <- file.path(data_root, "cache")
official_dir <- file.path(data_root, "official")
legacy_root <- file.path(data_root, "zipped_files")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(official_dir, recursive = TRUE, showWarnings = FALSE)
provenance_path <- file.path(official_dir, "provenance.json")

read_provenance <- function() {
  if (!file.exists(provenance_path)) {
    return(list())
  }
  return(jsonlite::fromJSON(provenance_path, simplifyVector = FALSE))
}

source_headers <- function(url) {
  if (!nzchar(Sys.which("curl"))) {
    return(list())
  }
  lines <- suppressWarnings(system2(
    "curl",
    c(
      "-sSIL",
      "--http1.1",
      "--max-time",
      "60",
      "-A",
      shQuote(user_agent),
      shQuote(url)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!length(lines) || !is.null(attr(lines, "status"))) {
    return(list())
  }
  lines <- sub("\r$", "", lines)
  value <- function(name) {
    hit <- grep(paste0("^", name, ":"), lines, ignore.case = TRUE, value = TRUE)
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

download_archive <- function(year) {
  url <- sprintf("%s/%d.zip", base_url, year)
  path <- file.path(cache_dir, sprintf("inmet-%d.zip", year))
  if (file.exists(path) && file.info(path)$size > 0) {
    return(list(path = path, url = url))
  }
  temporary <- paste0(path, ".part")
  on.exit(unlink(temporary), add = TRUE)
  timeout <- getOption("timeout", 60)
  options(timeout = max(3600, timeout))
  on.exit(options(timeout = timeout), add = TRUE)
  for (attempt in 1:3) {
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        url,
        temporary,
        mode = "wb",
        quiet = TRUE,
        headers = c("User-Agent" = user_agent)
      )),
      error = function(e) conditionMessage(e)
    )
    if (identical(status, 0L) && file.exists(temporary)) {
      break
    }
    if (attempt == 3L) {
      cli::cli_abort(
        "Could not download {.url {url}}: {status}."
      )
    }
    Sys.sleep(5)
  }
  if (!file.rename(temporary, path)) {
    cli::cli_abort("Could not cache {.file {path}}.")
  }
  return(list(path = path, url = url))
}

zip_inventory <- function(path) {
  listing <- utils::unzip(path, list = TRUE)
  csv <- listing[
    grepl("\\.[Cc][Ss][Vv]$", listing$Name) &
      !startsWith(basename(listing$Name), "._"),
  ]
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

extract_archive <- function(year, archive, inventory, existing) {
  target <- file.path(official_dir, sprintf("inmet_%d", year))
  if (dir.exists(target)) {
    files <- list.files(target, "\\.CSV$", recursive = TRUE)
    if (!identical(sort(basename(files)), sort(inventory$names))) {
      cli::cli_abort(
        "Extracted archive {.file {target}} has different files from {.file {archive}}. Move it aside before re-extracting."
      )
    }
    if (
      !is.null(existing$archive_sha256) &&
        !identical(
          existing$archive_sha256,
          digest::digest(file = archive, algo = "sha256")
        )
    ) {
      cli::cli_abort(
        "Extracted archive {.file {target}} does not match {.file {archive}}. Move it aside before re-extracting."
      )
    }
    return(target)
  }
  staging <- file.path(official_dir, sprintf(".staging-inmet_%d", year))
  unlink(staging, recursive = TRUE)
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  unpacked <- tempfile("inmet-unpack-")
  dir.create(unpacked)
  on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
  utils::unzip(archive, exdir = unpacked)
  files <- list.files(unpacked, "\\.CSV$", recursive = TRUE, full.names = TRUE)
  files <- files[!startsWith(basename(files), "._")]
  if (!identical(sort(basename(files)), sort(inventory$names))) {
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

file_hashes <- function(path) {
  files <- list.files(path, "\\.CSV$", recursive = TRUE, full.names = TRUE)
  files <- files[!startsWith(basename(files), "._")]
  if (!length(files)) {
    return(data.table::data.table(
      name = character(),
      size = numeric(),
      sha256 = character()
    ))
  }
  return(data.table::data.table(
    name = basename(files),
    size = unname(file.info(files)$size),
    sha256 = vapply(
      files,
      digest::digest,
      character(1),
      algo = "sha256",
      file = TRUE
    )
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
    files <- list.files(path, "\\.CSV$", recursive = TRUE, full.names = TRUE)
    files <- files[!startsWith(basename(files), "._")]
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
    file.path(official_dir, sprintf("revisions-%d-variables.csv", year))
  )
  data.table::fwrite(
    station_changes,
    file.path(official_dir, sprintf("revisions-%d-stations.csv", year))
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

provenance <- read_provenance()
audit <- list()
for (year in years) {
  cli::cli_inform("Acquiring {year} from the official INMET archive.")
  archive <- download_archive(year)
  inventory <- zip_inventory(archive$path)
  headers <- source_headers(archive$url)
  zip_sha256 <- digest::digest(file = archive$path, algo = "sha256")
  retrieved_at <- format(
    file.info(archive$path)$mtime,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  extracted <- extract_archive(
    year,
    archive$path,
    inventory,
    provenance[[as.character(year)]] %||% list()
  )
  files <- file_hashes(extracted)
  data.table::fwrite(
    files,
    file.path(official_dir, sprintf("files-%d.csv", year))
  )
  provenance[[as.character(year)]] <- list(
    year = year,
    source_url = source_page,
    archive_url = archive$url,
    retrieved_at_utc = retrieved_at,
    archive_path = sub(
      paste0("^", normalizePath(data_root, winslash = "/"), "/"),
      "",
      normalizePath(archive$path, winslash = "/")
    ),
    archive_bytes = unname(file.info(archive$path)$size),
    archive_sha256 = zip_sha256,
    last_modified = headers$last_modified %||% NA_character_,
    etag = headers$etag %||% NA_character_,
    declared_start = inventory$declared_start,
    declared_end = inventory$declared_end,
    file_count = nrow(files)
  )
  audit[[as.character(year)]] <- audit_revisions(
    year,
    file.path(legacy_root, sprintf("inmet_%d", year)),
    extracted
  )
}
jsonlite::write_json(
  provenance,
  provenance_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
audit <- data.table::rbindlist(Filter(Negate(is.null), audit))
revisions_path <- file.path(official_dir, "revisions.csv")
if (
  length(audit) &&
    file.exists(revisions_path) &&
    file.info(revisions_path)$size > 0
) {
  previous <- data.table::fread(revisions_path)
  audit <- data.table::rbindlist(list(
    previous[!year %in% audit$year],
    audit
  ))
  data.table::setorder(audit, year)
}
if (length(audit)) {
  data.table::fwrite(audit, revisions_path)
}
cli::cli_alert_success(
  "Cached and extracted {length(years)} official INMET archives under {.path {official_dir}}."
)
