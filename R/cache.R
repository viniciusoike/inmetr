.data_version <- "2026-10-24"
.release_url <- paste0(
  "https://github.com/viniciusoike/inmetr/releases/download/data-",
  .data_version
)

#' Locate the inmetr cache
#' @return The cache directory path.
#' @export
#' @examplesIf interactive()
#' inmet_cache_dir()
inmet_cache_dir <- function() {
  return(.cache_dir(sys.call()))
}

.cache_dir <- function(call) {
  path <- getOption("inmetr.cache_dir", tools::R_user_dir("inmetr", "cache"))
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    cli::cli_abort(
      "{.opt inmetr.cache_dir} must be one nonempty path.",
      call = call
    )
  }
  return(path)
}

#' Clear the inmetr data cache
#' @param ask Whether to ask for confirmation before deleting cached files.
#' @return Invisibly returns whether files were removed.
#' @export
#' @examplesIf interactive()
#' inmet_cache_clear()
inmet_cache_clear <- function(ask = interactive()) {
  call <- sys.call()
  path <- file.path(.cache_dir(call), .data_version)
  if (!dir.exists(path)) {
    return(invisible(FALSE))
  }
  if (ask) {
    cli::cli_inform("Delete cached inmetr data at {.path {path}}?")
    cli::cli_inform("Type {.val yes} to confirm:")
    if (!identical(tolower(readline("")), "yes")) {
      return(invisible(FALSE))
    }
  }
  unlink(path, recursive = TRUE)
  if (dir.exists(path)) {
    cli::cli_abort(
      "Could not clear the cache at {.path {path}}.",
      call = call
    )
  }
  return(invisible(TRUE))
}

.asset_name <- function(path, call = rlang::caller_env()) {
  if (identical(path, "stations.parquet")) {
    return(path)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli::cli_abort("Invalid manifest file path.", call = call)
  }
  pieces <- regmatches(
    path,
    regexec("^(daily|hourly)/year=([0-9]{4})/part-0+[.]parquet$", path)
  )[[1]]
  if (length(pieces) != 3L) {
    cli::cli_abort(
      "Invalid manifest file path {.val {path}}.",
      call = call
    )
  }
  return(sprintf("%s-%s.parquet", pieces[[2]], pieces[[3]]))
}

.fetch_asset <- function(
  asset,
  destination,
  sha256 = NULL,
  call = rlang::caller_env()
) {
  base <- getOption("inmetr.base_url", .release_url)
  if (
    !is.character(base) || length(base) != 1L || is.na(base) || !nzchar(base)
  ) {
    cli::cli_abort(
      "{.opt inmetr.base_url} must be one nonempty URL or directory.",
      call = call
    )
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(
    pattern = paste0(basename(destination), "-"),
    tmpdir = dirname(destination)
  )
  on.exit(unlink(temp), add = TRUE)
  if (startsWith(base, "file://")) {
    base <- utils::URLdecode(substring(base, 8L))
  }
  if (dir.exists(base)) {
    if (!file.copy(file.path(base, asset), temp)) {
      cli::cli_abort(
        "Could not copy {.file {asset}} from {.path {base}}.",
        call = call
      )
    }
  } else {
    url <- paste0(sub("/$", "", base), "/", asset)
    timeout <- getOption("timeout", 60)
    options(timeout = max(3600, timeout))
    on.exit(options(timeout = timeout), add = TRUE)
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        url,
        temp,
        mode = "wb",
        quiet = TRUE
      )),
      error = function(e) {
        cli::cli_abort(
          "Could not download {.file {asset}}: {conditionMessage(e)}",
          call = call
        )
      }
    )
    if (!identical(status, 0L) || !file.exists(temp)) {
      cli::cli_abort(
        "Could not download {.file {asset}} from {.url {url}}.",
        call = call
      )
    }
  }
  if (identical(asset, "manifest.json")) {
    tryCatch(
      jsonlite::fromJSON(temp, simplifyVector = FALSE),
      error = function(e) {
        cli::cli_abort(
          "Invalid downloaded {.file manifest.json}: {conditionMessage(e)}",
          call = call
        )
      }
    )
  } else if (
    !is.null(sha256) &&
      !identical(digest::digest(file = temp, algo = "sha256"), sha256)
  ) {
    cli::cli_abort("SHA-256 mismatch for {.file {asset}}.", call = call)
  }
  if (!file.rename(temp, destination)) {
    cli::cli_abort(
      "Could not save {.file {asset}} to {.path {destination}}.",
      call = call
    )
  }
  return(destination)
}

.manifest <- function(call = rlang::caller_env()) {
  path <- file.path(.cache_dir(call), .data_version, "manifest.json")
  if (!file.exists(path)) {
    .fetch_asset("manifest.json", path, call = call)
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      unlink(path)
      .fetch_asset("manifest.json", path, call = call)
      return(tryCatch(
        jsonlite::fromJSON(path, simplifyVector = FALSE),
        error = function(e) {
          cli::cli_abort(
            "Invalid cached {.file manifest.json}: {conditionMessage(e)}",
            call = call
          )
        }
      ))
    }
  )
  if (
    !identical(manifest$version, .data_version) ||
      !identical(manifest$schema_version, 2L) ||
      !all(c("daily", "hourly", "stations") %in% names(manifest$tables))
  ) {
    cli::cli_abort(
      "Cached {.file manifest.json} has an unsupported version or schema.",
      call = call
    )
  }
  for (table in manifest$tables) {
    if (!is.list(table) || !is.list(table$files)) {
      cli::cli_abort(
        "Cached {.file manifest.json} has an invalid file entry.",
        call = call
      )
    }
    for (entry in table$files) {
      if (!is.list(entry)) {
        cli::cli_abort(
          "Cached {.file manifest.json} has an invalid file entry.",
          call = call
        )
      }
      .asset_name(entry$path, call = call)
      if (
        !is.numeric(entry$size) ||
          length(entry$size) != 1L ||
          !is.finite(entry$size) ||
          entry$size < 0 ||
          entry$size != floor(entry$size) ||
          !is.character(entry$sha256) ||
          length(entry$sha256) != 1L ||
          is.na(entry$sha256) ||
          !grepl("^[0-9a-f]{64}$", entry$sha256)
      ) {
        cli::cli_abort(
          "Cached {.file manifest.json} has an invalid file entry.",
          call = call
        )
      }
    }
  }
  return(manifest)
}

.ensure_file <- function(entry, call = rlang::caller_env()) {
  asset <- .asset_name(entry$path, call = call)
  path <- file.path(.cache_dir(call), .data_version, entry$path)
  if (file.exists(path) && !isTRUE(file.info(path)$size == entry$size)) {
    unlink(path)
  }
  if (!file.exists(path)) {
    .fetch_asset(asset, path, sha256 = entry$sha256, call = call)
  }
  return(path)
}
