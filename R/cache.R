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
  path <- getOption("inmetr.cache_dir", tools::R_user_dir("inmetr", "cache"))
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    cli::cli_abort("{.opt inmetr.cache_dir} must be one nonempty path.")
  }
  return(path)
}

#' Clear the inmetr data cache
#' @return Invisibly returns whether files were removed.
#' @export
#' @examplesIf interactive()
#' inmet_cache_clear()
inmet_cache_clear <- function() {
  path <- file.path(inmet_cache_dir(), .data_version)
  if (!dir.exists(path)) {
    return(invisible(FALSE))
  }
  if (interactive()) {
    cli::cli_inform("Delete cached inmetr data at {.path {path}}?")
    if (!identical(tolower(readline("Type yes to confirm: ")), "yes")) {
      return(invisible(FALSE))
    }
  }
  unlink(path, recursive = TRUE)
  if (dir.exists(path)) {
    cli::cli_abort("Could not clear the cache at {.path {path}}.")
  }
  return(invisible(TRUE))
}

.asset_name <- function(path) {
  if (path %in% c("stations.parquet", "manifest.json")) {
    return(path)
  }
  pieces <- regmatches(
    path,
    regexec("^(daily|hourly)/year=([0-9]{4})/part-[^/]+[.]parquet$", path)
  )[[1]]
  if (length(pieces) != 3L) {
    cli::cli_abort(
      "Invalid manifest file path {.val {path}}.",
      call = rlang::caller_env()
    )
  }
  return(sprintf("%s-%s.parquet", pieces[[2]], pieces[[3]]))
}

.fetch_asset <- function(asset, destination) {
  base <- getOption("inmetr.base_url", .release_url)
  if (
    !is.character(base) || length(base) != 1L || is.na(base) || !nzchar(base)
  ) {
    cli::cli_abort(
      "{.opt inmetr.base_url} must be one nonempty URL or directory.",
      call = rlang::caller_env()
    )
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (startsWith(base, "file://")) {
    base <- utils::URLdecode(substring(base, 8L))
  }
  if (dir.exists(base)) {
    if (!file.copy(file.path(base, asset), destination, overwrite = TRUE)) {
      cli::cli_abort(
        "Could not copy {.file {asset}} from {.path {base}}.",
        call = rlang::caller_env()
      )
    }
  } else {
    url <- paste0(sub("/$", "", base), "/", asset)
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        url,
        destination,
        mode = "wb",
        quiet = TRUE
      )),
      error = function(e) {
        unlink(destination)
        cli::cli_abort(
          "Could not download {.file {asset}}: {conditionMessage(e)}",
          call = rlang::caller_env()
        )
      }
    )
    if (!identical(status, 0L) || !file.exists(destination)) {
      unlink(destination)
      cli::cli_abort(
        "Could not download {.file {asset}} from {.url {url}}.",
        call = rlang::caller_env()
      )
    }
  }
  return(destination)
}

.manifest <- function() {
  path <- file.path(inmet_cache_dir(), .data_version, "manifest.json")
  if (!file.exists(path)) {
    .fetch_asset("manifest.json", path)
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        "Invalid cached {.file manifest.json}: {conditionMessage(e)}",
        call = rlang::caller_env()
      )
    }
  )
  if (
    !identical(manifest$version, .data_version) ||
      !identical(manifest$schema_version, 2L) ||
      !all(c("daily", "hourly", "stations") %in% names(manifest$tables))
  ) {
    cli::cli_abort(
      "Cached {.file manifest.json} has an unsupported version or schema.",
      call = rlang::caller_env()
    )
  }
  return(manifest)
}

.ensure_file <- function(entry) {
  path <- file.path(inmet_cache_dir(), .data_version, entry$path)
  if (!file.exists(path)) {
    .fetch_asset(.asset_name(entry$path), path)
  }
  actual <- digest::digest(file = path, algo = "sha256")
  if (!identical(actual, entry$sha256)) {
    unlink(path)
    cli::cli_abort(
      "SHA-256 mismatch for {.file {entry$path}}; the cached file was deleted.",
      call = rlang::caller_env()
    )
  }
  return(path)
}
