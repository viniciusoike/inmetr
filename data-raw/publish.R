# Stage an existing build as flat GitHub Release assets. This script never uploads.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  cli::cli_abort("Provide the build directory as the sole argument.")
}
build <- normalizePath(args[[1]], mustWork = TRUE)
manifest <- jsonlite::fromJSON(
  file.path(build, "manifest.json"),
  simplifyVector = FALSE
)
version <- manifest$version
if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", version)) {
  cli::cli_abort("Invalid build version in {.file manifest.json}.")
}
release <- here::here("data-raw", "release", version)
if (dir.exists(release)) {
  cli::cli_abort(
    "Release staging directory already exists at {.path {release}}."
  )
}
dir.create(release, recursive = TRUE)
paths <- file.path(release, "manifest.json")
if (!file.copy(file.path(build, "manifest.json"), paths)) {
  cli::cli_abort("Could not copy {.file manifest.json}.")
}
for (table in names(manifest$tables)) {
  for (entry in manifest$tables[[table]]$files) {
    source <- file.path(build, entry$path)
    if (
      !file.exists(source) ||
        !identical(digest::digest(file = source, algo = "sha256"), entry$sha256)
    ) {
      cli::cli_abort("Missing or invalid source file {.file {entry$path}}.")
    }
    asset <- if (table == "stations") {
      "stations.parquet"
    } else {
      year <- sub(".*year=([0-9]{4})/.*", "\\1", entry$path)
      sprintf("%s-%s.parquet", table, year)
    }
    target <- file.path(release, asset)
    if (
      !file.copy(source, target) ||
        !identical(digest::digest(file = target, algo = "sha256"), entry$sha256)
    ) {
      cli::cli_abort("Could not stage and verify {.file {asset}}.")
    }
    paths <- c(paths, target)
  }
}
command <- paste(
  c(
    "gh release create",
    paste0("data-", version),
    vapply(paths, shQuote, character(1)),
    "--title",
    shQuote(paste("INMET data", version))
  ),
  collapse = " "
)
cli::cli_inform("Staged {length(paths)} release assets in {.path {release}}.")
options(cli.width = 100000)
cli::cli_inform("Upload command (not run): {command}")
