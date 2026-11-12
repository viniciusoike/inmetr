# Acquire official INMET archives ----

# Downloads INMET's yearly historical archives into a cache, verifies their
# SHA-256 against the bytes that were extracted, records provenance, extracts
# each year into data-raw/data/official/inmet_YYYY, and audits the result
# against the legacy extracts in data-raw/data/zipped_files.
#
#   Rscript data-raw/acquire.R [--refresh] [YEARS...]
#
# Cache reuse never contacts the network and never rewrites retrieval metadata.
# `--refresh` downloads the selected years, validates every downloaded archive
# in hidden staging, and only then installs it with rollback. Revised prior
# archives, extracts, inventory, provenance and audit files move under
# data/official/history/ instead of being overwritten in place. Nothing is
# uploaded.

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

source(here::here("data-raw", "pipeline", "parse.R"))
source(here::here("data-raw", "pipeline", "archive.R"))
source(here::here("data-raw", "pipeline", "acquire.R"))

current_year <- as.integer(format(Sys.Date(), "%Y"))
settings <- parse_acquire_args(commandArgs(trailingOnly = TRUE), current_year)
acquire_years(
  settings$years,
  here::here("data-raw", "data"),
  refresh = settings$refresh
)
