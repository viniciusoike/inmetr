# INMET automatic-station build ----

dependencies <- c(
  "DBI",
  "duckdb",
  "sf",
  "geobr",
  "digest",
  "jsonlite",
  "cli",
  "testthat",
  "here",
  "data.table",
  "rlang"
)
missing_dependencies <- dependencies[
  !vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_dependencies)) {
  install_message <- paste0(
    "Install build dependencies before running: install.packages(c(",
    paste(sprintf("\"%s\"", missing_dependencies), collapse = ", "),
    "))"
  )
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_abort(install_message)
  }
  # cli may itself be missing, so the dependency check needs a base-R fallback.
  stop(install_message, call. = FALSE)
}

source(here::here("data-raw", "pipeline", "parse.R"))
source(here::here("data-raw", "pipeline", "inventory.R"))
source(here::here("data-raw", "pipeline", "stations.R"))
source(here::here("data-raw", "pipeline", "write.R"))
source(here::here("data-raw", "pipeline", "manifest.R"))

args <- commandArgs(trailingOnly = TRUE)
if (anyDuplicated(args) || any(!args %in% c("--sample", "--allow-dirty"))) {
  cli::cli_abort("Use Rscript data-raw/build.R [--sample] [--allow-dirty].")
}
sample <- "--sample" %in% args
build_commit <- get_build_commit(
  allow_dirty = sample || "--allow-dirty" %in% args
)
version <- format(Sys.Date(), "%Y-%m-%d")
output_root <- here::here("data-raw", "output")
output_dir <- file.path(output_root, if (sample) "sample" else version)
if (!sample) {
  # Repeated same-day builds get a suffix instead of replacing an earlier one.
  revision <- 1L
  while (
    dir.exists(output_dir) ||
      dir.exists(paste0(output_dir, ".tmp"))
  ) {
    revision <- revision + 1L
    version <- sprintf("%s-r%d", format(Sys.Date(), "%Y-%m-%d"), revision)
    output_dir <- file.path(output_root, version)
  }
}
if (dir.exists(output_dir)) {
  cli::cli_abort("Output already exists: {.file {output_dir}}.")
}
working_dir <- paste0(output_dir, ".tmp")
if (dir.exists(working_dir)) {
  cli::cli_abort("Incomplete build exists: {.file {working_dir}}.")
}
dir.create(working_dir, recursive = TRUE)
report_dir <- file.path(working_dir, "reports")
dir.create(report_dir)
inventory <- discover_input_files(here::here("data-raw", "data"))
files <- inventory$files$path
years <- inventory$files$year
if (sample) {
  keep <- years %in% c(2001L, 2020L)
  files <- files[keep]
  years <- years[keep]
}
if (!length(files)) {
  cli::cli_abort("No INMET automatic-station CSV files found.")
}
write_inventory_report(inventory, report_dir)
if (nrow(inventory$superseded)) {
  cli::cli_inform(
    "Using refreshed official inputs for {length(unique(years))} years; {nrow(inventory$superseded)} legacy year directories are superseded."
  )
}
cli::cli_inform(
  "Scanning {length(files)} station files across {length(unique(years))} years."
)
started <- Sys.time()
stations <- build_stations(files, report_dir)

stats <- list()
for (year in sort(unique(years))) {
  cli::cli_inform("Building hourly {year} ({sum(years == year)} files).")
  db_path <- file.path(working_dir, paste0("build-", year, ".duckdb"))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  DBI::dbExecute(con, "SET memory_limit = '4GB'")
  stats[[as.character(year)]] <- write_hourly_year(
    con,
    files[years == year],
    stations,
    year,
    working_dir,
    report_dir
  )
  DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(c(db_path, paste0(db_path, ".wal")))
}
con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = file.path(working_dir, "final.duckdb")
)
DBI::dbExecute(con, "SET memory_limit = '4GB'")
DBI::dbExecute(
  con,
  sprintf(
    "SET temp_directory = '%s'",
    gsub("'", "''", file.path(working_dir, "duckdb-temp"), fixed = TRUE)
  )
)
write_daily(con, working_dir)
hourly <- gsub(
  "'",
  "''",
  file.path(working_dir, "hourly", "year=*", "part-*.parquet"),
  fixed = TRUE
)
observations <- DBI::dbGetQuery(
  con,
  sprintf(
    "
  SELECT code_station, min(timestamp_utc) AS first_obs, max(timestamp_utc) AS last_obs
  FROM read_parquet('%s') GROUP BY code_station",
    hourly
  )
)
stations <- merge(stations, observations, by = "code_station", all.x = TRUE)
data.table::setcolorder(
  stations,
  c(
    "code_station",
    "name_station",
    "status",
    "lat",
    "lon",
    "altitude_m",
    "date_start_operation",
    "first_obs",
    "last_obs",
    "tz_offset_hours",
    "code_muni",
    "name_muni",
    "abbrev_state",
    "name_region",
    "muni_match"
  )
)
DBI::dbWriteTable(con, "stations", stations, overwrite = TRUE)
DBI::dbExecute(
  con,
  sprintf(
    "COPY stations TO '%s' (FORMAT parquet, COMPRESSION zstd)",
    gsub("'", "''", file.path(working_dir, "stations.parquet"), fixed = TRUE)
  )
)
data.table::fwrite(
  data.table::rbindlist(stats),
  file.path(report_dir, "input-by-year.csv")
)
validate_output(con, working_dir, report_dir)
coverage <- DBI::dbGetQuery(
  con,
  sprintf(
    "
    SELECT year, min(timestamp_utc) AS first_obs, max(timestamp_utc) AS last_obs,
      count(*) AS hours
    FROM read_parquet('%s') GROUP BY year ORDER BY year",
    hourly
  )
)
coverage$first_obs <- format(
  coverage$first_obs,
  "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)
coverage$last_obs <- format(
  coverage$last_obs,
  "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)
provenance_path <- here::here("data-raw", "data", "official", "provenance.json")
provenance <- if (file.exists(provenance_path)) {
  jsonlite::fromJSON(provenance_path, simplifyVector = FALSE)
} else {
  NULL
}
write_manifest(
  con,
  working_dir,
  version,
  build_commit,
  provenance = provenance,
  coverage = coverage
)
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(c(
  file.path(working_dir, "final.duckdb"),
  file.path(working_dir, "final.duckdb.wal")
))
unlink(file.path(working_dir, "duckdb-temp"), recursive = TRUE)
if (!file.rename(working_dir, output_dir)) {
  cli::cli_abort("Could not finalize build directory.")
}
cli::cli_alert_success(
  "Built {.file {output_dir}} in {round(as.numeric(difftime(Sys.time(), started, units = 'mins')), 1)} minutes."
)
