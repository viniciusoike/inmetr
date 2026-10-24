# INMET automatic-station build ----

source(here::here("data-raw", "pipeline", "parse.R"))
source(here::here("data-raw", "pipeline", "stations.R"))
source(here::here("data-raw", "pipeline", "write.R"))
source(here::here("data-raw", "pipeline", "manifest.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L || (length(args) == 1L && args != "--sample")) {
  cli::cli_abort("Use Rscript data-raw/build.R [--sample].")
}
sample <- identical(args, "--sample")
version <- format(Sys.Date(), "%Y-%m-%d")
output_root <- here::here("data-raw", "output")
output_dir <- file.path(output_root, if (sample) "sample" else version)
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
files <- list.files(
  here::here("data-raw", "data", "zipped_files"),
  pattern = "\\.[Cc][Ss][Vv]$",
  recursive = TRUE,
  full.names = TRUE
)
files <- files[!startsWith(basename(files), "._")]
if (sample) {
  files <- files[grepl("inmet_(2001|2020)/", files)]
}
if (!length(files)) {
  cli::cli_abort("No INMET automatic-station CSV files found.")
}
years <- as.integer(sub(".*inmet_([0-9]{4})/.*", "\\1", files))
if (anyNA(years) || any(years < 2000L | years > 2025L)) {
  cli::cli_abort("Unexpected input year directory.")
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
    "name_region"
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
write_manifest(con, working_dir, version)
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(c(
  file.path(working_dir, "final.duckdb"),
  file.path(working_dir, "final.duckdb.wal")
))
if (!file.rename(working_dir, output_dir)) {
  cli::cli_abort("Could not finalize build directory.")
}
cli::cli_alert_success(
  "Built {.file {output_dir}} in {round(as.numeric(difftime(Sys.time(), started, units = 'mins')), 1)} minutes."
)
