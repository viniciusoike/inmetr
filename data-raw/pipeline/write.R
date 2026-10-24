# Parquet tables ----

write_hourly_year <- function(
  con,
  files,
  stations,
  year,
  output_dir,
  report_dir
) {
  DBI::dbExecute(con, "DROP TABLE IF EXISTS raw_hourly")
  DBI::dbWriteTable(
    con,
    "raw_hourly",
    read_inmet_csv(files[[1]]),
    temporary = FALSE
  )
  if (length(files) > 1L) {
    for (path in files[-1L]) {
      DBI::dbAppendTable(con, "raw_hourly", read_inmet_csv(path))
    }
  }
  DBI::dbWriteTable(
    con,
    "station_lookup",
    stations[, .(
      code_station,
      tz_offset_hours,
      code_muni,
      name_muni,
      abbrev_state
    )],
    overwrite = TRUE
  )
  duplicates <- DBI::dbGetQuery(
    con,
    "
    SELECT code_station, timestamp_utc, COUNT(*) AS n,
      COUNT(DISTINCT hash(precip_mm, pressure_station_hpa, pressure_max_hpa,
        pressure_min_hpa, radiation_kj_m2, temp_dry_bulb_c, temp_dew_point_c,
        temp_max_c, temp_min_c, temp_dew_point_max_c, temp_dew_point_min_c,
        rel_humidity_max_pct, rel_humidity_min_pct, rel_humidity_pct,
        wind_direction_deg, wind_gust_ms, wind_speed_ms)) AS variants
    FROM raw_hourly GROUP BY 1, 2 HAVING COUNT(*) > 1"
  )
  data.table::fwrite(
    duplicates,
    file.path(report_dir, paste0("duplicates-", year, ".csv"))
  )
  if (any(duplicates$variants > 1L)) {
    cli::cli_abort(
      "Conflicting station-hour duplicates in {year}; see duplicate report."
    )
  }
  variables <- paste(inmet_variables, collapse = ", ")
  hourly_dir <- file.path(output_dir, "hourly")
  dir.create(hourly_dir, recursive = TRUE, showWarnings = FALSE)
  query <- sprintf(
    "
    COPY (
      SELECT r.code_station, r.timestamp_utc,
        CAST(r.timestamp_utc + s.tz_offset_hours * INTERVAL '1 hour' AS DATE) AS date_local,
        CAST(year(r.timestamp_utc) AS INTEGER) AS year,
        CAST(month(r.timestamp_utc + s.tz_offset_hours * INTERVAL '1 hour') AS INTEGER) AS month,
        CAST(day(r.timestamp_utc + s.tz_offset_hours * INTERVAL '1 hour') AS INTEGER) AS day,
        CAST(hour(r.timestamp_utc + s.tz_offset_hours * INTERVAL '1 hour') AS INTEGER) AS hour_local,
        %s, s.code_muni, s.name_muni, s.abbrev_state
      FROM (SELECT * EXCLUDE (row_num) FROM (
        SELECT *, row_number() OVER (PARTITION BY code_station, timestamp_utc
          ORDER BY code_station) AS row_num FROM raw_hourly
      ) WHERE row_num = 1) r
      JOIN station_lookup s USING (code_station)
      ORDER BY r.code_station, r.timestamp_utc
    ) TO '%s' (FORMAT parquet, COMPRESSION zstd, PARTITION_BY (year),
      WRITE_PARTITION_COLUMNS true, APPEND true,
      FILENAME_PATTERN 'part-{uuid}')",
    variables,
    gsub("'", "''", hourly_dir, fixed = TRUE)
  )
  DBI::dbExecute(con, query)
  return(data.table::data.table(
    year = year,
    input_files = length(files),
    rows_raw = DBI::dbGetQuery(con, "SELECT count(*) AS n FROM raw_hourly")$n,
    duplicate_rows = if (nrow(duplicates)) sum(duplicates$n - 1L) else 0L,
    stations = DBI::dbGetQuery(
      con,
      "SELECT count(DISTINCT code_station) AS n FROM raw_hourly"
    )$n
  ))
}

write_daily <- function(con, output_dir) {
  hourly <- gsub(
    "'",
    "''",
    file.path(output_dir, "hourly", "year=*", "part-*.parquet"),
    fixed = TRUE
  )
  daily_dir <- file.path(output_dir, "daily")
  dir.create(daily_dir, recursive = TRUE, showWarnings = FALSE)
  # A missing aggregate stays NULL; SUM never turns an entirely missing day into zero.
  query <- sprintf(
    "
    COPY (
      WITH aggregate AS (
        SELECT code_station, date_local AS date,
          count(*) FILTER (WHERE %s) AS n_hours,
          avg(temp_dry_bulb_c) AS temp_mean_c,
          min(coalesce(temp_min_c, temp_dry_bulb_c)) AS temp_min_c,
          max(coalesce(temp_max_c, temp_dry_bulb_c)) AS temp_max_c,
          avg(temp_dew_point_c) AS temp_dew_point_mean_c,
          avg(rel_humidity_pct) AS rel_humidity_mean_pct,
          min(coalesce(rel_humidity_min_pct, rel_humidity_pct)) AS rel_humidity_min_pct,
          max(coalesce(rel_humidity_max_pct, rel_humidity_pct)) AS rel_humidity_max_pct,
          sum(precip_mm) AS precip_total_mm,
          sum(radiation_kj_m2) AS radiation_total_kj_m2,
          avg(pressure_station_hpa) AS pressure_station_mean_hpa,
          avg(wind_speed_ms) AS wind_speed_mean_ms,
          max(wind_gust_ms) AS wind_gust_max_ms,
          first(code_muni) AS code_muni, first(name_muni) AS name_muni,
          first(abbrev_state) AS abbrev_state
        FROM read_parquet('%s') GROUP BY 1, 2
      )
      SELECT code_station, date, CAST(year(date) AS INTEGER) AS year,
        CAST(month(date) AS INTEGER) AS month, CAST(day(date) AS INTEGER) AS day,
        CAST(dayofyear(date) AS INTEGER) AS day_of_year,
        CAST(week(date) AS INTEGER) AS week, CAST(isodow(date) AS INTEGER) AS weekday,
        n_hours, temp_mean_c, temp_min_c, temp_max_c, temp_dew_point_mean_c,
        rel_humidity_mean_pct, rel_humidity_min_pct, rel_humidity_max_pct,
        precip_total_mm, radiation_total_kj_m2, pressure_station_mean_hpa,
        wind_speed_mean_ms, wind_gust_max_ms, code_muni, name_muni, abbrev_state
      FROM aggregate ORDER BY code_station, date
    ) TO '%s' (FORMAT parquet, COMPRESSION zstd, PARTITION_BY (year),
      WRITE_PARTITION_COLUMNS true,
      FILENAME_PATTERN 'part-{i}')",
    paste(paste0(inmet_variables, " IS NOT NULL"), collapse = " OR "),
    hourly,
    gsub("'", "''", daily_dir, fixed = TRUE)
  )
  DBI::dbExecute(con, query)
  return(invisible(NULL))
}
