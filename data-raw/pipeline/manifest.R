# Manifest and validation ----

get_build_commit <- function(repo_root = here::here(), allow_dirty = FALSE) {
  git <- function(args) {
    result <- suppressWarnings(system2(
      "git",
      c("-C", shQuote(repo_root), args),
      stdout = TRUE,
      stderr = TRUE
    ))
    if (!is.null(attr(result, "status"))) {
      cli::cli_abort(
        "Could not read Git build provenance in {.file {repo_root}}."
      )
    }
    return(result)
  }
  commit <- git(c("rev-parse", "HEAD"))
  if (length(commit) != 1L || !grepl("^[0-9a-f]{40}$", commit)) {
    cli::cli_abort("Git HEAD is not a commit SHA in {.file {repo_root}}.")
  }
  if (!allow_dirty) {
    dirty <- git(c(
      "status",
      "--porcelain=v1",
      "--untracked-files=all",
      "--",
      "data-raw/pipeline",
      "data-raw/build.R"
    ))
    if (length(dirty)) {
      cli::cli_abort(
        "Pipeline code has uncommitted changes; commit it or pass --allow-dirty."
      )
    }
  }
  return(commit)
}

expected_schemas <- list(
  hourly = c(
    code_station = "VARCHAR",
    timestamp_utc = "TIMESTAMP",
    date_local = "DATE",
    year = "INTEGER",
    month = "INTEGER",
    day = "INTEGER",
    hour_local = "INTEGER",
    stats::setNames(rep("DOUBLE", length(inmet_variables)), inmet_variables),
    code_muni = "INTEGER",
    name_muni = "VARCHAR",
    abbrev_state = "VARCHAR"
  ),
  daily = c(
    code_station = "VARCHAR",
    date = "DATE",
    year = "INTEGER",
    month = "INTEGER",
    day = "INTEGER",
    day_of_year = "INTEGER",
    week = "INTEGER",
    weekday = "INTEGER",
    n_hours = "INTEGER",
    temp_mean_c = "DOUBLE",
    temp_min_c = "DOUBLE",
    temp_max_c = "DOUBLE",
    temp_dew_point_mean_c = "DOUBLE",
    rel_humidity_mean_pct = "DOUBLE",
    rel_humidity_min_pct = "DOUBLE",
    rel_humidity_max_pct = "DOUBLE",
    precip_total_mm = "DOUBLE",
    radiation_total_kj_m2 = "DOUBLE",
    pressure_station_mean_hpa = "DOUBLE",
    wind_speed_mean_ms = "DOUBLE",
    wind_gust_max_ms = "DOUBLE",
    code_muni = "INTEGER",
    name_muni = "VARCHAR",
    abbrev_state = "VARCHAR"
  ),
  stations = c(
    code_station = "VARCHAR",
    name_station = "VARCHAR",
    status = "VARCHAR",
    lat = "DOUBLE",
    lon = "DOUBLE",
    altitude_m = "DOUBLE",
    date_start_operation = "DATE",
    first_obs = "TIMESTAMP",
    last_obs = "TIMESTAMP",
    tz_offset_hours = "INTEGER",
    code_muni = "INTEGER",
    name_muni = "VARCHAR",
    abbrev_state = "VARCHAR",
    name_region = "VARCHAR",
    muni_match = "VARCHAR"
  )
)

assert_parquet_schema <- function(con, path, table) {
  schema <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT name, duckdb_type FROM parquet_schema('%s') WHERE name != 'duckdb_schema' ORDER BY column_id",
      gsub("'", "''", path, fixed = TRUE)
    )
  )
  actual <- stats::setNames(schema$duckdb_type, schema$name)
  if (!identical(actual, expected_schemas[[table]])) {
    cli::cli_abort(
      "Parquet schema mismatch in {.file {path}} for {.val {table}}."
    )
  }
  return(invisible(NULL))
}

write_manifest <- function(con, output_dir, version, build_commit) {
  tables <- list()
  for (table in c("hourly", "daily", "stations")) {
    paths <- if (table == "stations") {
      file.path(output_dir, "stations.parquet")
    } else {
      list.files(
        file.path(output_dir, table),
        pattern = "\\.parquet$",
        recursive = TRUE,
        full.names = TRUE
      )
    }
    paths <- sort(paths)
    if (!length(paths)) {
      cli::cli_abort("No Parquet files for {.val {table}}.")
    }
    files <- lapply(paths, function(path) {
      assert_parquet_schema(con, path, table)
      normalized <- normalizePath(path, winslash = "/")
      relative <- substring(
        normalized,
        nchar(normalizePath(output_dir, winslash = "/")) + 2L
      )
      n <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT count(*) AS n FROM read_parquet('%s')",
          gsub("'", "''", path, fixed = TRUE)
        )
      )$n
      list(
        path = relative,
        size = unname(file.info(path)$size),
        sha256 = digest::digest(file = path, algo = "sha256"),
        row_count = unname(n)
      )
    })
    schema <- expected_schemas[[table]]
    tables[[table]] <- list(
      files = files,
      schema = lapply(seq_along(schema), function(i) {
        list(name = names(schema)[[i]], type = unname(schema[[i]]))
      })
    )
  }
  manifest <- list(
    version = version,
    schema_version = 2L,
    build_commit = build_commit,
    retrieved_at = NULL,
    source_url = "https://portal.inmet.gov.br/dadoshistoricos",
    tables = tables
  )
  jsonlite::write_json(
    manifest,
    file.path(output_dir, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  return(invisible(manifest))
}

validate_output <- function(con, output_dir, report_dir) {
  hourly <- gsub(
    "'",
    "''",
    file.path(output_dir, "hourly", "year=*", "part-*.parquet"),
    fixed = TRUE
  )
  daily <- gsub(
    "'",
    "''",
    file.path(output_dir, "daily", "year=*", "part-*.parquet"),
    fixed = TRUE
  )
  counts <- DBI::dbGetQuery(
    con,
    sprintf(
      "
    SELECT year, count(*) AS rows, count(DISTINCT code_station) AS stations
    FROM read_parquet('%s') GROUP BY year ORDER BY year",
      hourly
    )
  )
  data.table::fwrite(counts, file.path(report_dir, "hourly-by-year.csv"))
  daily_counts <- DBI::dbGetQuery(
    con,
    sprintf(
      "
    SELECT year, count(*) AS rows, min(n_hours) AS min_hours,
      quantile_cont(n_hours, 0.5) AS median_hours, max(n_hours) AS max_hours
    FROM read_parquet('%s') GROUP BY year ORDER BY year",
      daily
    )
  )
  data.table::fwrite(daily_counts, file.path(report_dir, "daily-by-year.csv"))
  hours <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT year, n_hours, count(*) AS days FROM read_parquet('%s') GROUP BY 1, 2 ORDER BY 1, 2",
      daily
    )
  )
  data.table::fwrite(hours, file.path(report_dir, "daily-n-hours.csv"))
  na_sql <- paste(
    sprintf(
      "avg(CAST(%s IS NULL AS DOUBLE)) AS %s",
      inmet_variables,
      paste0("na_", inmet_variables)
    ),
    collapse = ", "
  )
  missing <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT year, %s FROM read_parquet('%s') GROUP BY year ORDER BY year",
      na_sql,
      hourly
    )
  )
  data.table::fwrite(missing, file.path(report_dir, "missing-by-year.csv"))
  input <- data.table::fread(file.path(report_dir, "input-by-year.csv"))
  comparison <- merge(
    input,
    counts[, c("year", "rows")],
    by = "year",
    all = TRUE
  )
  if (
    nrow(comparison) != nrow(input) ||
      anyNA(comparison$rows) ||
      any(comparison$rows != comparison$rows_raw - comparison$duplicate_rows)
  ) {
    cli::cli_abort(
      "Hourly Parquet row counts do not reconcile with input rows and deduplication."
    )
  }
  invalid <- DBI::dbGetQuery(
    con,
    sprintf(
      "
    SELECT count(*) AS n FROM read_parquet('%s') WHERE n_hours < 0 OR n_hours > 24",
      daily
    )
  )$n
  if (invalid != 0L) {
    cli::cli_abort("Daily n_hours is outside 0-24.")
  }
  return(invisible(list(hourly = counts, daily = daily_counts)))
}
