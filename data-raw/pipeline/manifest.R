# Manifest and validation ----

write_manifest <- function(con, output_dir, version) {
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
    schema <- DBI::dbGetQuery(
      con,
      sprintf(
        "DESCRIBE SELECT * FROM read_parquet('%s')",
        gsub("'", "''", paths[[1]], fixed = TRUE)
      )
    )
    tables[[table]] <- list(
      files = files,
      schema = lapply(seq_len(nrow(schema)), function(i) {
        list(name = schema$column_name[[i]], type = schema$column_type[[i]])
      })
    )
  }
  manifest <- list(
    version = version,
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
