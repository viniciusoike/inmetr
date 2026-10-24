# Regenerate with Rscript tests/testthat/fixtures/regenerate.R
root <- "tests/testthat/fixtures"
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", shared_home = FALSE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
write_part <- function(dat, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbWriteTable(con, "fixture", dat, overwrite = TRUE)
  DBI::dbExecute(con, sprintf("COPY fixture TO '%s' (FORMAT parquet)", path))
  DBI::dbRemoveTable(con, "fixture")
}
for (year in c(2023L, 2024L)) {
  write_part(
    data.frame(
      code_station = "A701",
      date = as.Date(sprintf("%d-01-01", year)),
      year = year,
      code_muni = 3550308L,
      name_muni = "São Paulo",
      abbrev_state = "SP",
      temp_mean_c = 20 + year - 2023
    ),
    file.path(root, sprintf("daily/year=%d/part-0.parquet", year))
  )
  write_part(
    data.frame(
      code_station = "A701",
      timestamp_utc = as.POSIXct(
        sprintf("%d-01-01 12:00:00", year),
        tz = "UTC"
      ),
      year = year,
      code_muni = 3550308L,
      name_muni = "São Paulo",
      abbrev_state = "SP",
      temp_dry_bulb_c = 20 + year - 2023
    ),
    file.path(root, sprintf("hourly/year=%d/part-0.parquet", year))
  )
}
write_part(
  data.frame(
    code_station = c("A701", "A702", "A703"),
    name_station = c("Campinas", "São Paulo", "Santos"),
    status = c("operating", "broken", "closed"),
    lat = c(-22.902, -23.55, -23.96),
    lon = c(-47.0573, -46.63, -46.33),
    first_obs = as.POSIXct(rep("2020-01-01", 3), tz = "UTC"),
    last_obs = as.POSIXct(rep("2024-01-01", 3), tz = "UTC"),
    code_muni = c(3509502L, 3550308L, 3548500L),
    name_muni = c("Campinas", "São Paulo", "Santos"),
    abbrev_state = "SP",
    muni_match = "polygon"
  ),
  file.path(root, "stations.parquet")
)
manifest <- list(
  version = "2026-10-24",
  schema_version = 2L,
  build_commit = "fixture",
  retrieved_at = NULL,
  source_url = "fixture",
  tables = list()
)
for (table in c("daily", "hourly", "stations")) {
  files <- if (table == "stations") {
    "stations.parquet"
  } else {
    sprintf("%s/year=%d/part-0.parquet", table, c(2023L, 2024L))
  }
  schema <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT name, duckdb_type FROM parquet_schema('%s') WHERE name != 'duckdb_schema' ORDER BY column_id",
      file.path(root, files[[1]])
    )
  )
  manifest$tables[[table]] <- list(
    files = lapply(files, function(file) {
      list(
        path = file,
        size = unname(file.info(file.path(root, file))$size),
        sha256 = digest::digest(file = file.path(root, file), algo = "sha256"),
        row_count = as.integer(
          DBI::dbGetQuery(
            con,
            sprintf(
              "SELECT count(*) n FROM read_parquet('%s')",
              file.path(root, file)
            )
          )$n
        )
      )
    }),
    schema = lapply(seq_len(nrow(schema)), function(i) {
      list(name = schema$name[[i]], type = schema$duckdb_type[[i]])
    })
  )
}
jsonlite::write_json(
  manifest,
  file.path(root, "manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
