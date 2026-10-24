local_fixture <- function() {
  source <- testthat::test_path("fixtures")
  assets <- withr::local_tempdir(.local_envir = parent.frame())
  file.copy(file.path(source, "manifest.json"), assets)
  file.copy(file.path(source, "stations.parquet"), assets)
  for (table in c("daily", "hourly")) {
    for (year in c(2023L, 2024L)) {
      from <- file.path(source, table, paste0("year=", year), "part-0.parquet")
      file.copy(from, file.path(assets, sprintf("%s-%d.parquet", table, year)))
    }
  }
  withr::local_options(
    list(
      inmetr.base_url = assets,
      inmetr.cache_dir = withr::local_tempdir(.local_envir = parent.frame())
    ),
    .local_envir = parent.frame()
  )
  return(assets)
}
