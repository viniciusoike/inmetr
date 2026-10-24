# Build the INMET data

Install R dependencies:

```sh
Rscript -e 'install.packages(c("DBI", "duckdb", "sf", "geobr", "digest", "jsonlite", "cli", "testthat", "here", "data.table", "rlang"))'
```

Place the historical INMET CSVs under `data-raw/data/zipped_files/inmet_YYYY/`.

```sh
Rscript data-raw/build.R --sample
Rscript data-raw/build.R
Rscript -e 'testthat::test_dir("data-raw/pipeline/tests")'
```

Outputs go to `data-raw/output/sample/` or `data-raw/output/YYYY-MM-DD/`. Remove an existing output directory before rebuilding it. Full builds require committed `data-raw/build.R` and `data-raw/pipeline/` code; pass `--allow-dirty` only for an intentionally uncommitted run.
