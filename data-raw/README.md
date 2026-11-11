# Build the INMET data

Install R dependencies:

```sh
Rscript -e 'install.packages(c("DBI", "duckdb", "sf", "geobr", "digest", "jsonlite", "cli", "testthat", "here", "data.table", "rlang"))'
```

Download and extract the current official archives:

```sh
Rscript data-raw/acquire.R              # 2000 to the current year
Rscript data-raw/acquire.R 2025 2026    # selected years
```

Archives are cached under `data-raw/data/cache/` and extracted separately to `data-raw/data/official/inmet_YYYY/`. `official/provenance.json` records the source URL, retrieval time, HTTP validators, archive size and SHA-256, declared CSV date span, and file count. `official/revisions.csv` and `revisions-YYYY-*.csv` audit refreshed archives against the legacy extracts in `data-raw/data/zipped_files/inmet_YYYY/`, which are left in place. Extraction never overwrites an existing directory; move one aside explicitly before re-extracting it.

```sh
Rscript data-raw/build.R --sample
Rscript data-raw/build.R
Rscript -e 'testthat::test_dir("data-raw/pipeline/tests")'
Rscript data-raw/publish.R data-raw/output/YYYY-MM-DD
```

Outputs go to `data-raw/output/sample/` or `data-raw/output/YYYY-MM-DD/`. A second build on the same day uses `YYYY-MM-DD-r2`, then `-r3`, instead of replacing an earlier output. The build prefers `official/inmet_YYYY/` over a legacy year and writes the choice to `reports/input-selection.csv`. Full builds require committed `data-raw/build.R`, `data-raw/pipeline/` and `data-raw/acquire.R` code; pass `--allow-dirty` only for an intentionally uncommitted run.

`publish.R` only stages verified flat assets locally and prints the `gh release create` command. It does not upload. After publication, update `.data_version` in `R/cache.R` in a separate commit; until then the package keeps downloading the published schema 2 release.
