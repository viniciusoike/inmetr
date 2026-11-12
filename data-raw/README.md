# Build the INMET data

Install R dependencies:

```sh
Rscript -e 'install.packages(c("DBI", "duckdb", "sf", "geobr", "digest", "jsonlite", "cli", "testthat", "here", "data.table", "rlang"))'
```

Download and extract the current official archives:

```sh
Rscript data-raw/acquire.R              # 2000 to the current year
Rscript data-raw/acquire.R 2025 2026    # selected years
Rscript data-raw/acquire.R --refresh 2025 2026  # re-download and compare
```

Acquisition also needs the `curl` command-line tool: the downloader reads HTTP validators from the same response as the archive bytes.

Acquisition caches archives under `data-raw/data/cache/`, extracts each year into `data-raw/data/official/inmet_YYYY/`, and writes `official/files-YYYY.csv` with the size and SHA-256 of every extracted CSV. Before reusing a cached archive it re-expands the archive and compares member names and bytes with the extracted directory. A missing, extra, or altered CSV aborts without touching the existing inventory or provenance. Cache reuse never contacts the network and keeps the original retrieval time and HTTP validators. A fresh download records the `Last-Modified`, `ETag`, and `Content-Length` headers from the same response as the bytes, and `official/provenance.json` ties each year to the archive size, archive SHA-256, and inventory SHA-256.

`--refresh` downloads the selected years into a staging file. The download is validated completely in hidden staging first: ZIP central-directory listing, member CRC-32 and sizes, extraction, inventory bytes, prospective provenance, and revision audit parsing. Only then does the archive, extraction, inventory, audit files, and provenance move into place, with rollback if any install step fails. If the bytes match the provenance, the archive is kept or restored and only `revalidated_at_utc` changes. If they differ, the previous archive, extracted directory, file inventory, provenance, and revision reports move to `official/history/` (`archives/`, `extracts/`, `inventory/`, `revisions/<sha>/`), and the prior provenance entry is kept under `previous`. A missing or corrupt cached archive is recorded honestly as `archive_missing` instead of pointing at bytes that no longer exist. Nothing is overwritten in place.

The build uses an official year only when its provenance is complete, the cached archive matches the recorded size and SHA-256, `files-YYYY.csv` matches its recorded SHA-256, and every extracted CSV matches the archive member list and hashes. Empty, partial, mutated, or unprovenanced official years abort the build instead of silently falling back to legacy files. Years without an official directory remain legacy-only. `official/revisions.csv` and `revisions-YYYY-*.csv` audit refreshed archives against the legacy extracts in `data-raw/data/zipped_files/inmet_YYYY/`, which are left in place.

```sh
Rscript data-raw/build.R --sample
Rscript data-raw/build.R
Rscript -e 'testthat::test_dir("data-raw/pipeline/tests")'
Rscript data-raw/publish.R data-raw/output/YYYY-MM-DD
```

Outputs go to `data-raw/output/sample/` or `data-raw/output/YYYY-MM-DD/`. A second build on the same day uses `YYYY-MM-DD-r2`, then `-r3`, instead of replacing an earlier output. The build prefers `official/inmet_YYYY/` over a legacy year and writes the choice to `reports/input-selection.csv`. Full builds require committed `data-raw/build.R`, `data-raw/pipeline/` and `data-raw/acquire.R` code; pass `--allow-dirty` only for an intentionally uncommitted run.

`publish.R` only stages verified flat assets locally and prints the `gh release create` command. It does not upload. After publication, update `.data_version` in `R/cache.R` in a separate commit; until then the package keeps downloading the published schema 2 release.
