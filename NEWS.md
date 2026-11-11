# inmetr (development)

* The daily table gains one `n_hours_*` coverage column per aggregate
  (schema 3). Each counts the non-missing hourly inputs behind that day's
  value, 0–24, including the documented fallback for temperature and
  humidity extremes.
* Refreshed INMET data extend hourly and daily observations through
  31 October 2026, with official archive hashes, retrieval dates, declared
  spans, actual coverage, and revision audits recorded in the manifest and
  build reports.
* `inmetr` reads both schema 2 and schema 3 releases during the transition.

# inmetr 0.1.0

* `inmet_cache_dir()` and `inmet_cache_clear()` manage the versioned cache.
* `inmet_nearest()` finds stations from municipality-seat coordinates.
* `inmet_stations()` reads verified station metadata.
* `read_inmet()` queries verified hourly and daily Parquet data lazily.
