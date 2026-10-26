inmetr provides verified INMET automatic weather station data for lazy R queries and nearby-station lookup.

Install from GitHub:

```r
remotes::install_github("viniciusoike/inmetr")
```

Find a station near São Paulo and collect its daily observations for 2024:

```r
library(inmetr)

stations <- inmet_nearest(3550308, n = 1)
daily <- read_inmet("daily", years = 2024)
city_daily <- daily |>
  dplyr::filter(.data$code_station %in% stations$code_station) |>
  dplyr::collect()
```

The data release is downloaded on first use into the user cache. Downloaded files are checked against their SHA-256 hashes. Later reads check file size; use `inmet_cache_clear()` to recover from suspected cache corruption.
