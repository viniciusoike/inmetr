# Read INMET observations lazily

Downloads the selected yearly Parquet files to the cache once and
returns a lazy table. Filter and summarise it with dplyr, then call
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to bring the result into R.

## Usage

``` r
read_inmet(table = c("daily", "hourly"), years = NULL)
```

## Arguments

- table:

  Either `"daily"` or `"hourly"`.

- years:

  Years to download; `NULL` selects all available years.

## Value

A lazy dbplyr table backed by DuckDB.

## Details

Values are INMET's as published, except that `-9999` and blanks become
`NA`. The release applies no outlier filter or coverage threshold. Its
last year is usually partial: the `2026-10-24` release ends on 28
February 2025.

## Hourly table

One row per station and hour in INMET's files, including hours where
every measurement is missing.

|  |  |
|----|----|
| Column | Meaning |
| `code_station` | INMET station code |
| `timestamp_utc` | Observation time, UTC |
| `date_local`, `month`, `day`, `hour_local` | Local date and its parts, at the station's fixed standard UTC offset (no daylight saving) |
| `year` | UTC year; `years` selects on it |
| `precip_mm` | Precipitation in the hour, mm |
| `pressure_station_hpa` | Station pressure, hPa |
| `pressure_max_hpa`, `pressure_min_hpa` | Pressure extremes in the previous hour, hPa |
| `radiation_kj_m2` | Global radiation, kJ/m² |
| `temp_dry_bulb_c` | Dry-bulb air temperature, °C |
| `temp_dew_point_c` | Dew-point temperature, °C |
| `temp_max_c`, `temp_min_c` | Air-temperature extremes in the previous hour, °C |
| `temp_dew_point_max_c`, `temp_dew_point_min_c` | Dew-point extremes in the previous hour, °C |
| `rel_humidity_pct` | Relative humidity, % |
| `rel_humidity_max_pct`, `rel_humidity_min_pct` | Relative-humidity extremes in the previous hour, % |
| `wind_direction_deg` | Wind direction, degrees |
| `wind_speed_ms` | Wind speed, m/s |
| `wind_gust_ms` | Maximum gust in the previous hour, m/s |
| `code_muni`, `name_muni`, `abbrev_state` | Station's IBGE municipality code, name, and state |

## Daily table

The pipeline aggregates hourly rows by station and `date_local`. Every
available hour contributes, and an aggregate is `NA` only when all of
its hourly inputs are missing, so a daily sum is never zero for lack of
data.

|  |  |
|----|----|
| Column | Meaning |
| `code_station` | INMET station code |
| `date` | Local date |
| `year`, `month`, `day` | Parts of the local date; `years` selects on `year` |
| `day_of_year` | Day of the year, 1–366 |
| `week`, `weekday` | ISO week (1–53) and ISO weekday (Monday = 1) |
| `n_hours` | Hours with at least one non-missing measurement, 0–24 |
| `temp_mean_c` | Mean dry-bulb temperature, °C |
| `temp_min_c`, `temp_max_c` | Lowest and highest hourly air-temperature extreme, °C; see below |
| `temp_dew_point_mean_c` | Mean dew-point temperature, °C |
| `rel_humidity_mean_pct` | Mean relative humidity, % |
| `rel_humidity_min_pct`, `rel_humidity_max_pct` | Lowest and highest hourly relative-humidity extreme, %; see below |
| `precip_total_mm` | Total precipitation, mm |
| `radiation_total_kj_m2` | Total global radiation, kJ/m² |
| `pressure_station_mean_hpa` | Mean station pressure, hPa |
| `wind_speed_mean_ms` | Mean wind speed, m/s |
| `wind_gust_max_ms` | Highest gust, m/s |
| `code_muni`, `name_muni`, `abbrev_state` | Station's IBGE municipality code, name, and state |

Three features of the daily table need care.

- **Empty days.** INMET's files list hours with no measurements, so the
  daily table has rows with `n_hours == 0` and every measurement `NA`.
  Counting rows per station-year therefore counts calendar days, not
  days with data. Filter on `n_hours` first.

- **`n_hours` counts hours with any measurement.** A day can have
  `n_hours == 24` while one variable is missing all day. Coverage rules
  for a single variable need the hourly table; the example below counts
  hours with `temp_max_c`.

- **Extremes fall back to the hourly reading.** For each hour,
  `temp_max_c` uses the hourly maximum when present and
  `temp_dry_bulb_c` otherwise. `temp_min_c`, `rel_humidity_max_pct` and
  `rel_humidity_min_pct` work the same way. When the extreme field is
  missing for long spells, the daily extremes come from the hourly
  reading alone.

## Known data problems

The release keeps INMET's values, including some implausible
temperatures that `n_hours` filters do not catch.

|  |  |  |
|----|----|----|
| Station | Years | Problem |
| A201 Belém/PA | 2005–2006 | The 99th percentile of hourly `temp_max_c` minus `temp_dry_bulb_c` is 5.8 °C, against 3.9 °C across all stations, and daily maxima reach 44–45 °C |
| A303 Maceió/AL | 2004–2005 | 60 and 92 days with `temp_max_c` of 35 °C or more, against at most 5 in any later year |
| A409 Aracaju/SE | 2021 | Hourly `temp_max_c` missing on 130 days; daily maxima reach 35 °C or more on 73 days, 70 of them from the dry-bulb fallback, against at most 3 in other years |

No temperature in the release exceeds 45.0 °C, and values do not bunch
at 45.0 °C. Treat readings near 45 °C with suspicion.

## Examples

``` r
if (FALSE) { # interactive()
daily <- read_inmet("daily", 2024)
dplyr::collect(dplyr::filter(daily, .data$code_muni == 3550308L))

# Hours per day with a measured temp_max_c, to drop days that rely on the
# dry-bulb fallback
read_inmet("hourly", 2021) |>
  dplyr::filter(.data$code_station == "A409") |>
  dplyr::summarise(
    n_hours_temp_max = sum(as.integer(!is.na(.data$temp_max_c)), na.rm = TRUE),
    .by = c("code_station", "date_local")
  ) |>
  dplyr::collect()
}
```
