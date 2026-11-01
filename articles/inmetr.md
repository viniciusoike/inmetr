# Get started with inmetr

inmetr reads hourly and daily observations from INMET’s automatic
weather stations. The data run from 2000 to February 2025. Each table is
split into one Parquet file per year. inmetr downloads only the years
you ask for and queries them with DuckDB, so you filter and summarise
with dplyr before anything enters memory.

``` r

library(inmetr)
library(dplyr)
```

## Find a station

Most municipalities have no station of their own.
[`inmet_nearest()`](https://viniciusoike.github.io/inmetr/reference/inmet_nearest.md)
ranks stations by their distance to a municipality seat. It accepts an
IBGE code or a name, and accents and case don’t matter.

``` r

poa <- inmet_nearest("Porto Alegre", n = 3)
poa |>
  select(code_station, name_station, status, name_muni, distance_km)
#> # A tibble: 3 × 5
#>   code_station name_station                   status    name_muni    distance_km
#>   <chr>        <chr>                          <chr>     <chr>              <dbl>
#> 1 A801         Porto Alegre - Jardim Botanico operating Porto Alegre        5.91
#> 2 B807         Porto Alegre- Belem Novo       operating Porto Alegre       18.2 
#> 3 A884         Campo Bom                      operating Campo Bom          42.4
```

A name shared by several municipalities is an error. Pass `state` to
choose one:

``` r

inmet_nearest("Bom Jesus")
#> Error in `inmet_nearest()`:
#> ! Ambiguous `muni` "Bom Jesus". Matches: Bom Jesus/PI (2201903), Bom
#>   Jesus/RN (2401701), Bom Jesus/PB (2502201), Bom Jesus/SC (4202537), Bom
#>   Jesus/RS (4302303), Bom Jesus/GO (5203500). Use `state` or an IBGE code.
```

``` r

inmet_nearest("Bom Jesus", state = "RS") |>
  select(code_station, name_station, distance_km)
#> # A tibble: 1 × 3
#>   code_station name_station          distance_km
#>   <chr>        <chr>                       <dbl>
#> 1 A829         Sao Jose Dos Ausentes        37.9
```

[`inmet_stations()`](https://viniciusoike.github.io/inmetr/reference/inmet_stations.md)
returns the full station table. Closed stations stay in it with
`status = "closed"`, and `first_obs` and `last_obs` show what period
each station covers.

``` r

inmet_stations() |>
  count(status)
#> # A tibble: 3 × 2
#>   status        n
#>   <chr>     <int>
#> 1 broken       97
#> 2 closed       50
#> 3 operating   469
```

## Query daily data

[`read_inmet()`](https://viniciusoike.github.io/inmetr/reference/read_inmet.md)
returns a lazy table. The `years` argument controls which files are
downloaded, and dplyr verbs build a SQL query that DuckDB runs when you
call [`collect()`](https://dplyr.tidyverse.org/reference/compute.html).

``` r

daily <- read_inmet("daily", years = 2020:2024)

monthly <- daily |>
  filter(code_station == "A801", !is.na(temp_mean_c), n_hours >= 18) |>
  group_by(year, month) |>
  summarise(
    temp_mean_c = mean(temp_mean_c),
    n_days = n(),
    .groups = "drop"
  ) |>
  arrange(year, month) |>
  collect()
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.

monthly
#> # A tibble: 60 × 4
#>     year month temp_mean_c n_days
#>    <int> <int>       <dbl>  <dbl>
#>  1  2020     1        25.3     31
#>  2  2020     2        24.8     29
#>  3  2020     3        24.5     31
#>  4  2020     4        20.4     30
#>  5  2020     5        16.8     31
#>  6  2020     6        16.5     30
#>  7  2020     7        13.7     31
#>  8  2020     8        16.1     31
#>  9  2020     9        16.9     30
#> 10  2020    10        19.3     31
#> # ℹ 50 more rows
```

inmetr publishes INMET’s values without outlier filtering or coverage
thresholds, so choose a coverage rule that suits your analysis. The
query above keeps days with a mean temperature and at least 18 hours of
data, and `n_days` reports how many days each month retains. `n_hours`
counts hours with at least one valid measurement of any variable, so it
doesn’t guarantee 18 hours of temperature readings. For
variable-specific coverage, count non-missing values in the hourly
table.

The daily table aggregates the hourly one by local standard date. For
sub-daily questions, use `read_inmet("hourly", years = ...)` the same
way. Timestamps there are in UTC, and `date_local` and `hour_local` give
the station’s local standard time.

## Cache

Files go to
[`inmet_cache_dir()`](https://viniciusoike.github.io/inmetr/reference/inmet_cache_dir.md)
on first use. inmetr checks each downloaded Parquet file against the
SHA-256 hash listed in the release manifest and reuses cached files
after that.
[`inmet_cache_clear()`](https://viniciusoike.github.io/inmetr/reference/inmet_cache_clear.md)
deletes the cached release.
