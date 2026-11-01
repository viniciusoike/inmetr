# Find stations nearest a municipality seat

Ranks stations by great-circle (haversine) distance from each
municipality's seat.

## Usage

``` r
inmet_nearest(muni, n = 1, max_km = NULL, state = NULL, status = NULL)
```

## Arguments

- muni:

  One or more seven-digit IBGE municipality codes or municipality names.

- n:

  Maximum number of stations to return per municipality.

- max_km:

  Optional maximum distance in kilometres.

- state:

  Optional two-letter state abbreviation for name matching. Either one
  value for every `muni` or one value per `muni`; use `NA` to skip an
  element.

- status:

  Optional station statuses to keep: any of `"operating"`, `"broken"`
  and `"closed"`. See
  [`inmet_stations()`](https://viniciusoike.github.io/inmetr/reference/inmet_stations.md)
  for their meaning. `NULL` keeps every station.

## Value

A tibble with the station metadata from
[`inmet_stations()`](https://viniciusoike.github.io/inmetr/reference/inmet_stations.md),
`distance_km`, and a first column `code_muni_query` with the IBGE code
each row answers. Rows follow the order of `muni`, then distance.

## Examples

``` r
if (FALSE) { # interactive()
inmet_nearest("Campinas", n = 3)
inmet_nearest(c(3550308, 3304557), status = "operating")
}
```
