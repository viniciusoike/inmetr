# Read INMET station metadata

Read INMET station metadata

## Usage

``` r
inmet_stations()
```

## Value

A tibble of station metadata.

## Station status

`status` records each station's state in the INMET station catalog used
to build the release:

- `"operating"`: listed as operating (*Operante*).

- `"broken"`: listed as faulty (*Pane*). The station may have stopped
  reporting recently or may still report with gaps, but its earlier
  observations are kept. Check `last_obs` and the coverage of the
  variables you need before dropping it.

- `"closed"`: absent from the catalog, though the historical files hold
  observations for it. `last_obs` gives its last observation in the
  release.

## Examples

``` r
if (FALSE) { # interactive()
inmet_stations()
}
```
