# INMET automatic-station data dictionary

Source: [INMET historical observations](https://portal.inmet.gov.br/dadoshistoricos). Each file retains INMET's reported values except that `-9999` and blank measurements become missing. No outlier filtering or coverage threshold is applied. The CSVs contain 17 measured fields, plus date and hour.

## Hourly table

| Column | Unit or meaning |
| --- | --- |
| `code_station` | INMET WMO station code |
| `timestamp_utc` | Observation timestamp in UTC |
| `date_local` | Calendar date at fixed standard UTC offset |
| `year` | UTC year; partition key |
| `month`, `day`, `hour_local` | Local calendar month, day, and hour (0–23) |
| `precip_mm` | Hourly precipitation, mm |
| `pressure_station_hpa` | Station pressure, hPa (source mB) |
| `pressure_max_hpa`, `pressure_min_hpa` | Previous-hour pressure extremes, hPa |
| `radiation_kj_m2` | Global radiation, kJ/m² |
| `temp_dry_bulb_c` | Dry-bulb temperature, °C |
| `temp_dew_point_c` | Dew-point temperature, °C |
| `temp_max_c`, `temp_min_c` | Previous-hour air-temperature extremes, °C |
| `temp_dew_point_max_c`, `temp_dew_point_min_c` | Previous-hour dew-point extremes, °C |
| `rel_humidity_max_pct`, `rel_humidity_min_pct` | Previous-hour relative-humidity extremes, % |
| `rel_humidity_pct` | Hourly relative humidity, % |
| `wind_direction_deg` | Hourly wind direction, degrees |
| `wind_gust_ms` | Previous-hour maximum gust, m/s |
| `wind_speed_ms` | Hourly wind speed, m/s |
| `code_muni`, `name_muni`, `abbrev_state` | IBGE municipality code, name, and state; joined from station coordinates |

## Daily table

Daily groups use station and `date_local`. All available hours contribute. SQL aggregates return missing when every source value is missing, including sums.

| Column | Unit or rule |
| --- | --- |
| `code_station` | INMET WMO station code |
| `date` | Local calendar date |
| `year`, `month`, `day` | Local calendar parts; `year` is the partition key |
| `day_of_year` | Local ordinal day, 1–366 |
| `week` | ISO week number, 1–53 |
| `weekday` | ISO weekday, Monday = 1 |
| `n_hours` | Distinct hours with at least one non-missing measurement, 0–24 |
| `temp_mean_c` | Mean dry-bulb temperature, °C |
| `temp_min_c`, `temp_max_c` | Minimum/maximum of previous-hour extremes, falling back to dry-bulb per hour, °C |
| `temp_dew_point_mean_c` | Mean dew-point temperature, °C |
| `rel_humidity_mean_pct` | Mean hourly relative humidity, % |
| `rel_humidity_min_pct`, `rel_humidity_max_pct` | Minimum/maximum of previous-hour extremes, falling back to hourly relative humidity, % |
| `precip_total_mm` | Sum of hourly precipitation, mm |
| `radiation_total_kj_m2` | Sum of hourly global radiation, kJ/m² |
| `pressure_station_mean_hpa` | Mean station pressure, hPa |
| `wind_speed_mean_ms` | Mean hourly wind speed, m/s |
| `wind_gust_max_ms` | Maximum previous-hour gust, m/s |
| `code_muni`, `name_muni`, `abbrev_state` | Station's IBGE municipality code, name, and state |

## Stations table

| Column | Unit or meaning |
| --- | --- |
| `code_station` | INMET WMO station code |
| `name_station` | Catalog name, or latest CSV metadata name, in title case |
| `status` | `operating` for catalog Operante, `broken` for Pane, `closed` if absent from current catalog |
| `lat`, `lon` | Latitude and longitude in degrees (catalog preferred, latest CSV otherwise) |
| `altitude_m` | Elevation above sea level, m (catalog preferred) |
| `date_start_operation` | Catalog start date, or latest CSV metadata date, when parseable |
| `first_obs`, `last_obs` | Earliest and latest UTC observations in the included CSVs |
| `tz_offset_hours` | Fixed standard offset from UTC, hours |
| `code_muni`, `name_muni`, `abbrev_state`, `name_region` | IBGE 2022 municipality identity and region at the station's coordinates |

Municipalities come from a point-in-polygon join against `geobr::read_municipality(year = 2022)`. A station outside every polygon is assigned to its nearest municipality and listed in `reports/nearest-municipalities.csv`.

Local dates use fixed standard time with no daylight saving adjustment, including historical observations. The [Brazilian legal time-zone map](https://www2.camara.leg.br/legin/fed/lei/2013/lei-12876-30-outubro-2013-777336-publicacaooriginal-141612-pl.html) gives UTC−3 for most states and all Pará, UTC−4 for MT/MS/RO/RR and eastern Amazonas, and UTC−5 for Acre and western Amazonas. The Amazonas boundary is approximated by a straight line from Tabatinga to Porto Acre using station coordinates. Fernando de Noronha and points outside municipality polygons east of 40°W in PE/RN/ES use UTC−2. These are present-day standard offsets applied consistently to every year, not historical civil-time reconstruction.

`manifest.json` records the build version, source URL, schema, file sizes, SHA-256 digests, and row counts. `retrieved_at` is `null` because the downloaded files have no reliable retrieval timestamp.
