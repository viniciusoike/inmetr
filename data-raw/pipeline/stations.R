# Station reference ----

station_name <- function(x) {
  x <- trimws(gsub("[[:space:]]+", " ", x))
  return(tools::toTitleCase(tolower(x)))
}

parse_start_date <- function(x) {
  result <- as.Date(rep(NA_character_, length(x)))
  short <- grepl("^[0-9]{2}/[0-9]{2}/[0-9]{2}$", x)
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  result[short] <- as.Date(x[short], format = "%d/%m/%y")
  result[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  result[!short & !iso] <- as.Date(x[!short & !iso], format = "%d/%m/%Y")
  return(result)
}

read_station_catalog <- function() {
  path <- list.files(
    here::here("data-raw", "dim_stations"),
    pattern = "Automa.*\\.csv$",
    full.names = TRUE
  )
  if (length(path) != 1L) {
    cli::cli_abort("Expected one automatic station catalog.")
  }
  dat <- data.table::fread(path, sep = ";", dec = ",", encoding = "Latin-1")
  expected <- c(
    "DC_NOME",
    "SG_ESTADO",
    "CD_SITUACAO",
    "VL_LATITUDE",
    "VL_LONGITUDE",
    "VL_ALTITUDE",
    "DT_INICIO_OPERACAO",
    "CD_ESTACAO"
  )
  if (!identical(names(dat), expected)) {
    cli::cli_abort("Unknown automatic station catalog schema.")
  }
  data.table::setnames(
    dat,
    c(
      "name_station",
      "abbrev_state",
      "catalog_status",
      "lat",
      "lon",
      "altitude_m",
      "date_start_operation",
      "code_station"
    )
  )
  if (anyDuplicated(dat$code_station)) {
    cli::cli_abort("Duplicate station codes in catalog.")
  }
  return(dat)
}

station_offset <- function(abbrev_state, lon, lat, name_muni, offshore) {
  offset <- rep(-3L, length(lon))
  offset[abbrev_state %in% c("MT", "MS", "RO", "RR")] <- -4L
  offset[abbrev_state %in% "AC"] <- -5L
  # The 2013 legal boundary runs from Tabatinga to Porto Acre.
  boundary_lon <- -69.94 + (lat + 4.23) * (1.96 / -5.76)
  offset[abbrev_state %in% "AM" & lon <= boundary_lon] <- -5L
  offset[abbrev_state %in% "AM" & lon > boundary_lon] <- -4L
  offset[
    name_muni %in%
      "Fernando de Noronha" |
      (offshore &
        lon > -40 &
        lat < 1 &
        lat > -25 &
        abbrev_state %in% c("PE", "RN", "ES"))
  ] <- -2L
  return(offset)
}

check_unmatched_stations <- function(codes) {
  # C891 is the Antarctic station and has no Brazilian municipality.
  unexpected <- setdiff(codes, "C891")
  if (length(unexpected)) {
    cli::cli_abort(
      "Unmatched stations need a documented exception: {paste(unexpected, collapse = ', ')}."
    )
  }
  return(invisible(NULL))
}

build_stations <- function(files, report_dir) {
  metadata <- data.table::rbindlist(lapply(files, function(path) {
    x <- read_inmet_header(path)
    data.table::data.table(
      path = path,
      source_year = as.integer(sub(".*inmet_([0-9]{4}).*", "\\1", path)),
      code_station = x$code_station,
      name_station = x$name_station,
      abbrev_state = x$abbrev_state,
      name_region = x$name_region,
      lat = x$lat,
      lon = x$lon,
      altitude_m = x$altitude_m,
      date_start_operation = x$date_start_operation
    )
  }))
  data.table::setorder(metadata, code_station, -source_year, -path)
  latest <- unique(metadata, by = "code_station")
  catalog <- read_station_catalog()
  stations <- merge(
    latest[, .(
      code_station,
      name_region,
      csv_name = name_station,
      csv_state = abbrev_state,
      csv_lat = lat,
      csv_lon = lon,
      csv_altitude = altitude_m,
      csv_start = date_start_operation
    )],
    catalog,
    by = "code_station",
    all = TRUE
  )
  stations[,
    status := data.table::fifelse(
      is.na(catalog_status),
      "closed",
      data.table::fifelse(catalog_status == "Operante", "operating", "broken")
    )
  ]
  if (
    any(
      !is.na(stations$catalog_status) &
        !stations$catalog_status %in% c("Operante", "Pane")
    )
  ) {
    cli::cli_abort("Unknown catalog station status.")
  }
  stations[,
    name_station := station_name(data.table::fcoalesce(name_station, csv_name))
  ]
  stations[, abbrev_state := data.table::fcoalesce(abbrev_state, csv_state)]
  stations[, lat := data.table::fcoalesce(lat, csv_lat)]
  stations[, lon := data.table::fcoalesce(lon, csv_lon)]
  stations[, altitude_m := data.table::fcoalesce(altitude_m, csv_altitude)]
  stations[,
    date_start_operation := data.table::fcoalesce(
      date_start_operation,
      csv_start
    )
  ]
  stations[, date_start_operation := parse_start_date(date_start_operation)]
  if (anyNA(stations$lat) || anyNA(stations$lon)) {
    cli::cli_abort("Station coordinates are missing.")
  }

  municipalities <- geobr::read_municipality(
    year = 2022,
    simplified = TRUE,
    showProgress = FALSE
  )
  points <- sf::st_as_sf(
    stations,
    coords = c("lon", "lat"),
    crs = 4674,
    remove = FALSE
  )
  joined <- sf::st_join(
    points,
    municipalities[, c(
      "code_muni",
      "name_muni",
      "abbrev_state",
      "name_region"
    )],
    left = TRUE,
    join = sf::st_intersects
  )
  if (nrow(joined) != nrow(stations)) {
    cli::cli_abort("Ambiguous municipality spatial join.")
  }
  missing <- which(is.na(joined$code_muni))
  nearest_match <- integer()
  distance_km <- numeric()
  if (length(missing)) {
    nearest <- sf::st_nearest_feature(points[missing, ], municipalities)
    distance_km <- as.numeric(sf::st_distance(
      points[missing, ],
      municipalities[nearest, ],
      by_element = TRUE
    )) /
      1000
    nearest_match <- missing[distance_km <= 650]
    nearest <- nearest[distance_km <= 650]
    joined$code_muni[nearest_match] <- municipalities$code_muni[nearest]
    joined$name_muni[nearest_match] <- municipalities$name_muni[nearest]
    joined$abbrev_state.y[nearest_match] <- municipalities$abbrev_state[nearest]
    joined$name_region.y[nearest_match] <- municipalities$name_region[nearest]
  }
  unmatched <- which(is.na(joined$code_muni))
  check_unmatched_stations(stations$code_station[unmatched])
  data.table::fwrite(
    data.table::data.table(
      code_station = stations$code_station[missing],
      name_station = stations$name_station[missing],
      distance_km = distance_km,
      muni_match = ifelse(missing %in% nearest_match, "nearest", "none"),
      assigned_code_muni = joined$code_muni[missing],
      assigned_name_muni = joined$name_muni[missing]
    ),
    file.path(report_dir, "nearest-municipalities.csv")
  )
  stations[, code_muni := as.character(joined$code_muni)]
  stations[, name_muni := joined$name_muni]
  stations[, abbrev_state := joined$abbrev_state.y]
  stations[, name_region := joined$name_region.y]
  stations[, muni_match := "polygon"]
  stations[nearest_match, muni_match := "nearest"]
  stations[unmatched, muni_match := "none"]
  stations[,
    tz_offset_hours := station_offset(
      abbrev_state,
      lon,
      lat,
      name_muni,
      seq_len(.N) %in% nearest_match
    )
  ]
  stations[code_station == "C891", tz_offset_hours := -3L]
  stations[,
    c(
      "csv_name",
      "csv_state",
      "csv_lat",
      "csv_lon",
      "csv_altitude",
      "csv_start",
      "catalog_status"
    ) := NULL
  ]
  return(stations)
}
