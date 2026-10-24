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

# Municipality exceptions ----

municipality_exceptions <- data.table::data.table(
  code_station = c("A302", "C891"),
  code_muni = c(2605459L, NA_integer_),
  reason = c(
    "São Pedro e São Paulo archipelago is administered by Fernando de Noronha district",
    "Criosfera is in Antarctica, outside Brazil"
  )
)

assign_station_municipalities <- function(
  stations,
  municipalities,
  report_dir
) {
  stations <- data.table::copy(stations)
  points <- sf::st_as_sf(
    stations[, .(code_station, name_station, lat, lon)],
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
  stations[, code_muni := as.integer(joined$code_muni)]
  stations[, name_muni := joined$name_muni]
  stations[, abbrev_state := joined$abbrev_state]
  stations[, name_region := joined$name_region]
  stations[, muni_match := "polygon"]

  missing <- which(is.na(stations$code_muni))
  if (length(missing)) {
    nearest <- sf::st_nearest_feature(points[missing, ], municipalities)
    distance_km <- as.numeric(sf::st_distance(
      points[missing, ],
      municipalities[nearest, ],
      by_element = TRUE
    )) /
      1000
    exceptions <- match(
      stations$code_station[missing],
      municipality_exceptions$code_station
    )
    manual_exception <- exceptions[
      !is.na(exceptions) &
        !is.na(municipality_exceptions$code_muni[exceptions])
    ]
    if (length(manual_exception)) {
      manual <- missing[match(manual_exception, exceptions)]
      match_muni <- match(
        municipality_exceptions$code_muni[manual_exception],
        municipalities$code_muni
      )
      if (anyNA(match_muni)) {
        cli::cli_abort(
          "Municipality exception references an unknown code_muni."
        )
      }
      stations[
        manual,
        code_muni := as.integer(municipalities$code_muni[match_muni])
      ]
      stations[manual, name_muni := municipalities$name_muni[match_muni]]
      stations[manual, abbrev_state := municipalities$abbrev_state[match_muni]]
      stations[manual, name_region := municipalities$name_region[match_muni]]
      stations[manual, muni_match := "manual"]
    }
    stations[
      missing[
        !is.na(exceptions) &
          is.na(municipality_exceptions$code_muni[exceptions])
      ],
      muni_match := "none"
    ]
    report <- data.table::data.table(
      code_station = stations$code_station[missing],
      name_station = stations$name_station[missing],
      nearest_code_muni = as.integer(municipalities$code_muni[nearest]),
      nearest_name_muni = municipalities$name_muni[nearest],
      distance_km = distance_km,
      muni_match = ifelse(
        is.na(exceptions),
        NA_character_,
        stations$muni_match[missing]
      ),
      assigned_code_muni = stations$code_muni[missing],
      assigned_name_muni = stations$name_muni[missing]
    )
    data.table::fwrite(
      report,
      file.path(report_dir, "nearest-municipalities.csv")
    )
    unknown <- which(is.na(exceptions))
    if (length(unknown)) {
      i <- unknown[[1]]
      station <- stations[missing[[i]]]
      cli::cli_abort(
        "Station {station$code_station} ({station$name_station}) at ({station$lat}, {station$lon}) is outside every municipality polygon; nearest is {report$nearest_name_muni[[i]]} ({report$nearest_code_muni[[i]]}), {round(distance_km[[i]], 3)} km away. Review and add the station to the exception table."
      )
    }
  } else {
    data.table::fwrite(
      data.table::data.table(
        code_station = character(),
        name_station = character(),
        nearest_code_muni = integer(),
        nearest_name_muni = character(),
        distance_km = numeric(),
        muni_match = character(),
        assigned_code_muni = integer(),
        assigned_name_muni = character()
      ),
      file.path(report_dir, "nearest-municipalities.csv")
    )
  }
  return(stations)
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
  stations <- assign_station_municipalities(
    stations,
    municipalities,
    report_dir
  )
  stations[,
    tz_offset_hours := station_offset(
      abbrev_state,
      lon,
      lat,
      name_muni,
      muni_match == "manual"
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
