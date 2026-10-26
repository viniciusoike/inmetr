.normalize_name <- function(x) {
  return(stringi::stri_trans_general(x, "Latin-ASCII; Lower"))
}

#' Find stations nearest a municipality seat
#' @param muni A seven-digit IBGE municipality code or municipality name.
#' @param n Maximum number of stations to return.
#' @param max_km Optional maximum distance in kilometres.
#' @param state Optional two-letter state abbreviation for name matching.
#' @return A tibble of station metadata with `distance_km`.
#' @export
#' @examplesIf interactive()
#' inmet_nearest("Campinas", n = 3)
inmet_nearest <- function(muni, n = 1, max_km = NULL, state = NULL) {
  if (
    length(muni) != 1L ||
      is.na(muni) ||
      !is.character(muni) && !is.numeric(muni)
  ) {
    cli::cli_abort("{.arg muni} must be one IBGE code or municipality name.")
  }
  if (
    !is.numeric(n) ||
      length(n) != 1L ||
      is.na(n) ||
      !is.finite(n) ||
      n < 1 ||
      n > .Machine$integer.max ||
      n != floor(n)
  ) {
    cli::cli_abort("{.arg n} must be a positive whole number.")
  }
  if (
    !is.null(max_km) &&
      (!is.numeric(max_km) ||
        length(max_km) != 1L ||
        is.na(max_km) ||
        max_km < 0)
  ) {
    cli::cli_abort("{.arg max_km} must be a nonnegative number or `NULL`.")
  }
  if (
    !is.null(state) &&
      (!is.character(state) ||
        length(state) != 1L ||
        is.na(state) ||
        !grepl("^[A-Za-z]{2}$", state))
  ) {
    cli::cli_abort(
      "{.arg state} must be a two-letter state abbreviation or `NULL`."
    )
  }
  seats <- municipal_seats
  if (!is.null(state)) {
    seats <- seats[seats$abbrev_state == toupper(state), ]
  }
  if (is.numeric(muni) || grepl("^[0-9]{7}$", muni)) {
    if (!grepl("^[0-9]{7}$", as.character(muni))) {
      cli::cli_abort("{.arg muni} must be a seven-digit IBGE code.")
    }
    matches <- seats[seats$code_muni == as.integer(muni), ]
  } else {
    matches <- seats[
      .normalize_name(seats$name_muni) == .normalize_name(muni),
    ]
  }
  if (!nrow(matches)) {
    suggestions <- agrep(
      .normalize_name(muni),
      .normalize_name(seats$name_muni),
      value = TRUE
    )
    hint <- if (length(suggestions)) {
      paste0(
        " Did you mean: ",
        paste(utils::head(unique(suggestions), 3L), collapse = ", "),
        "?"
      )
    } else {
      ""
    }
    cli::cli_abort("Unknown {.arg muni}: {.val {muni}}.{hint}")
  }
  if (nrow(matches) > 1L) {
    choices <- paste0(
      matches$name_muni,
      "/",
      matches$abbrev_state,
      " (",
      matches$code_muni,
      ")"
    )
    cli::cli_abort(
      "Ambiguous {.arg muni}. Matches: {paste(choices, collapse = ', ')}. Use {.arg state} or an IBGE code."
    )
  }
  stations <- inmet_stations()
  lat1 <- matches$lat * pi / 180
  lat2 <- stations$lat * pi / 180
  delta_lat <- lat2 - lat1
  delta_lon <- (stations$lon - matches$lon) * pi / 180
  haversine <- sin(delta_lat / 2)^2 +
    cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  stations$distance_km <- 6371.0088 * 2 * asin(pmin(1, sqrt(haversine)))
  stations <- stations[!is.na(stations$distance_km), ]
  if (!is.null(max_km)) {
    stations <- stations[stations$distance_km <= max_km, ]
  }
  stations <- stations[order(stations$distance_km, stations$code_station), ]
  return(tibble::as_tibble(utils::head(stations, as.integer(n))))
}
