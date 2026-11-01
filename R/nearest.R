.normalize_name <- function(x) {
  return(stringi::stri_trans_general(x, "Latin-ASCII; Lower"))
}

.station_status <- c("operating", "broken", "closed")

.resolve_seat <- function(muni, state, call) {
  seats <- municipal_seats
  if (!is.na(state)) {
    seats <- seats[seats$abbrev_state == toupper(state), ]
  }
  if (is.numeric(muni) || grepl("^[0-9]{7}$", muni)) {
    if (!grepl("^[0-9]{7}$", as.character(muni))) {
      cli::cli_abort(
        "{.arg muni} must be a seven-digit IBGE code, not {.val {muni}}.",
        call = call
      )
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
    cli::cli_abort("Unknown {.arg muni}: {.val {muni}}.{hint}", call = call)
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
      "Ambiguous {.arg muni} {.val {muni}}. Matches: {paste(choices, collapse = ', ')}. Use {.arg state} or an IBGE code.",
      call = call
    )
  }
  return(matches)
}

#' Find stations nearest a municipality seat
#'
#' Ranks stations by great-circle (haversine) distance from each
#' municipality's seat.
#'
#' @param muni One or more seven-digit IBGE municipality codes or
#'   municipality names.
#' @param n Maximum number of stations to return per municipality.
#' @param max_km Optional maximum distance in kilometres.
#' @param state Optional two-letter state abbreviation for name matching.
#'   Either one value for every `muni` or one value per `muni`; use `NA` to
#'   skip an element.
#' @param status Optional station statuses to keep: any of `"operating"`,
#'   `"broken"` and `"closed"`. See [inmet_stations()] for their meaning.
#'   `NULL` keeps every station.
#' @return A tibble with the station metadata from [inmet_stations()],
#'   `distance_km`, and a first column `code_muni_query` with the IBGE code
#'   each row answers. Rows follow the order of `muni`, then distance.
#' @export
#' @examplesIf interactive()
#' inmet_nearest("Campinas", n = 3)
#' inmet_nearest(c(3550308, 3304557), status = "operating")
inmet_nearest <- function(
  muni,
  n = 1,
  max_km = NULL,
  state = NULL,
  status = NULL
) {
  call <- sys.call()
  if (
    !length(muni) ||
      anyNA(muni) ||
      !is.character(muni) && !is.numeric(muni)
  ) {
    cli::cli_abort(
      "{.arg muni} must contain IBGE codes or municipality names.",
      call = call
    )
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
    cli::cli_abort("{.arg n} must be a positive whole number.", call = call)
  }
  if (
    !is.null(max_km) &&
      (!is.numeric(max_km) ||
        length(max_km) != 1L ||
        is.na(max_km) ||
        max_km < 0)
  ) {
    cli::cli_abort(
      "{.arg max_km} must be a nonnegative number or `NULL`.",
      call = call
    )
  }
  if (is.null(state)) {
    state <- NA_character_
  }
  if (
    !is.character(state) ||
      !length(state) %in% c(1L, length(muni)) ||
      !all(is.na(state) | grepl("^[A-Za-z]{2}$", state))
  ) {
    cli::cli_abort(
      "{.arg state} must be `NULL` or two-letter state abbreviations, one for all of {.arg muni} or one per element.",
      call = call
    )
  }
  if (
    !is.null(status) &&
      (!is.character(status) ||
        !length(status) ||
        !all(status %in% .station_status))
  ) {
    cli::cli_abort(
      "{.arg status} must be `NULL` or any of {.val {(.station_status)}}.",
      call = call
    )
  }
  state <- rep_len(state, length(muni))
  seats <- lapply(seq_along(muni), function(i) {
    return(.resolve_seat(muni[[i]], state[[i]], call = call))
  })
  stations <- inmet_stations()
  if (!is.null(status)) {
    stations <- stations[stations$status %in% status, ]
  }
  results <- lapply(seats, function(seat) {
    lat1 <- seat$lat * pi / 180
    lat2 <- stations$lat * pi / 180
    delta_lat <- lat2 - lat1
    delta_lon <- (stations$lon - seat$lon) * pi / 180
    haversine <- sin(delta_lat / 2)^2 +
      cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
    nearest <- stations
    nearest$distance_km <- 6371.0088 * 2 * asin(pmin(1, sqrt(haversine)))
    nearest <- nearest[!is.na(nearest$distance_km), ]
    if (!is.null(max_km)) {
      nearest <- nearest[nearest$distance_km <= max_km, ]
    }
    nearest <- nearest[order(nearest$distance_km, nearest$code_station), ]
    nearest <- utils::head(nearest, as.integer(n))
    return(tibble::add_column(
      nearest,
      code_muni_query = rep(seat$code_muni, nrow(nearest)),
      .before = 1L
    ))
  })
  return(dplyr::bind_rows(results))
}
