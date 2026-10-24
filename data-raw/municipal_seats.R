# Rebuild internal municipality-seat coordinates from geobr's 2022 seats.
seats <- geobr::read_municipal_seat(
  year = 2022,
  showProgress = FALSE,
  verbose = FALSE
)
coords <- sf::st_coordinates(seats)
municipal_seats <- data.frame(
  code_muni = as.integer(seats$code_muni),
  name_muni = seats$name_muni,
  abbrev_state = seats$abbrev_state,
  lat = coords[, "Y"],
  lon = coords[, "X"]
)
stopifnot(
  nrow(municipal_seats) == 5570L,
  !anyNA(municipal_seats[, c("code_muni", "lat", "lon")])
)
save(municipal_seats, file = here::here("R", "sysdata.rda"), compress = "xz")
