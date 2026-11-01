# Rebuild internal municipality-seat coordinates from geobr's 2022 seats.
read_seats <- function(year) {
  seats <- geobr::read_municipal_seat(
    year = year,
    showProgress = FALSE,
    verbose = FALSE
  )
  coords <- sf::st_coordinates(seats)
  return(data.frame(
    code_muni = as.integer(seats$code_muni),
    name_muni = seats$name_muni,
    abbrev_state = seats$abbrev_state,
    lat = coords[, "Y"],
    lon = coords[, "X"]
  ))
}

municipal_seats <- read_seats(2022)

# geobr's 2022 seat for these municipalities lies more than 5 km from both its
# 2010 seat and the kelvins/municipios-brasileiros coordinates, while the 2010
# seat is less than half as far from the latter. Use the 2010 seat for them
# (issue #4; Porto Velho's 2022 seat is 99 km south of the city).
# Selected on 2026-11-01 with geobr 2010 and 2022 seats and kelvins commit
# 503e2f70bbf1b4b7ec0b1f68b09086ccc38fe861 (csv/municipios.csv). Rule: 2010 and
# 2022 seats more than 5 km apart, 2022 seat more than 5 km from kelvins, and
# the 2010 seat less than half as far from kelvins as the 2022 seat.
use_2010 <- c(
  1100072L, # Corumbiara/RO
  1100130L, # Machadinho d'Oeste/RO
  1100205L, # Porto Velho/RO
  1100288L, # Rolim de Moura/RO
  1100908L, # Castanheiras/RO
  1101401L, # Monte Negro/RO
  1101476L, # Primavera de Rondônia/RO
  1101484L, # São Felipe d'Oeste/RO
  1300706L, # Boca do Acre/AM
  1302603L, # Manaus/AM
  1302702L, # Manicoré/AM
  2107506L, # Paço do Lumiar/MA
  2515401L, # São Vicente do Seridó/PB
  3541000L # Praia Grande/SP
)
seats_2010 <- read_seats(2010)
seats_2010 <- seats_2010[seats_2010$code_muni %in% use_2010, ]
stopifnot(setequal(seats_2010$code_muni, use_2010))
rows <- match(seats_2010$code_muni, municipal_seats$code_muni)
municipal_seats[rows, c("lat", "lon")] <- seats_2010[, c("lat", "lon")]

stopifnot(
  nrow(municipal_seats) == 5570L,
  !anyNA(municipal_seats[, c("code_muni", "lat", "lon")])
)
save(municipal_seats, file = here::here("R", "sysdata.rda"), compress = "xz")
