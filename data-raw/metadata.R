get_metadata <- function(path, include_path = FALSE, verbose = FALSE) {
  # Validate input
  if (!file.exists(path)) {
    cli::cli_alert_danger("File not found: {path}")
    return(NULL)
  }

  # Expected column names
  col_names <- get_colnames("metadata_fixed")

  # Extract filename without extension
  name_file <- tools::file_path_sans_ext(basename(path))

  # Split filename and create metadata
  tryCatch({
    # Use stringr for robust splitting
    metadata_parts <- stringr::str_split(name_file, "_", simplify = TRUE)

    # Validate number of parts
    if (length(metadata_parts) != length(col_names)) {
      cli::cli_alert_warning(
        "Filename does not match expected format: {name_file}"
      )
      return(NULL)
    }

    # Create data.table with proper column names
    metadata_station <- data.table::data.table(
      matrix(metadata_parts, nrow = 1, dimnames = list(NULL, col_names))
    )

    metadata_station[, drop_col := NULL]

    # Optionally include file path
    if (include_path) {
      metadata_station[, path := path]
    }

    # Optional verbose output
    if (verbose) {
      cli::cli_alert_info("Metadata extracted successfully:")
      print(metadata_station)
    }

    return(metadata_station)

  }, error = function(e) {
    cli::cli_alert_danger("Error processing metadata: {e$message}")
    return(NULL)
  })
}

source("R/utils.R")
library(stringr)
library(data.table)

path_files <- find_path_csv()
inmet_metadata <- parallel::mclapply(path_files, get_metadata, include_path = TRUE)
inmet_metadata <- data.table::rbindlist(inmet_metadata)

# station_names <- unique(inmet_metadata$name_station_full)

muni <- geobr::read_municipality(year = 2022)

cities_br <- muni$name_muni
cities_br <- stringi::stri_trans_general(cities_br, "latin-ascii")

check_pattern = paste(str_glue("({cities_br})"), collapse = "|")

inmet_metadata[, name_station := fix_station_names(name_station_full)]
inmet_metadata = inmet_metadata[order(code_station)]

inmet_metadata[, date_start := as.Date(date_start, format = "%d-%m-%Y")]
inmet_metadata[, date_end := as.Date(date_end, format = "%d-%m-%Y")]

inmet_metadata[, name_city := fifelse(str_detect(name_station, " -"),
                                      str_extract(name_station, "(.+)(?= -)"),
                                      name_station)]

inmet_metadata[, name_station := str_remove(name_station, ".+ - ")]
inmet_metadata[name_station == "Porto Alegre", name_station := "Jardim Botanico"]

# inmet_metadata[, name_station := fifelse(str_detect(name_station_full, " -"),
#                                          str_extract(name_station_full, "(?<=- )(.+)"),
#                                          name_station_full)]

# cities_inmet <- unique(inmet_metadata$name_city)
#
# dim_city <- data.table(name_city = cities_inmet)
#
# dim_city[!str_detect(name_city, check_pattern)]
#
# load_all()
# library(sf)
#
# dim_station <- st_as_sf(dim_station[, .(code_station, name_station, st_lng, st_lat)],
#                         coords = c("st_lng", "st_lat"),
#                         crs = 4674)
#
# dim_station
#
#
# # inmet_metadata[str_detect(name_city, check_pattern)]
#
#
#
# inmet_metadata[!str_detect(name_s)][["name_station"]]
#
#
# x_replacement <- c(
#   "ARQ\\." = "Arquipelago",
#   "PRES\\." = "Presidente",
#   "MAL\\." = "Marechal",
#   "S\\.J\\." = "Sao Jose",
#   "S\\. G\\." = "Sao Gabriel",
#   "COOPA-DF" = "COOPA/DF",
#   "ARCO VERDE" = "ARCOVERDE",
#   "BEBDOURO" = "BEBEDOURO",
#   "MARAMBAIA" = "RIO DE JANEIRO - MARAMBAIA"
# )
#
# y_replacement <- c(
#   "^Pef" = "PEF",
#   "^Dtcea" = "DTCEA",
#   "Df$" = "DF"
# )
#
# y = str_replace_all(y, x_replacement)
# y = str_to_title(y)
#
# y = str_replace(y, " Da ", " da ")
# y = str_replace(y, " Do ", " do ")
# y = str_replace(y, " Dos ", " dos ")
# y = str_replace(y, " De ", " de ")
#
# y = str_replace(y, "\\(", " - ")
# y = str_remove(y, "\\)$")
# y = str_replace(y, "(?<=[A-z])-(?=[A-z])", " - ")
# y = str_replace(y, "(?<=[A-z])-(?= )", " -")
# y = str_squish(y)
# y = str_replace_all(y, y_replacement)
#
# y = y[order(y)]
#
# y[!str_detect(y, check_pattern)]
#
# check[str_detect(check, "^Monte")]
#
# dplyr::filter(station_cities, str_detect(name_station, "MONTE VERDE"))
#
#
# # [1] "Abrolhos"
# # [2] "Aguas Emendadas"
# # [3] "Angra Dos Reis"
# # [4] "Arco Verde"
# # [5] "Bebdouro"
# # [6] "Brazlandia"
# # [7] "Calcanhar"
# # [8] "Camaratuba"
# # [9] "Campos"
# # [10] "Campos Dos Goytacazes"
# # [11] "Criosfera"
# # [12] "Delfino"
# # [13] "Ecologia Agricola"
# # [14] "Forte de Copacabana"
# # [15] "Forte Principe"
# # [16] "Guarda - Mor"
# # [17] "Ilha do Mel"
# # [18] "Luiz Eduardo Magalhaes"
# # [19] "Marambaia"
# # [20] "Mina do Palito"
# # [21] "Mocambinho"
# # [22] "Monte Verde"
# # [23] "Nhumirim"
# # [24] "Pampulha"
# # [25] "Paranoa - Coopa/DF"
# # [26] "Parati"
# # [27] "Parque Estadual Chandless"
# # [28] "PEF Yauarete"
# # [29] "Pico do Couto"
# # [30] "Preguicas"
# # [31] "Vila Militar"
# # [32] "Xerem"
#
# library(sf)
#
# pt_station <- st_as_sf(dim_station[, .(code_station, name_station, st_lng, st_lat)],
#                        coords = c("st_lng", "st_lat"),
#                        crs = 4674)
#
# br_cities <- st_transform(br_cities, crs = 4674)
# station_cities <- st_join(pt_station, br_cities)
#
# station_cities
#
# check[str_detect(check, "Angra")]
#
# # Abrolhos - arquipelago
# # Aguas Emendadas - estacao ecologica
#
#
#
#
# load_all()
#
# inmet_metadata[name_city == "PORTO ALEGRE"]
#
# dim_station[str_detect(name_station, "JARDIM BOTANICO")]
#
# x <- unique(inmet_metadata$code_station)
# y <- unique(dim_station$code_station)
#
# all(x %in% y)
#
# x[!x %in% y]
#
# d1 <- fread("data-raw/dim_stations/CatalogoEstaçõesAutomáticas.csv")
# d2 <- fread("data-raw/dim_stations/CatalogoEstaçõesConvencionais.csv")
#
# d1[CD_ESTACAO == "A301"]
#
# d1[str_detect(DC_NOME, "REC")]
# d2[str_detect(DC_NOME, "REC")]
#
# inmet_metadata[, status := fifelse(code_station %in% dim_station$code_station, "Active", "Inactive")]
#
# merge(inmet_metadata, dim_station)
#
# inmet_metadata[name_city == "SAO PAULO"]
#
# inmet_metadata[str_detect(name_station_full, "KENN")]
#
# unique(inmet_metadata$name_station_full)

usethis::use_data(inmet_metadata, internal = TRUE, overwrite = TRUE)
