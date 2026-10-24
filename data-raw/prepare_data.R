load_all()

path_files <- find_path_csv()

station = c("PORTO ALEGRE")

bra <- inmet_import("Brasilia", include_metadata = FALSE, metadata_list = FALSE, metadata_condense = FALSE)

bra




dat <- inmet_import()

path_files <- inmet_metadata[name_city == "Porto Alegre" & name_station == "Jardim Botanico"]$path

poa <- lapply(path_files, inmet_import_csv)

poa <- data.table::rbindlist(poa)

data.table::fwrite(poa, "data/porto_alegre.csv")

inmet_import_csv(path_files[1], include_metadata = FALSE)
path_files[1]
inmet_read_csv(path_files[1])


dat

inmet_metadata[stringr::str_detect(name_city, "Porto Al")]



dat$inmet[order(data_ymd_hms)][order(code_station)]

inmet_metadata[stringr::str_detect(name_station, " - ")][order(name_station)]

inmet_metadata[, name_city := fifelse(stringr::str_detect(name_station, " -"),
                                      stringr::str_extract(name_station, "(.+)(?= -)"),
                                      name_station)]
inmet_metadata[, name_station_new := fifelse(stringr::str_detect(name_station, " -"),
                                         stringr::str_extract(name_station, "(?<=- )(.+)"),
                                         name_station)]

inmet_metadata



inmet_metadata[, name_city := ""]
inmet_metadata[stringr::str_detect(name_station, " -")
               ][, name_city := stringr::str_extract(name_station, "(.+)(?= -)")]

stringr::str_extract("SAO PAULO - INTERLAGOS", "(.+)(?= -)")
library(data.table)
dt <- data.table(x = "SAO PAULO - INTERLAGOS")

dt[, name_city := ""]
dt[stringr::str_detect(x, " -")
][, name_city := stringr::str_extract(x, "(.+)(?= -)")]

dt

inmet_metadata[stringr::str_detect(name_station, " -")]



id_variables <- c("source", "name_region", "name_state", "code_station", "name_station")
dat$metadata[, .(min_date = min(date_start), max_date = max(date_end)), by = id_variables]

path <- find_path_csv()
metadata_path <- data.table::rbindlist(lapply(path, get_metadata, include_path = TRUE))

sub_station <- metadata_path[code_station %in% station | name_station %in% station]
sub_path <- sub_station[["path"]]
ls <- parallel::mclapply(sub_path, inmet_import_csv, include_metadata = TRUE)
dat <- data.table::rbindlist(ls)

metadata_path <- lapply(path, get_metadata, include_path = TRUE)


dt <- dat$inmet

dt[, temp_max_num := as.numeric(temp_max)]

dt[is.na(temp_max_num)]


data <- inmet_import()


path = find_path_csv()

all_files <- lapply(path[1:100], inmet_import_csv, include_metadata = FALSE)

test <- inmet_import_csv(path[1], include_metadata = FALSE)

dput(sapply(test, class))

str(sapply(test, class))



dput(str(check_classes(test)))

check_classes <- function(dt) {

  target <- list(data_ymd = "Date", hora_utc = "character", precipitacao_total_mm = "numeric",
                 pressao_atm_hora_mB = "numeric", pressao_atm_max_mB = "numeric",
                 pressao_atm_min_mB = "numeric", radiacao_global_kj_m2 = "numeric",
                 temp_bulbo_hora = "numeric", temp_orvalho_hora = "numeric",
                 temp_max = "numeric", temp_min = "numeric", temp_orvalho_max = "numeric",
                 temp_orvalho_min = "numeric", umidade_rel_max = "numeric",
                 umidade_rel_min = "numeric", umidade_rel_hora = "numeric",
                 vento_direcao = "numeric", vento_rajada_max = "numeric",
                 vento_velocidade = "numeric", data_ymd_hms = c("POSIXct",
                                                                "POSIXt"), data_hora = "integer", data_dia = "integer", data_mes = "numeric")

  check <- sapply(dt, class)

  return(all.equal(target, check))

}

sapply(all_files, check_classes)

test <- inmet_import_csv(path[3])

unique(test$precipitacao_total_mm)

data.table::fread(path[3], skip = 8, na.strings = "-9999",
                  encoding = "Latin-1")

inmet_import_csv(path[3])

dat <- data.table::fread(
  path,
  skip = skip,
  colClasses = c("Date", "character", rep("numeric", 18)),
  na.strings = "-9999",
  encoding = "Latin-1"
)

path[3]

check_classes(test)

sapply(test, class)
load_all()
library(data.table)

inmet_metadata[, .N, by = c("code_station", "name_station")]
