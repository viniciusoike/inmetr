p <- "data-raw/data/zipped_files/inmet_2001/2001/INMET_CO_DF_A001_BRASILIA_01-01-2001_A_31-12-2001.CSV"

meta <- get_metadata_csv(p, col_names = "pt", include_path = TRUE)

class(meta)
str(meta)

meta <- get_metadata_csv(p, include_path = TRUE)

meta <- get_metadata_csv(p, include_path = TRUE, col_names = "pt")

meta <- get_metadata_csv(p, col_names = "es")


paths <- find_path_csv()

metadata_path_csv <- parallel::mclapply(paths, get_metadata_csv, include_path = TRUE)
meta <- data.table::rbindlist(metadata_path_csv)

# Fix st_date_start column

meta[, period := stringr::str_extract(basename(file_path), "(?<=_)[0-9]{2}.+_A_.+(?=\\.CSV)")]
meta[, c("period_start", "period_end") := data.table::tstrsplit(period, "_A_")]
date_cols <- c("period_start", "period_end")
meta[, (date_cols) := lapply(.SD, \(x) as.Date(x, format = "%d-%m-%Y")), .SDcols = date_cols]

meta[, st_date_start := zoo::na.locf(st_date_start), by = "code_station"]

meta[is.na(st_date_start), st_date_start := min(period_start), by = "code_station"]

meta[, period := NULL]

meta[, name_station := fix_station_names(name_station_full)]

stations <- unique(meta[, .(code_station, name_station)])
stations[, .N, c("code_station", "name_station")][N > 1]


skimr::skim(meta)
