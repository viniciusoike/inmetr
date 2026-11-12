# Input inventory ----

# Refreshed official archives live under data-raw/data/official/inmet_YYYY and
# supersede legacy extracts in data-raw/data/zipped_files/inmet_YYYY. Official
# years are selected only after their provenance, cached archive, extracted
# bytes and inventory evidence are verified. Legacy files stay in place so
# revisions can be audited, and legacy-only years remain supported.

year_directory_name <- function(path) {
  return(grepl("^inmet_[0-9]{4}(_.*)?$", basename(path)))
}

discover_legacy_files <- function(legacy_root) {
  if (!dir.exists(legacy_root)) {
    return(NULL)
  }
  year_dirs <- list.dirs(legacy_root, recursive = FALSE)
  year_dirs <- year_dirs[year_directory_name(year_dirs)]
  run <- data.table::rbindlist(lapply(year_dirs, function(directory) {
    paths <- scan_csv_files(directory)
    if (!length(paths)) {
      return(NULL)
    }
    return(data.table::data.table(
      path = paths,
      year = as.integer(sub(
        "^inmet_([0-9]{4}).*$",
        "\\1",
        basename(directory)
      )),
      source = "legacy",
      directory = directory
    ))
  }))
  return(run)
}

discover_official_files <- function(data_root) {
  official_root <- file.path(data_root, "official")
  if (!dir.exists(official_root)) {
    return(NULL)
  }
  year_dirs <- list.dirs(official_root, recursive = FALSE)
  year_dirs <- year_dirs[year_directory_name(year_dirs)]
  provenance <- read_provenance(official_root)
  if (!length(year_dirs)) {
    if (length(provenance)) {
      cli::cli_abort(c(
        "Provenance lists official year{?s} {.val {names(provenance)}} without an extracted directory.",
        "i" = "Restore the directory or run {.code Rscript data-raw/acquire.R --refresh}."
      ))
    }
    return(NULL)
  }
  years <- as.integer(sub("^inmet_([0-9]{4}).*$", "\\1", basename(year_dirs)))
  if (anyDuplicated(years)) {
    cli::cli_abort(
      "Official inputs repeat year{?s} {.val {years[duplicated(years)]}}."
    )
  }
  run <- data.table::rbindlist(lapply(seq_along(year_dirs), function(index) {
    directory <- year_dirs[[index]]
    year <- years[[index]]
    if (!length(scan_csv_files(directory))) {
      cli::cli_abort(c(
        "Official input {.val {year}} in {.file {directory}} has no CSV files.",
        "i" = "Remove the empty directory or run {.code Rscript data-raw/acquire.R --refresh {year}}."
      ))
    }
    return(data.table::data.table(
      path = verify_official_year(data_root, year, provenance),
      year = year,
      source = "official",
      directory = directory
    ))
  }))
  absent <- setdiff(names(provenance), as.character(years))
  if (length(absent)) {
    cli::cli_abort(c(
      "Provenance lists official year{?s} {.val {absent}} without an extracted directory.",
      "i" = "Restore the directory or run {.code Rscript data-raw/acquire.R --refresh {absent}}."
    ))
  }
  return(run)
}

discover_input_files <- function(
  data_root,
  min_year = 2000L,
  max_year = as.integer(format(Sys.Date(), "%Y")) + 1L
) {
  run <- data.table::rbindlist(Filter(
    Negate(is.null),
    list(
      discover_legacy_files(file.path(data_root, "zipped_files")),
      discover_official_files(data_root)
    )
  ))
  if (is.null(run) || !nrow(run)) {
    cli::cli_abort("No INMET automatic-station CSV files found.")
  }
  if (anyNA(run$year)) {
    cli::cli_abort("Could not read an input year from every INMET CSV.")
  }
  if (any(run$year < min_year | run$year > max_year)) {
    cli::cli_abort(
      "Input years must lie between {min_year} and {max_year}."
    )
  }
  refreshed <- unique(run[source == "official", .(year, directory)])
  superseded <- run[
    source == "legacy" & year %in% refreshed$year,
    .(n_files = .N),
    by = .(year, path = directory)
  ]
  selected <- run[
    source == "official" | !year %in% refreshed$year,
    .(path, year, source)
  ]
  data.table::setorder(selected, year, path)
  data.table::setorder(superseded, year, path)
  return(list(files = selected, superseded = superseded))
}

write_inventory_report <- function(inventory, report_dir) {
  selection <- inventory$files[,
    .(files = .N),
    by = .(year, source)
  ]
  data.table::fwrite(
    selection,
    file.path(report_dir, "input-selection.csv")
  )
  data.table::fwrite(
    inventory$superseded,
    file.path(report_dir, "superseded-inputs.csv")
  )
  return(invisible(NULL))
}
