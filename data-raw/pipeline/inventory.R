# Input inventory ----

# Refreshed official archives live under data-raw/data/official/inmet_YYYY and
# supersede legacy extracts in data-raw/data/zipped_files/inmet_YYYY. The
# legacy files stay in place so revisions can be audited.
discover_input_files <- function(
  data_root,
  min_year = 2000L,
  max_year = as.integer(format(Sys.Date(), "%Y")) + 1L
) {
  roots <- c(
    legacy = file.path(data_root, "zipped_files"),
    official = file.path(data_root, "official")
  )
  run <- data.table::rbindlist(
    lapply(names(roots), function(source) {
      if (!dir.exists(roots[[source]])) {
        return(NULL)
      }
      year_dirs <- list.dirs(roots[[source]], recursive = FALSE)
      year_dirs <- year_dirs[
        grepl("^inmet_[0-9]{4}(_.*)?$", basename(year_dirs))
      ]
      paths <- as.character(unlist(
        lapply(
          year_dirs,
          list.files,
          pattern = "\\.[Cc][Ss][Vv]$",
          recursive = TRUE,
          full.names = TRUE
        ),
        use.names = FALSE
      ))
      paths <- paths[!startsWith(basename(paths), "._")]
      if (!length(paths)) {
        return(NULL)
      }
      relative <- substring(
        normalizePath(paths, winslash = "/"),
        nchar(normalizePath(roots[[source]], winslash = "/")) + 2L
      )
      year <- vapply(
        strsplit(relative, "/", fixed = TRUE),
        function(parts) {
          match <- grep("^inmet_[0-9]{4}(_.*)?$", parts, value = TRUE)
          if (length(match) != 1L) {
            return(NA_integer_)
          }
          return(as.integer(sub("^inmet_([0-9]{4}).*$", "\\1", match)))
        },
        integer(1)
      )
      data.table::data.table(
        path = paths,
        year = year,
        source = source,
        directory = dirname(paths)
      )
    }),
    use.names = TRUE
  )
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
