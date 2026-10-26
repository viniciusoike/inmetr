.connection <- new.env(parent = emptyenv())

.get_connection <- function() {
  if (is.null(.connection$con) || !DBI::dbIsValid(.connection$con)) {
    .connection$con <- DBI::dbConnect(
      duckdb::duckdb(shared_home = FALSE),
      dbdir = ":memory:"
    )
  }
  return(.connection$con)
}

.onUnload <- function(libpath) {
  if (!is.null(.connection$con) && DBI::dbIsValid(.connection$con)) {
    DBI::dbDisconnect(.connection$con, shutdown = TRUE)
  }
  return(invisible(NULL))
}

.read_sql <- function(paths, schema, call = rlang::caller_env()) {
  con <- .get_connection()
  quoted <- vapply(
    paths,
    function(path) {
      return(as.character(DBI::dbQuoteString(con, normalizePath(path))))
    },
    character(1)
  )
  expected_names <- vapply(schema, `[[`, character(1), "name")
  hive_types <- if ("year" %in% expected_names) {
    ", hive_types = {'year': INTEGER}"
  } else {
    ""
  }
  sql <- sprintf(
    "SELECT * FROM read_parquet([%s], hive_partitioning = true%s)",
    paste(quoted, collapse = ", "),
    hive_types
  )
  actual <- DBI::dbGetQuery(con, paste("DESCRIBE", sql))
  expected_types <- vapply(schema, `[[`, character(1), "type")
  if (
    !identical(actual$column_name, expected_names) ||
      !identical(actual$column_type, expected_types)
  ) {
    cli::cli_abort(
      "Parquet columns or types do not match the data manifest.",
      call = call
    )
  }
  return(sql)
}

#' Read INMET observations lazily
#' @param table Either `"daily"` or `"hourly"`.
#' @param years Years to download; `NULL` selects all available years.
#' @return A lazy dbplyr table backed by DuckDB.
#' @export
#' @examplesIf interactive()
#' daily <- read_inmet("daily", 2024)
#' dplyr::collect(dplyr::filter(daily, .data$code_muni == 3550308L))
read_inmet <- function(table = c("daily", "hourly"), years = NULL) {
  call <- sys.call()
  table <- rlang::arg_match(table)
  manifest <- .manifest(call = call)
  entries <- manifest$tables[[table]]$files
  available <- as.integer(vapply(
    entries,
    function(entry) return(sub(".*year=([0-9]{4})/.*", "\\1", entry$path)),
    character(1)
  ))
  if (is.null(years)) {
    size <- sum(vapply(entries, `[[`, numeric(1), "size")) / 1024^2
    cli::cli_inform(
      "Downloading all {table} years may require {round(size, 1)} MB."
    )
    years <- available
  }
  if (
    !is.numeric(years) ||
      !length(years) ||
      anyNA(years) ||
      any(!is.finite(years)) ||
      any(years != floor(years)) ||
      any(!years %in% available)
  ) {
    cli::cli_abort(
      "{.arg years} must contain available years: {paste(available, collapse = ', ')}.",
      call = call
    )
  }
  selected <- entries[available %in% unique(as.integer(years))]
  paths <- vapply(selected, .ensure_file, character(1), call = call)
  sql <- .read_sql(paths, manifest$tables[[table]]$schema, call = call)
  return(dplyr::tbl(.get_connection(), dbplyr::sql(sql)))
}

#' Read INMET station metadata
#' @return A tibble of station metadata.
#' @export
#' @examplesIf interactive()
#' inmet_stations()
inmet_stations <- function() {
  call <- sys.call()
  manifest <- .manifest(call = call)
  entry <- manifest$tables$stations$files[[1]]
  path <- .ensure_file(entry, call = call)
  sql <- .read_sql(path, manifest$tables$stations$schema, call = call)
  return(tibble::as_tibble(DBI::dbGetQuery(.get_connection(), sql)))
}
