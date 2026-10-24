#' Find Download Links for INMET Historical Data
#'
#' Scrapes download links for historical meteorological data from the INMET portal.
#' This function extracts hyperlink URLs from a specified webpage using an XPath selector.
#'
#' @param url Character string representing the URL of the INMET historical data portal.
#'   Defaults to the official INMET historical data page.
#' @param xpath Character string specifying the XPath selector to locate download links.
#'   Defaults to a selector targeting article preview links.
#'
#' @return A character vector containing URLs for downloading historical meteorological data.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads the HTML content of the specified URL
#' 2. Locates HTML elements using the provided XPath selector
#' 3. Extracts the 'href' attribute from the selected elements
#'
#' @note
#' - Requires an active internet connection
#' - Depends on `rvest` packages for web scraping
#' - The function may break if the website structure changes
#'
#' @examples
#' \dontrun{
#' # Default usage
#' download_links <- inmet_find_links()
#'
#' # Custom URL and XPath
#' custom_links <- inmet_find_links(
#'   url = "https://example.com/data",
#'   xpath = "//div[@class='download']/a"
#' )
#' }
#'
#' @importFrom rvest html_elements html_attr
#'
#' @export
inmet_find_links <- function(
    url = "https://portal.inmet.gov.br/dadoshistoricos",
    xpath = "//article[@class='post-preview']/a"
) {
  # Validate URL
  if (!is.character(url) || length(url) != 1) {
    cli::cli_abort("URL must be a single character string")
  }

  # Validate XPath
  if (!is.character(xpath) || length(xpath) != 1) {
    cli::cli_abort("XPath must be a single character string")
  }

  # Attempt to fetch and parse HTML
  tryCatch({
    # Read HTML and extract links
    download_links <- rvest::read_html(url) |>
      rvest::html_elements(xpath = xpath) |>
      rvest::html_attr("href")

    # Validate extracted links
    if (length(download_links) == 0) {
      cli::cli_warn("No download links found at the specified URL")
      return(character(0))
    }

    # Optional logging
    cli::cli_alert_success("Found {length(download_links)} download links")

    return(download_links)

  }, error = function(e) {
    # Comprehensive error handling
    cli::cli_alert_danger("Error fetching download links: {e$message}")

    if (grepl("Could not resolve host", e$message)) {
      cli::cli_alert_warning("Network connection issue. Check internet connectivity.")
    } else if (grepl("HTTP error", e$message)) {
      cli::cli_alert_warning("HTTP error. Verify the URL is accessible.")
    }

    return(NULL)
  })
}

#' Download Historical Meteorological Data from INMET
#'
#' Downloads ZIP files containing historical meteorological data from the
#' Brazilian National Institute of Meteorology (INMET) portal.
#'
#' @param data_dir Character string specifying the base directory for data storage.
#'   If `NULL`, defaults to a standard package data-raw directory.
#' @param overwrite Logical. If `TRUE`, existing ZIP files will be overwritten.
#'   If `FALSE` and files exist, the function will return `NULL`.
#'
#' @return
#' \itemize{
#'   \item A vector of downloaded file paths if successful
#'   \item `NULL` if no files are downloaded (e.g., files already exist and `overwrite = FALSE`)
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Creates necessary directories for data storage
#'   \item Fetches download links from the INMET portal
#'   \item Checks for existing files
#'   \item Handles potential download errors
#' }
#'
#' @note
#' \itemize{
#' \item Requires an active internet connection.
#' \item Downloads historical meteorological data for Brazilian stations.
#' }
#'
#' @examples
#' \dontrun{
#' # Standard download
#' inmet_download_data()
#'
#' # Overwrite existing files
#' inmet_download_data(overwrite = TRUE)
#'
#' # Custom data directory
#' inmet_download_data(data_dir = "/path/to/custom/directory")
#' }
#'
#' @importFrom cli cli_alert_info cli_warn cli_progress_bar cli_alert_success
#' @importFrom here here
#' @importFrom utils download.file
#'
#' @export
inmet_download_data <- function(data_dir = NULL, overwrite = FALSE) {
  # Input validation
  if (!is.null(data_dir) && !is.character(data_dir)) {
    cli::cli_abort("data_dir must be a character string or NULL")
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    cli::cli_abort("overwrite must be a single logical value")
  }

  # Set default directories
  tryCatch({
    data_dir <- here::here("data-raw", "data")
    dest_dir <- here::here("data-raw", "data", "zipped_files")
  }, error = function(e) {
    cli::cli_alert_danger("Error creating default directories: {e$message}")
    stop("Cannot determine default directory paths")
  })

  # Create directories with error handling
  cli::cli_alert_info("Creating directories...")
  tryCatch({
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to create directories: {e$message}")
    stop("Directory creation failed")
  })

  # Fetch download links
  cli::cli_alert_info("Fetching download links...")
  download_links <- inmet_find_links()

  # Validate download links
  if (is.null(download_links) || length(download_links) == 0) {
    cli::cli_alert_danger("No download links found")
    return(NULL)
  }

  # Check existing files
  check <- list.files(dest_dir, pattern = "\\.zip$")

  if (length(check) > 0) {
    cli::cli_warn("Zipped files detected in {dest_dir}.")

    if (!overwrite) {
      cli::cli_warn("To overwrite files, use `overwrite = TRUE`.")
      return(NULL)
    }
  }

  # Prepare destination files
  dest_file <- here::here(dest_dir, basename(download_links))

  # Download progress bar
  cli::cli_alert_info("Starting download of {length(download_links)} files")
  cli::cli_progress_bar(
    total = length(download_links),
    format = paste(
      "{cli::pb_spin} Downloading {current_year}",
      "({cli::pb_current}/{cli::pb_total})",
      "| {cli::pb_percent}"
    )
  )

  # Safely download function
  safely_download <- function(url, destfile) {
    tryCatch(
      download.file(url, destfile = destfile, mode = "wb", quiet = TRUE),
      error = function(e) {
        cli::cli_alert_danger("Failed to download {basename(destfile)}: {e$message}")
        return(NULL)
      }
    )
  }

  # Download loop with error tracking
  results <- vector("list", length(download_links))
  for (i in seq_along(download_links)) {

    current_year <- gsub("\\.zip", "", basename(download_links)[[i]])
    results[[i]] <- safely_download(download_links[[i]], dest_file[[i]])
    cli::cli_progress_update()

  }

  cli::cli_progress_done()

  # Download summary
  successful_downloads <- sum(!sapply(results, is.null))
  cli::cli_alert_success(
    "Download complete: {successful_downloads}/{length(download_links)} files downloaded"
  )

  # Return paths of downloaded files
  return(dest_file[!sapply(results, is.null)])
}
