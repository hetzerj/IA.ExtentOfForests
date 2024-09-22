#' Download the latest indicator file for "Extent of Forests"
#'
#' This function downloads the "Extent of Forests" indicator data from its origin,
#' specifically from the FAO database using the FAOSTAT R package. The function
#' allows users to define a local directory where the downloaded data will be saved.
#' If no directory is provided, a temporary directory will be created.
#'
#' @param local_dir A string representing the file path where the data should be downloaded.
#' If not provided, the function will automatically create a user-specific data directory using rappdirs.
#'
#' @return `is_upToDate`: TRUE if the downloaded file is identical (based on MD5 checksum)
#' to the version already in the package, or FALSE if the newly downloaded file is different.
#' @export
#'
#' @examples
#' download_from_origin() # Download to a default user directory
#' download_from_origin("path/to/directory") # Download to a specific directory
download_from_origin <- function(local_dir = NULL) {

  # Set default local directory using rappdirs if local_dir is not provided or is NA
  if (is.null(local_dir) || is.na(local_dir)) {
    local_dir <- rappdirs::user_data_dir("IA.ExtentOfForest", "indicator_reloaded")
  }

  # Create the directory if it doesn't exist
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
    message("Directory created at: ", local_dir)
  }

  # Try downloading data from FAOSTAT
  tryCatch({
    # Download SDGB data from FAOSTAT
    SDGB_download.data <- FAOSTAT::get_faostat_bulk(code = "SDGB",
                                                    data_folder = tempdir(),
                                                    subset = "All Data Normalized")

    # Subset the data for the World, specifically for forest area
    global_data <- subset(SDGB_download.data,
                          element_code == 6121 & area == 'World' & item_code__sdg_ == "'AG_LND_FRST",
                          select = c("year", "value"))

    # Define the file path for the downloaded data
    reloaded_file_path <- file.path(local_dir, "ExtentOfForests.csv")

    # Save the data to a CSV file
    write.csv(global_data, reloaded_file_path, row.names = FALSE)
    message("Latest file successfully downloaded to: ", reloaded_file_path)

    # Check if the newly downloaded file is up to date compared to the package version
    is_upToDate <- check_file_checksum(new_file_path = reloaded_file_path)

  }, error = function(e) {
    message("Error during download or saving: ", e$message)
    is_upToDate <- FALSE
  })

  # Return the result indicating whether the data is up to date
  return(is_upToDate)
}
