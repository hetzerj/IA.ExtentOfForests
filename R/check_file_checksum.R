#' Check MD5 checksum of the default package file versus the newly downloaded file
#'
#' This function compares the MD5 checksum of the newly downloaded indicator file
#' with the existing (default) file from the package. It is used to determine if
#' the downloaded file is a newer version.
#'
#' @param new_file_path The file path where the new indicator file was downloaded.
#'
#' @return TRUE if the existing package data is up to date, or FALSE if the downloaded data is newer.
#' @export
#'
#' @examples
#' check_file_checksum("path/to/new_file.csv")
#'
check_file_checksum <- function(new_file_path) {
  # Get the file path of the existing (default) indicator file in the package
  inst_filepath  <- system.file("indicator_2019", "ExtentOfForests.csv", package = "IA.ExtentOfForests")

  # Calculate MD5 checksums for both the new and existing files
  new_file_checksum <- digest::digest(file = new_file_path, algo = "md5")
  existing_file_checksum <- digest::digest(file = inst_filepath, algo = "md5")

  # Compare the checksums
  if (new_file_checksum != existing_file_checksum) {
    message("There is an updated indicator file available: Extent of forests")
    return(FALSE)  # The existing file is outdated
  } else {
    message("The current indicator file is up to date: Extent of forests")
    return(TRUE)   # The existing file is up to date
  }
}

