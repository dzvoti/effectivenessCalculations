#' Load CSV Files
#'
#' This function loads all CSV files from a specified folder into the global environment.
#' Each CSV file is read into a data frame, its column names are cleaned, and it is
#' assigned to a variable named after the file (without the .csv extension).
#'
#' @param folder_path A string. The path to the folder containing the CSV files.
#'
#' @examples
#' \dontrun{
#' loadCsvFiles("path/to/your/csvfiles")
#' }
#'
#' @return None. This function does not return a value; it assigns variables in the global environment.
#'
#' @export
#' @keywords internal
loadCsvFiles <- function(folder_path) {
    file_paths <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
    data_list <- list()

    # Iterate over each file path
    for (i in seq_along(file_paths)) {
        # Extract the base name without the extension
        file_name <- tools::file_path_sans_ext(basename(file_paths[i]))

        # Read the CSV file and clean names
        data <- readr::read_csv(file_paths[i]) |>
            janitor::clean_names()

        # Assign it to a dynamically named element in the list
        data_list[[file_name]] <- data
    }

    return(data_list)
}
