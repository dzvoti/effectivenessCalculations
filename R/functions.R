#' Load CSV Files
#'
#' This function loads all CSV files from a specified folder into the global environment.
#' Each CSV file is read into a data frame, its column names are cleaned, and it is
#' assigned to a variable named after the file (without the .csv extension).
#'
#' @param folder_path A string. The path to the folder containing the CSV files.
#'
#' @examples
#' loadCsvFiles("path/to/your/csvfiles")
#'
#' @return None. This function does not return a value; it assigns variables in the global environment.
loadCsvFiles <- function(folder_path) {
    file_paths <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

    # Iterate over each file path
    for (file_path in file_paths) {
        # Extract the base name without the extension
        file_name <- tools::file_path_sans_ext(basename(file_path))

        # Read the CSV file and clean names
        data <- readr::read_csv(file_path) |>
            janitor::clean_names()

        # Assign it to a dynamically named variable in the global environment
        assign(file_name, data, envir = .GlobalEnv)
    }
}

#' Load all .rda files in a directory
#'
#' This function loads all .rda files in the specified directory into the global environment.
#'
#' @param dir A character string specifying the directory to load .rda files from.
#'
#' @return Invisible NULL. This function is called for its side effect of loading .rda files into the global environment.
#'
#' @examples
#' \dontrun{
#' load_all_rda_files("data/sd123")
#' }
#'
#' @export
load_all_rda_files <- function(dir) {
    # Get all .rda files in the dir
    rda_files <- list.files(dir, pattern = "\\.rda$", full.names = TRUE)

    # Load all .rda files
    lapply(rda_files, load, .GlobalEnv)
}

#' Load all .rda files in a directory
#'
#' This function loads all .rda files in the specified directory into the global environment.
#'
#' @param dir A character string specifying the directory to load .rda files from.
#'
#' @return Invisible NULL. This function is called for its side effect of loading .rda files into the global environment.
#'
#' @examples
#' \dontrun{
#' load_all_rda_files("data/sd123")
#' }
#'
#' @export
loadMapsRdaTables <- function(dir) {
    # Get all .rda files in the dir
    rda_files <- list.files(dir, pattern = "\\.rda$", full.names = TRUE)

    # Initialize an empty list to store the data
    data <- list()

    # Load all .rda files into the list
    for (file in rda_files) {
        # Get the name of the file without the extension
        name <- tools::file_path_sans_ext(basename(file))

        # Load the file into a temporary environment
        e <- new.env()
        load(file, envir = e)

        # Add the loaded data to the list
        data[[name]] <- e[[ls(e)[1]]] # Just store the first object from the environment
    }

    return(invisible(data)) # Return data but make the return invisible
}

#' Check if Required Data is Loaded
#'
#' This function checks if the required data ("householdConsumption", "householdDetails", and "nctList") is loaded in the global environment.
#'
#' @return A logical value indicating whether the required data is loaded. Returns TRUE if all required data is loaded, and FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' checkData()
#' }
#'
#' @export
checkData <- function() {
    # Check if the data is loaded
    if (exists("householdConsumption") && exists("householdDetails") && exists("nctList")) {
        return(TRUE)
    } else {
        print("All expected data not loaded, please load data first.")
        return(FALSE)
    }
}
