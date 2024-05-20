#' Load RDA Files from a Directory
#'
#' This function lists all `.rda` files in a specified directory and loads them into the global environment.
#'
#' @param dir A character string specifying the directory from which to load the `.rda` files. Defaults to `"data/sd123"`.
#' @return This function does not return a value. It loads the `.rda` files into the global environment.
#' @examples
#' \dontrun{
#' # Load .rda files from the default directory
#' load_rda_files()
#'
#' # Load .rda files from a custom directory
#' load_rda_files("your/custom/path")
#' }
#'
#' @export
#' @export
load_rda_files <- function(dir = "data/sd123") {
    # List .rda files in the specified directory
    rdaFiles <- list.files(dir, pattern = "\\.rda$", full.names = TRUE)

    # Load the .rda files into the global environment
    for (rdaFile in rdaFiles) {
        load(here::here(rdaFile), envir = .GlobalEnv)
    }
}
