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
#' @keywords internal
checkData <- function() {
    # Check if the data is loaded
    if (exists("householdConsumption") && exists("householdDetails") && exists("nctList")) {
        return(TRUE)
    } else {
        print("All expected data not loaded, please load data first.")
        return(FALSE)
    }
}
