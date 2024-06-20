#' Year Average Fortification Level
#'
#' This function loads the average fortification levels from a CSV file located in the data directory,
#' and then returns the average fortification level for a specific year and micronutrient.
#'
#' @param fortification_vehicle A string specifying the fortification vehicle.
#' @param Year An integer specifying the year.
#' @param MN A string specifying the micronutrient.
#' @param fortificationLevels A dataframe containing the average fortification levels.
#' @return A numeric value representing the average fortification level for the specified year and micronutrient.
#' @examples
#' \dontrun{
#' avgFortificationLevel <- yearAverageFortificationLevel("wheat flour", 2024, "A")
#' }
#' @export
#' @keywords internal
yearAverageFortificationLevel <- function(fortification_vehicle, Year, MN, fortificationLevels) {
    yearAverageFortificationLevel <- fortificationLevels[fortificationLevels$Year == Year, MN][[1]]

    return(yearAverageFortificationLevel)
}
