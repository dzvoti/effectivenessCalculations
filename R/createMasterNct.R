#' Create a master NCT (Nutrient Composition Table)
#'
#' This function creates a master NCT from a list of NCTs from the MAPS Tool. It selects specified columns from the NCT list and reshapes the data from long to wide format.
#'
#' @param nctList A list of NCTs.
#' @param fctListIdCol A string specifying the column name for the FCT list ID. Default is "fctListId".
#' @param foodGenusIdCol A string specifying the column name for the food genus ID. Default is "foodGenusId".
#' @param micronutrientIdCol A string specifying the column name for the micronutrient ID. Default is "micronutrientId".
#' @param micronutrientCompositionCol A string specifying the column name for the micronutrient composition. Default is "micronutrientComposition".
#'
#' @return A data frame representing the master NCT. Each row corresponds to a food item, and each column corresponds to a micronutrient.
#'
#' @examples
#' \dontrun{
#' createMasterNct(nctList)
#' }
#'
#' @keywords internal
createMasterNct <- function(nctList, fctListIdCol = "fctListId", foodGenusIdCol = "foodGenusId", micronutrientIdCol = "micronutrientId", micronutrientCompositionCol = "micronutrientComposition") {
    # if (checkData()) {
    masterNct <- nctList |>
        dplyr::select(fctListIdCol, foodGenusIdCol, micronutrientIdCol, micronutrientCompositionCol) |>
        tidyr::pivot_wider(names_from = micronutrientIdCol, values_from = micronutrientCompositionCol)
    # }
    return(masterNct)
}
