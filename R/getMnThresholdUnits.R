#' Get Micronutrient Thresholds Units
#'
#' This function retrieves the thresholds for a specified micronutrient from a dataframe of intake thresholds.
#'
#' @param intakeThresholds A dataframe containing intake thresholds for various micronutrients.
#' @param Mn A string specifying the micronutrient to retrieve thresholds for.
#' @param param A string specifying the parameter to retrieve.Default is "unitAdequacy".
#'
#' @return A list containing the thresholds units for the specified micronutrient.
#'
#' @examples
#' \dontrun{
#' intakeThresholds <- data.frame(
#'     nutrient = c("vitaminA", "vitaminB", "vitaminC"),
#'     ear = c(0.5, 0.6, 0.7), ul = c(1, 1.2, 1.3), unitAdequacy = c(0.8, 0.9, 1)
#' )
#' thresholds <- getMnThresholdUnits(intakeThresholds, "vitaminA", "unitAdequacy")
#' }
#'
#' @export
#' @keywords internal
getMnThresholdUnits <- function(intakeThresholds, nutrient, param = "unitAdequacy") {
    threshold <- intakeThresholds[intakeThresholds$nutrient == nutrient, param]
    return(threshold)
}
