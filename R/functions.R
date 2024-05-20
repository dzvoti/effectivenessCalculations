#' Calculate Micronutrient Content
#'
#' This function calculates the micronutrient content for each food item in a dataframe.
#'
#' @param df A dataframe containing food consumption data and micronutrient content.
#' @param consumptionCol A string specifying the column name for the food consumption data.
#' @param MNs A vector of column names for the micronutrients.
#'
#' @return A dataframe with the calculated micronutrient content for each food item.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(food = c("apple", "banana", "carrot"), consumption = c(100, 200, 150), vitaminA = c(0.5, 0.6, 0.7))
#' df <- calculateMNsContent(df, "consumption", c("vitaminA"))
#' }
#'
#' @export
calculateMNsContent <- function(df, consumptionCol, MNs) {
    for (i in MNs) {
        df[i] <- ifelse(is.na(df[[i]]) | df[[i]] == 0, NA, df[[consumptionCol]] * df[[i]]) # Is this the correct MNs conversion?
    }
    return(df)
}


#' Calculate Consumption Per Adult Female Equivalent (AFE)
#'
#' This function calculates the consumption per AFE for each food item in a dataframe.
#'
#' @param df A dataframe containing food consumption data and AFE data.
#' @param consumptionCol A string specifying the column name for the food consumption data.
#' @param afeCol A string specifying the column name for the AFE data.
#'
#' @return A dataframe with the calculated consumption per AFE for each food item.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(food = c("apple", "banana", "carrot"), consumption = c(100, 200, 150), afe = c(2, 3, 4))
#' df <- calculateConsPerAfe(df, "consumption", "afe")
#' }
#'
#' @export
calculateConsPerAfe <- function(df, consumptionCol, afeCol) {
    df[[paste0(consumptionCol, "PerAfe")]] <- df[[consumptionCol]] / df[[afeCol]]
    return(df)
}



#' Preview the First 1000 Rows of a Dataframe
#'
#' This function previews the first 1000 rows of a dataframe using DT::datatable.
#' The name of the dataframe is used as the caption, which is left-aligned.
#'
#' @param df A dataframe to preview.
#'
#' @return A DT::datatable object showing the first 1000 rows of the input dataframe.
#'
#' @examples
#' # Create a dataframe
#' df <- data.frame(
#'     x = rnorm(5000),
#'     y = rnorm(5000),
#'     z = rnorm(5000)
#' )
#' # Preview the dataframe
#' previewData(df)
#'
#' @export
previewData <- function(df) {
    df |>
        head(1000) |>
        # Use the name of the dataframe as the caption. Left align the caption
        DT::datatable(
            caption = htmltools::tags$caption(
                style = "caption-side: left; text-align: left;",
                paste0("Preview of the ", deparse(substitute(df)))
            )
        )
}



# #' Calculate Food Vehicle Household Reach
# #'
# #' This function calculates the household reach for each food vehicle. It filters the fortifiable food items to get the food vehicle, processes the consumption data, and then calculates the household reach.
# #'
# #' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "foodGenusId", "householdId".
# #' @param householdDetailsDf A dataframe containing household details. Must contain column: "householdId".
# #' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items. Must contain columns: "food_genus_id", "food_vehicle_name".
# #' @param foodVehicleName A string specifying the food vehicle name. Default is "wheat flour".
# #' @param aggregationGroup A character vector specifying the columns to group by. Default is `c("admin0Name", "admin1Name")`.
# #'
# #' @return A dataframe with the household reach for the specified food vehicle.
# #'
# #' @examples
# #' calculateFoodVehicleHouseholdReach(
# #'     householdConsumptionDf = householdConsumption,
# #'     householdDetailsDf = householdDetails,
# #'     fortifiableFoodItemsDf = fortifiable_food_items,
# #'     foodVehicleName = "wheat flour",
# #'     aggregationGroup = c("admin0Name", "admin1Name")
# #' )
# #'
# #' @export
# calculateFoodVehicleHouseholdReach <- function(householdConsumptionDf = householdConsumption, householdDetailsDf = householdDetails, fortifiableFoodItemsDf = fortifiable_food_items, foodVehicleName = "wheat flour", aggregationGroup = c("admin0Name", "admin1Name")) {
#     # Check if the dataframes are dataframes
#     if (!is.data.frame(householdConsumptionDf)) {
#         stop("householdConsumptionDf must be a dataframe")
#     }

#     if (!is.data.frame(householdDetailsDf)) {
#         stop("householdDetailsDf must be a dataframe")
#     }

#     if (!is.data.frame(fortifiableFoodItemsDf)) {
#         stop("fortifiableFoodItemsDf must be a dataframe")
#     }

#     # Define required columns
#     requiredConsumptionCols <- c("foodGenusId", "householdId")
#     requiredDetailsCols <- c("householdId")
#     requiredFortifiableCols <- c("food_genus_id", "food_vehicle_name")

#     # Check if required columns exist in the dataframes
#     if (!all(requiredConsumptionCols %in% colnames(householdConsumptionDf))) {
#         stop(paste("householdConsumptionDf must contain the following columns:", paste(requiredConsumptionCols, collapse = ", ")))
#     }

#     if (!all(requiredDetailsCols %in% colnames(householdDetailsDf))) {
#         stop(paste("householdDetailsDf must contain the following column:", paste(requiredDetailsCols, collapse = ", ")))
#     }

#     if (!all(requiredFortifiableCols %in% colnames(fortifiableFoodItemsDf))) {
#         stop(paste("fortifiableFoodItemsDf must contain the following columns:", paste(requiredFortifiableCols, collapse = ", ")))
#     }

#     # Filter the fortifiable food items to get the food vehicle
#     fortifiableFoodVehicle <- fortifiableFoodItemsDf |>
#         dplyr::filter(food_vehicle_name == foodVehicleName)

#     # Process the consumption data
#     foodVehicleHouseholdReach <- householdConsumptionDf |>
#         tibble::as_tibble() |>
#         dplyr::left_join(fortifiableFoodVehicle, by = c("foodGenusId" = "food_genus_id")) |>
#         dplyr::left_join(householdDetailsDf, by = "householdId") |>
#         dplyr::filter(!is.na(food_vehicle_name)) |>
#         dplyr::group_by(householdId) |>
#         dplyr::summarize(
#             dailyConsumptionG = sum(amountConsumedInG, na.rm = TRUE),
#             afeFactor = first(afeFactor)
#         ) |>
#         dplyr::mutate(
#             dailyConsumptionPerAfe = dailyConsumptionG / afeFactor
#         ) |>
#         dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
#         dplyr::summarize(
#             households = n(),
#             totalDailyConsumptionG = sum(dailyConsumptionG, na.rm = TRUE),
#             totalDailyConsumptionPerAfe = sum(dailyConsumptionPerAfe, na.rm = TRUE)
#         )
#     return(foodVehicleHouseholdReach)
# }
