#' Calculate Food Vehicle Household Reach
#'
#' This function calculates the household reach for each food vehicle. It filters the fortifiable food items to get the food vehicle, processes the consumption data, and then calculates the household reach.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "foodGenusId", "householdId", "amountConsumedInG".
#' @param householdDetailsDf A dataframe containing household details. Must contain columns: "householdId", "afeFactor".
#' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items. Must contain columns: "food_genus_id", "food_vehicle_name".
#' @param foodVehicleName A string specifying the food vehicle name. Default is "wheat flour".
#' @param aggregationGroup A character vector specifying the columns to group by. Default is `c("admin0Name", "admin1Name")`.
#'
#' @return A dataframe with the household reach for the specified food vehicle.
#'
#' @examples
#' \dontrun{
#' calculateFoodVehicleHouseholdReach(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     fortifiableFoodItemsDf = fortifiable_food_items,
#'     foodVehicleName = "wheat flour",
#'     aggregationGroup = c("admin0Name", "admin1Name")
#' )
#' }
#'
#' @export
calculateFoodVehicleHouseholdReach <- function(
    householdConsumptionDf = householdConsumption,
    householdDetailsDf = householdDetails,
    fortifiableFoodItemsDf = fortifiable_food_items,
    foodVehicleName = "wheat flour",
    aggregationGroup = c("admin0Name", "admin1Name")) {
    # Check if the dataframes are dataframes
    if (!is.data.frame(householdConsumptionDf)) {
        stop("householdConsumptionDf must be a dataframe")
    }

    if (!is.data.frame(householdDetailsDf)) {
        stop("householdDetailsDf must be a dataframe")
    }

    if (!is.data.frame(fortifiableFoodItemsDf)) {
        stop("fortifiableFoodItemsDf must be a dataframe")
    }

    # Define required columns
    requiredConsumptionCols <- c("foodGenusId", "householdId", "amountConsumedInG")
    requiredDetailsCols <- c("householdId", "afeFactor")
    requiredFortifiableCols <- c("food_genus_id", "food_vehicle_name")

    # Check if required columns exist in the dataframes
    if (!all(requiredConsumptionCols %in% colnames(householdConsumptionDf))) {
        stop(paste("householdConsumptionDf must contain the following columns:", paste(requiredConsumptionCols, collapse = ", ")))
    }

    if (!all(requiredDetailsCols %in% colnames(householdDetailsDf))) {
        stop(paste("householdDetailsDf must contain the following columns:", paste(requiredDetailsCols, collapse = ", ")))
    }

    if (!all(requiredFortifiableCols %in% colnames(fortifiableFoodItemsDf))) {
        stop(paste("fortifiableFoodItemsDf must contain the following columns:", paste(requiredFortifiableCols, collapse = ", ")))
    }

    # Filter the fortifiable food items to get the food vehicle
    fortifiableFoodVehicle <- fortifiableFoodItemsDf |>
        dplyr::filter(food_vehicle_name == foodVehicleName)

    # Process the consumption data
    foodVehicleHouseholdReach <- householdConsumptionDf |>
        tibble::as_tibble() |>
        dplyr::left_join(fortifiableFoodVehicle, by = c("foodGenusId" = "food_genus_id")) |>
        dplyr::left_join(householdDetailsDf, by = "householdId") |>
        dplyr::filter(!is.na(food_vehicle_name)) |>
        dplyr::group_by(householdId) |>
        dplyr::mutate(amountConsumedInG = as.numeric(amountConsumedInG), afeFactor = as.numeric(afeFactor)) |>
        dplyr::summarize(
            dailyConsumptionG = sum(amountConsumedInG, na.rm = TRUE),
            afeFactor = dplyr::first(afeFactor),
            admin0Name = dplyr::first(admin0Name),
            admin1Name = dplyr::first(admin1Name),
            admin2Name = dplyr::first(admin2Name),
            urbanity = dplyr::first(urbanity),
            wealthQuintile = dplyr::first(wealthQuintile)
        ) |>
        dplyr::mutate(
            dailyConsumptionPerAfe = dailyConsumptionG / afeFactor
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            households = dplyr::n(),
            meanDailyConsumptionG = mean(dailyConsumptionG, na.rm = TRUE),
            medianDailyConsumptionG = median(dailyConsumptionG, na.rm = TRUE),
            totalDailyConsumptionPerAfe = sum(dailyConsumptionPerAfe, na.rm = TRUE)
        )

    return(foodVehicleHouseholdReach)
}
