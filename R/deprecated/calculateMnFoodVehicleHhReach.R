# #' Calculate Micronutrient Food Vehicle Household Reach
# #'
# #' This function calculates the household reach for each food vehicle and micronutrient. It filters the fortifiable food items to get the food vehicle, processes the consumption data, and then calculates the household reach.
# #'
# #' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "foodGenusId", "householdId", "amountConsumedInG".
# #' @param householdDetailsDf A dataframe containing household details. Must contain columns: "householdId", "afeFactor".
# #' @param nctListDf A dataframe containing the NCT list.
# #' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items. Must contain columns: "food_genus_id", "food_vehicle_name".
# #' @param foodVehicleName A string specifying the food vehicle name. Default is "wheat flour".
# #' @param MN A string specifying the micronutrient. Default is "A".
# #' @param year An integer specifying the year. Default is 2024.
# #' @param aggregationGroup A character vector specifying the columns to group by. Default is `c("admin0Name", "admin1Name")`.
# #'
# #' @return A dataframe with the household reach for the specified food vehicle and micronutrient.
# #'
# #' @examples
# #' \dontrun{
# #' calculateMnFoodVehicleHhLSFFReach(
# #'     householdConsumptionDf = householdConsumption,
# #'     householdDetailsDf = householdDetails,
# #'     nctListDf = nctList,
# #'     fortifiableFoodItemsDf = fortifiable_food_items,
# #'     foodVehicleName = "wheat flour",
# #'     MN = "A",
# #'     year = 2024,
# #'     aggregationGroup = c("admin0Name", "admin1Name")
# #' )
# #' }
# #'
# #' @export
# calculateMnFoodVehicleHhLSFFReach <- function(
#     householdConsumptionDf = householdConsumption,
#     householdDetailsDf = householdDetails,
#     nctListDf = nctList,
#     fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
#     foodVehicleName = "wheat flour",
#     MN = "A",
#     intakeThresholdsDf = intakeThresholds,
#     year = 2024,
#     aggregationGroup = c("admin0Name", "admin1Name")) {
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
#     requiredConsumptionCols <- c("foodGenusId", "householdId", "amountConsumedInG")
#     requiredDetailsCols <- c("householdId", "afeFactor")
#     requiredFortifiableCols <- c("food_genus_id", "food_vehicle_name")

#     # Check if required columns exist in the dataframes
#     if (!all(requiredConsumptionCols %in% colnames(householdConsumptionDf))) {
#         stop(paste("householdConsumptionDf must contain the following columns:", paste(requiredConsumptionCols, collapse = ", ")))
#     }

#     if (!all(requiredDetailsCols %in% colnames(householdDetailsDf))) {
#         stop(paste("householdDetailsDf must contain the following columns:", paste(requiredDetailsCols, collapse = ", ")))
#     }

#     if (!all(requiredFortifiableCols %in% colnames(fortifiableFoodItemsDf))) {
#         stop(paste("fortifiableFoodItemsDf must contain the following columns:", paste(requiredFortifiableCols, collapse = ", ")))
#     }

#     # Use the createMasterNct function to create a master NCT
#     masterNCT <- effectivenessCalculations::createMasterNct(nctList)

#     # Filter the fortifiable food items to get the food vehicle
#     fortifiableFoodVehicle <- fortifiableFoodItemsDf |>
#         dplyr::filter(food_vehicle_name == foodVehicleName)

#     # Pull yearAverageFortificationLevel
#     yearAverageFortificationLevel <- yearAverageFortificationLevel(fortification_vehicle = foodVehicleName, Year = year, MN = MN)

#     ## Create a wider format for the intakeThresholds
#     earThreshholds <- intakeThresholdsDf |>
#         dplyr::select(nutrient, ear) |>
#         # Remove rows where ear is NA
#         dplyr::filter(!is.na(ear)) |>
#         # Leave thresholds for the nutrients in the MNList
#         dplyr::filter(nutrient %in% MN) |>
#         tidyr::pivot_wider(names_from = nutrient, values_from = ear) |>
#         # Convert all columns to numeric
#         dplyr::mutate_all(as.numeric) |>
#         # Add a suffix of "ear" to the column names
#         dplyr::rename_with(~ paste0(., "SupplyEarThreshold"), dplyr::everything())


#     # Process the consumption data
#     MnFoodVehicleHhLSFFReach <- householdConsumptionDf |>
#         tibble::as_tibble() |>
#         dplyr::left_join(fortifiableFoodVehicle, by = c("foodGenusId" = "food_genus_id")) |>
#         dplyr::left_join(householdDetailsDf, by = "householdId") |>
#         # Join the master NCT to the consumption data
#         dplyr::left_join(masterNCT) |>
#         dplyr::filter(!is.na(food_vehicle_name)) |>
#         dplyr::mutate_at(c("amountConsumedInG", "afeFactor", MN), as.numeric) |>
#         dplyr::group_by(householdId) |>
#         dplyr::mutate(dplyr::across(MN, ~ . / afeFactor)) |>
#         # dplyr::mutate(amountConsumedInG = as.numeric(amountConsumedInG), afeFactor = as.numeric(afeFactor)) |>
#         dplyr::summarize(
#             householdId = dplyr::first(householdId),
#             food_vehicle_name = dplyr::first(food_vehicle_name),
#             fortifiable_portion = dplyr::first(fortifiable_portion),
#             admin0Name = dplyr::first(admin0Name),
#             admin1Name = dplyr::first(admin1Name),
#             admin2Name = dplyr::first(admin2Name),
#             urbanity = dplyr::first(urbanity),
#             wealthQuintile = dplyr::first(wealthQuintile),
#             afeFactor = dplyr::first(afeFactor),
#             dailyAmountConsumedPerAfeInG = sum(amountConsumedInG / afeFactor, na.rm = TRUE),
#             dplyr::across(dplyr::all_of(MN), ~ sum(.x / 100 * amountConsumedInG, na.rm = TRUE), .names = "{.col}Supply")
#         ) |>
#         dplyr::ungroup() |>
#             dplyr::mutate(dplyr::across(dplyr::ends_with("Supply"), ~ (. * yearAverageFortificationLevel), .names = "{.col}addedByLSFF")) |>
#             dplyr::mutate(dplyr::across(dplyr::ends_with("Supply"), ~ . + (. * yearAverageFortificationLevel), .names = "{.col}wLSFF")) |>
#             |>
#         # Bind thresholds to the data. The thresholds data has one row so it should be recycled to the number of rows in the data
#         dplyr::bind_cols(earThreshholds)

#         # Create adequacy column without LSFF
#         if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholds, MN, "ear"))) {
#             MnFoodVehicleHhLSFFReach[[paste0(MN, "EarAdequacywOUTLSFF")]] <- ifelse(MnFoodVehicleHhLSFFReach[[paste0(MN, "Supply")]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, MN, "ear"), 1, 0)
#         }

#         # Create adequacy column with LSFF
#         if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholds, MN, "ear"))) {
#             MnFoodVehicleHhLSFFReach[[paste0(MN, "EarAdequacywLSFF")]] <- ifelse(MnFoodVehicleHhLSFFReach[[paste0(MN, "SupplywLSFF")]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, MN, "ear"), 1, 0)
#         }


#    # Calculate original HH numbers
#    statsHouseholdCount <- householdDetailsDf |>
#        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
#        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
#        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
#        dplyr::summarize(households = dplyr::n())

#    # Group by the aggregation group
#    MnFoodVehicleHhLSFFReach <- MnFoodVehicleHhLSFFReach |>
#        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
#        dplyr::summarize(dplyr::across(dplyr::ends_with("EarAdequacywLSFF"), ~ sum(.x, na.rm = TRUE), .names = "{.col}EarAdequacywLSFFCount"))

#    # Finish up the summaries.

#     return(MnFoodVehicleHhLSFFReach)
# }
