#' Calculate Pre and Post LSFF Nutrient Summaries
#'
#' This function calculates summaries of nutrient inadequacy before and after large-scale food fortification (LSFF) for different administrative groups using the Adult Female Equivalent (AFE) Method.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "householdId", "amountConsumedInG".
#' @param householdDetailsDf A dataframe containing household details. Must contain columns: "householdId", "memberCount","afeFactor".
#' @param nctListDf A dataframe containing nutrient composition tables. Must contain column: "nutrient".
#' @param intakeThresholdsDf A dataframe containing intake thresholds for nutrients. Must contain columns: "nutrient", "ear", "ul".
#' @param aggregationGroup A character vector of administrative groups to aggregate the data. Must not be empty. Defaults to c("admin0Name", "admin1Name").
#' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items. Generated using the function `createFortifiableFoodItemsTable()`.
#' @param foodVehicleName A character string specifying the name of the food vehicle for fortification. Defaults to "wheat flour".
#' @param fortificationLevels A dataframe containing the average fortification levels for different nutrients and years.
#' @param years A numeric vector specifying the years for which LSFF is analyzed. Defaults to 2021:2024.
#' @param MNList A character vector of nutrients to be included in the analysis. Defaults to "A". Must not be empty.
#'
#' @return A dataframe with the summaries of nutrient inadequacy for the specified administrative groups before and after LSFF.
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_pre_and_post_lsff_summaries_afe(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     nctListDf = nctList,
#'     intakeThresholdsDf = intakeThresholds,
#'     aggregationGroup = c("admin0Name", "admin1Name"),
#'     fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
#'     foodVehicleName = "wheat flour",
#'     fortificationLevels = fortificationLevels,
#'     years = c(2021:2024),
#'     MNList = c("A", "Ca")
#' )
#' }
calculate_pre_and_post_lsff_summaries_afe <- function(
    householdConsumptionDf = householdConsumption,
    householdDetailsDf = householdDetails,
    nctListDf = nctList,
    intakeThresholdsDf = intakeThresholds,
    aggregationGroup = c("admin0Name", "admin1Name"),
    fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
    foodVehicleName = "wheat flour",
    fortificationLevelsDf = fortificationLevels,
    years = c(2021:2024),
    MNList = "A") {
    # Define required columns
    requiredConsumptionCols <- c("householdId", "amountConsumedInG")
    requiredDetailsCols <- c("householdId", "memberCount")
    requiredNctCols <- c("micronutrientId")
    requiredIntakeCols <- c("nutrient", "CND")

    # Check if MNList is a character vector
    if (!is.character(MNList)) {
        stop("MNList must be a character vector e.g. c('A', 'Ca')")
    }

    # Check if aggregationGroup is a character vector
    if (!is.character(aggregationGroup)) {
        stop("aggregationGroup must be a character vector e.g. c('admin0Name', 'admin1Name')")
    }

    # Check if MNList and aggregationGroup are not empty
    if (length(aggregationGroup) == 0) {
        stop("aggregationGroup cannot be empty")
    }

    # Check if input dataframes have required columns
    if (!all(requiredConsumptionCols %in% names(householdConsumptionDf))) {
        stop(paste("householdConsumptionDf must contain the following columns:", paste(requiredConsumptionCols, collapse = ", ")))
    }

    if (!all(requiredDetailsCols %in% names(householdDetailsDf))) {
        stop(paste("householdDetailsDf must contain the following column:", paste(requiredDetailsCols, collapse = ", ")))
    }

    if (!all(requiredNctCols %in% names(nctListDf))) {
        stop(paste("nctListDf must contain the following columns:", paste(requiredNctCols, collapse = ", ")))
    }

    if (!all(requiredIntakeCols %in% names(intakeThresholdsDf))) {
        stop(paste("intakeThresholdsDf must contain the following columns:", paste(requiredIntakeCols, collapse = ", ")))
    }

    # Mutate fortificationLevel values to numeric
    fortificationLevelsDf <- fortificationLevelsDf |>
        dplyr::mutate_at(c("perc_fortifiable", "perc_fortified", "perc_average_fortification_level", MNList), as.numeric)

    # Use the createMasterNct function to create a master NCT
    masterNCT <- effectivenessCalculations::createMasterNct(nctList)

    # Filter the fortifiable food items to get the food vehicle
    fortifiableFoodVehicle <- fortifiableFoodItemsDf |>
        dplyr::filter(food_vehicle_name == foodVehicleName)

    ## Create a wider format for the intakeThresholds
    earThreshholds <- intakeThresholdsDf |>
        dplyr::select(nutrient, ear) |>
        # Remove rows where ear is NA
        dplyr::filter(!is.na(ear)) |>
        # Leave thresholds for the nutrients in the MNList
        dplyr::filter(nutrient %in% MNList) |>
        tidyr::pivot_wider(names_from = nutrient, values_from = ear) |>
        # Convert all columns to numeric
        dplyr::mutate_all(as.numeric) |>
        # Add a suffix of "ear" to the column names
        dplyr::rename_with(~ paste0(., "SupplyEarThreshold"), dplyr::everything())

    # Process the consumption data
    # Load the consumption data
    enrichedHouseholdConsumption <- householdConsumptionDf |>
        # Not necessary by its a personal preference
        tibble::as_tibble() |>
        # Join the household details to the consumption data (Joining columns with the same name)
        dplyr::left_join(householdDetailsDf) |>
        # Join the master NCT to the consumption data
        dplyr::left_join(masterNCT) |>
        dplyr::left_join(fortifiableFoodVehicle, by = c("foodGenusId" = "food_genus_id")) |>
        # Convert all columns needed for calculations to numeric
        dplyr::mutate_at(c("amountConsumedInG", "afeFactor", "fortifiable_portion", MNList), as.numeric) |>
        dplyr::mutate(dplyr::across(MNList, ~ . / afeFactor), amountConsumedInGAfe = amountConsumedInG / afeFactor) |>
        dplyr::bind_cols(earThreshholds)

    # Calculate HH count summaries
    HHCountSummaries <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::distinct(householdId) |>
        dplyr::summarise(householdsCount = dplyr::n())

    # Fortification vehicle reach summaries
    fortificationVehicleReach <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::filter(!is.na(food_vehicle_name)) |>
        dplyr::distinct(householdId) |>
        dplyr::summarize(fortification_vehicle_reach_hh_count = dplyr::n())

    # Mean and median fortification vehicle amounts consumed
    # TODO: This calculation is repeated below. Check if it is necessary here.
    # fortificationVehicleAmountsConsumedAfe <- enrichedHouseholdConsumption |>
    #     dplyr::filter(!is.na(food_vehicle_name)) |>
    #     dplyr::group_by(householdId) |>
    #     dplyr::summarize(fortification_vehicle_amountConsumedInGAfe = sum(amountConsumedInGAfe, na.rm = TRUE
    #         # median_fortification_vehicle_amountConsumedInGAfe = median(amountConsumedInGAfe, na.rm = TRUE), mean_fortification_vehicle_amountConsumedInGAfe = mean(amountConsumedInGAfe, na.rm = TRUE
    #         )) |>
    #     dplyr::left_join(householdDetailsDf) |>
    #     dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #     dplyr::summarize(
    #         median_fortification_vehicle_amountConsumedInGAfe = median(fortification_vehicle_amountConsumedInGAfe, na.rm = TRUE), mean_fortification_vehicle_amountConsumedInGAfe = mean(fortification_vehicle_amountConsumedInGAfe, na.rm = TRUE)
    # )

    # Average daily consumption per AFE
    # TODO: Check if this calculation is correct when all foods are tagged as fortifiable. Currently it is not used in the final output.
    amountConsumedPerDayAfe <- enrichedHouseholdConsumption |>
        dplyr::group_by(householdId) |>
        dplyr::summarize(
            # dailyAmountConsumedPerAfeInG = sum(amountConsumedInG / 100 * afeFactor, na.rm = TRUE)
            dailyAmountConsumedPerAfeInG = sum(amountConsumedInG / afeFactor, na.rm = TRUE)
        ) |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            meanDailyAmountConsumedPerAfeInG = mean(dailyAmountConsumedPerAfeInG, na.rm = TRUE),
            medianDailyAmountConsumedPerAfeInG = median(dailyAmountConsumedPerAfeInG, na.rm = TRUE)
        )

    # Amount consumed containing fortificant
    amountConsumedContainingFortificant <- enrichedHouseholdConsumption |>
        dplyr::group_by(householdId) |>
        dplyr::filter(!is.na(food_vehicle_name)) |>
        dplyr::summarize(
            dailyAmountConsumedPerAfeInG = sum(amountConsumedInG / afeFactor, na.rm = TRUE)
        ) |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            meanDailyamountConsumedContainingFortificantInGAfe = mean(dailyAmountConsumedPerAfeInG, na.rm = TRUE),
            medianDailyAmountConsumedContainingFortificantInGAfe = median(dailyAmountConsumedPerAfeInG, na.rm = TRUE)
        )

    # Merge the summaries
    initialSummaries <- HHCountSummaries |>
        dplyr::left_join(fortificationVehicleReach) |>
        # dplyr::left_join(amountConsumedPerDayAfe) |>
        # dplyr::left_join(fortificationVehicleAmountsConsumedAfe) |>
        dplyr::left_join(amountConsumedContainingFortificant)

    for (nutrient in MNList) {
        # TODO: Check if units are the same for ear and ul. Create _BaseSupply_UL when Retinol and Folic Acid are available in the NctList.

        enrichedHouseholdConsumption[paste0(nutrient, "_BaseSupply")] <- enrichedHouseholdConsumption[nutrient] / 100 * enrichedHouseholdConsumption["amountConsumedInG"] # TODO: This is verified as correct. Line 112 divided the nuttrient by AFE factor hence we use amountConsumedInG and not amountConsumedInGAfe here. It's quacky I know but it's correct.

        for (year in years) {
            # Calculate the supply of the nutrient with LSFF per food item
            # TODO: Check if this calculation is correct when all foods are tagged as fortifiable.
            enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_LSFFSupply")] <- enrichedHouseholdConsumption["amountConsumedInGAfe"] * yearAverageFortificationLevel(fortification_vehicle = foodVehicleName, Year = year, MN = nutrient, fortificationLevels = fortificationLevelsDf) * enrichedHouseholdConsumption["fortifiable_portion"] / 100
        }
    }

    # Aggregate nutrient supplies by household
    nutrientSupply <- enrichedHouseholdConsumption |>
        dplyr::group_by(householdId) |>
        dplyr::summarize(
            dplyr::across(dplyr::ends_with("_BaseSupply"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
            dplyr::across(dplyr::ends_with("_LSFFSupply"), ~ sum(.x, na.rm = TRUE), .names = "{.col}")
        )


    # Calculate mean and median nutrient supplies
    # TODO: These were checked and are consistent with the maps tool.
    medianNutrientSupplySummaries <- nutrientSupply |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            dplyr::across(dplyr::ends_with("_BaseSupply"), ~ round(mean(.x, na.rm = TRUE), 0), .names = "{.col}MeanSupply"),
            dplyr::across(dplyr::ends_with("_BaseSupply"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianSupply")
            # TOD0: Add the same for LSFFSupply
        )

    # Add _BaseSupply and _LSFFSupply for each nutrient and year combo
    for (nutrient in MNList) {
        for (year in years) {
            nutrientSupply[paste0(nutrient, "_", year, "_BaseAndLSFFTotalSupply")] <- nutrientSupply[paste0(nutrient, "_BaseSupply")] + nutrientSupply[paste0(nutrient, "_", year, "_LSFFSupply")]
        }
    }


    # Remerge the household details
    enrichedNutrientSupply <- nutrientSupply |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::bind_cols(earThreshholds)

    # TODO: Calculate the gap between the {ear}threshold and the nutrient supply for both basesuply and baseandlsffsupply and the next loop should then calculate the mean and media of the gap
    # Create adequacy columns for each Baseline and LSFF nutrient supply
    for (nutrient in MNList) {
        if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"))) {
            enrichedNutrientSupply[paste0(nutrient, "_base_supply_ear_inadequacy")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_BaseSupply")] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"), 0, 1)
        }
        for (year in years) {
            if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"))) {
                enrichedNutrientSupply[paste0(nutrient, "_", year, "_base_and_lsff_ear_inadequacy")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_", year, "_BaseAndLSFFTotalSupply")] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"), 0, 1)
            }
        }
    }

    # Check if the intake is above the Upper Limit
    for (nutrient in MNList) {
        if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"))) {
            #
            enrichedNutrientSupply[paste0(nutrient, "_base_ul_exceedance")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_BaseSupply")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"), 1, 0)
        }
        for (year in years) {
            if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"))) {
                enrichedNutrientSupply[paste0(nutrient, "_", year, "_base_and_lsff_ul_exceedance")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_", year, "_BaseAndLSFFTotalSupply")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"), 1, 0)
            }
        }
    }

    # Create adequacy summaries
    inadequacySummarries <- enrichedNutrientSupply |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            dplyr::across(dplyr::ends_with("_ear_inadequacy"), ~ sum(.x, na.rm = TRUE), .names = "{.col}_count"),
            dplyr::across(dplyr::ends_with("_ul_exceedance"), ~ sum(.x, na.rm = TRUE), .names = "{.col}_count")
        ) |>
        dplyr::left_join(initialSummaries) |>
        dplyr::left_join(medianNutrientSupplySummaries) |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_count"), ~ round((.x * 100 / householdsCount), 2), .names = "{.col}_perc"))


    # Get the column order for the data
    columnOrder <- sort(names(inadequacySummarries))

    # Reorder the columns for better readability
    finalSummarries <- inadequacySummarries |>
        dplyr::select(dplyr::all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, householdsCount, dplyr::everything())

    return(finalSummarries)
}
