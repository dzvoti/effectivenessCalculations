#' Calculate Pre and Post LSFF Nutrient Summaries
#'
#' This function calculates summaries of nutrient inadequacy before and after large-scale food fortification (LSFF) for different administrative groups using the Adult Female Equivalent (AFE) Method.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "householdId", "amountConsumedInG".
#' @param householdDetailsDf A dataframe containing household details. Must contain columns: "householdId", "memberCount".
#' @param nctListDf A dataframe containing nutrient composition tables. Must contain column: "nutrient".
#' @param intakeThresholdsDf A dataframe containing intake thresholds for nutrients. Must contain columns: "nutrient", "ear".
#' @param aggregationGroup A character vector of administrative groups to aggregate the data. Must not be empty. Defaults to c("admin0Name", "admin1Name").
#' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items. Generated using the function `createFortifiableFoodItemsTable()`.
#' @param foodVehicleName A character string specifying the name of the food vehicle for fortification. Defaults to "wheat flour".
#' @param years A numeric vector specifying the years for which LSFF is analyzed. Defaults to 2021:2024.
#' @param MNList A character vector of nutrients to be included in the analysis. Defaults to "A". Must not be empty.
#'
#' @return A dataframe with the summaries of nutrient inadequacy for the specified administrative groups before and after LSFF.
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_pre_and_post_lsff_summaries(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     nctListDf = nctList,
#'     intakeThresholdsDf = intakeThresholds,
#'     aggregationGroup = c("admin0Name", "admin1Name"),
#'     fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
#'     foodVehicleName = "wheat flour",
#'     years = c(2021:2024),
#'     MNList = c("A", "Ca")
#' )
#' }
calculate_pre_and_post_lsff_summaries_cnd <- function(
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

    # Use the createMasterNct function to create a master NCT
    masterNCT <- effectivenessCalculations::createMasterNct(nctList)

    # Filter the fortifiable food items to get the food vehicle
    fortifiableFoodVehicle <- fortifiableFoodItemsDf |>
        dplyr::filter(food_vehicle_name == foodVehicleName)

    ## Create a wider format for the intakeThresholds
    cndThreshholds <- intakeThresholdsDf |>
        dplyr::select(nutrient, CND,CUL) |>
        dplyr::filter(!is.na(CND)) |>
        dplyr::filter(nutrient %in% MNList) |>
        tidyr::pivot_wider(names_from = nutrient, values_from = c(CND,CUL)) |>
        dplyr::mutate_all(as.numeric) |>
        dplyr::rename_with(~ paste0(., "_SupplyThreshold"), dplyr::everything())

    # Process the consumption data
    enrichedHouseholdConsumption <- householdConsumptionDf |>
        tibble::as_tibble() |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::left_join(masterNCT) |>
        dplyr::left_join(fortifiableFoodVehicle, by = c("foodGenusId" = "food_genus_id")) |>
        dplyr::mutate_at(c("amountConsumedInG", "afeFactor", "memberCount", MNList, "Energy"), as.numeric) |>
        dplyr::mutate(dailyAmountConsumedPerAfeInG = amountConsumedInG / afeFactor)

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
    amountConsumedContainingFortificant <- enrichedHouseholdConsumption |>
        dplyr::group_by(householdId) |>
        dplyr::filter(!is.na(food_vehicle_name)) |>
        dplyr::summarize(
            dailyAmountConsumedPerAfe = sum(amountConsumedInG / afeFactor, na.rm = TRUE)
        ) |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            meanDailyamountConsumedContainingFortificantInGPerAfe = mean(dailyAmountConsumedPerAfe, na.rm = TRUE),
            medianDailyamountConsumedContainingFortificantInGPerAfe = median(dailyAmountConsumedPerAfe, na.rm = TRUE)
        )

    # # Average daily consumption per AFE
    # amountConsumedPerDayPerCapita <- enrichedHouseholdConsumption |>
    #     dplyr::group_by(householdId) |>
    #     dplyr::summarize(
    #         perDayAmountConsumedInGPerCapita = sum(perDayAmountConsumedInGPerCapita / 100 * memberCount, na.rm = TRUE)
    #     ) |>
    #     dplyr::left_join(householdDetailsDf) |>
    #         dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #         dplyr::summarize(
    #             meanPerDayAmountConsumedInGPerCapita = mean(perDayAmountConsumedInGPerCapita, na.rm = TRUE),
    #             medianPerDayAmountConsumedInGPerCapita = median(perDayAmountConsumedInGPerCapita, na.rm = TRUE)
    #         )


    # # Amount consumed containing fortificant
    # amountConsumedContainingFortificant <- enrichedHouseholdConsumption |>
    #     dplyr::group_by(householdId) |>
    #     dplyr::filter(!is.na(food_vehicle_name)) |>
    #     dplyr::summarize(
    #         perDayAmountConsumedInGPerCapita = sum(perDayAmountConsumedInGPerCapita, na.rm = TRUE)
    #     ) |>
    #     dplyr::left_join(householdDetailsDf) |>
    #     dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #     dplyr::summarize(
    #         meanDailyamountConsumedContainingFortificantInG = mean(perDayAmountConsumedInGPerCapita, na.rm = TRUE),
    #         medianDailyAmountConsumedContainingFortificantInG = median(perDayAmountConsumedInGPerCapita, na.rm = TRUE)
    #     )

    # Household level fortification vehicle consumption.
    hhFortificantConsumptionInG <- enrichedHouseholdConsumption |>
        dplyr::group_by(householdId) |>
        dplyr::filter(!is.na(food_vehicle_name)) |>
        dplyr::summarize(
            dailyFortificantAmountConsumedInG = sum(amountConsumedInG * fortifiable_portion/100, na.rm = TRUE)
        )

    # summaryFortificantConsumptionInG <- enrichedHouseholdConsumption |>
    #     dplyr::group_by(householdId) |>
    #     dplyr::filter(!is.na(food_vehicle_name)) |>
    #     dplyr::summarize(
    #         dailyFortificantAmountConsumedPerCapitaInG = sum(amountConsumedInG / afeFactor, na.rm = TRUE)
    #     ) |>
    #     dplyr::left_join(householdDetailsDf) |>
    #     dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #     dplyr::summarize(
    #         meanDailyFortificantAmountConsumedPerCapitaInG = mean(dailyFortificantAmountConsumedPerCapitaInG, na.rm = TRUE),
    #         medianDailyFortificantAmountConsumedPerCapitaInG = median(dailyFortificantAmountConsumedPerCapitaInG, na.rm = TRUE)
    #     )

    # Merge the summaries
    initialSummaries <- HHCountSummaries |>
        dplyr::left_join(fortificationVehicleReach) |>
        # dplyr::left_join(amountConsumedPerDayPerCapita) |>
        # dplyr::left_join(summaryFortificantConsumptionInG) |>
        dplyr::left_join(amountConsumedContainingFortificant)



    # Household apparent intake and CND
    enrichedHouseholdConsumption <- enrichedHouseholdConsumption |>
        # dplyr::group_by(householdId) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(c(MNList, "Energy")), ~ (.x / 100 * amountConsumedInG), .names = "{.col}_BaseSupply")) |>
        # dplyr::left_join(householdDetailsDf) |>
        # dplyr::left_join(hhFortificantConsumptionInG)|>
        dplyr::bind_cols(cndThreshholds)

    # Version2: Household apparent intake and CND
    # enrichedHouseholdConsumption_hh <- enrichedHouseholdConsumption |>
    #     dplyr::group_by(householdId) |>
    #     dplyr::summarise(dplyr::across(dplyr::all_of(c(MNList, "Energy")), ~ sum(.x / 100 * amountConsumedInG,na.rm = TRUE), .names = "{.col}_BaseSupply")) |>
    #     dplyr::left_join(hhFortificantConsumptionInG) |>
    #     # dplyr::left_join(hhFortificantConsumptionInG)|>
    #     dplyr::bind_cols(cndThreshholds)

    # calculate LSFF Supply
    for (nutrient in MNList) {
        for (year in years) {
            # Calculate the supply of the nutrient with LSFF per food item
            enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_LSFFSupply")] <- enrichedHouseholdConsumption["amountConsumedInG"] * yearAverageFortificationLevel(fortification_vehicle = foodVehicleName, Year = year, MN = nutrient, fortificationLevels = fortificationLevelsDf) * enrichedHouseholdConsumption["fortifiable_portion"] / 100

            enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_TotalBaseAndLSFFSupply")] <- enrichedHouseholdConsumption[paste0(nutrient, "_BaseSupply")] + enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_LSFFSupply")]
        }
    }

    # TODO: There is something wrong here. Fix it.
    # Sum up the food item supplies to Household level
    enrichedHouseholdConsumption <- enrichedHouseholdConsumption |>
        dplyr::group_by(householdId) |>
        dplyr::summarize(
            dplyr::across(dplyr::ends_with("_BaseSupply"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
            dplyr::across(dplyr::ends_with("_LSFFSupply"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
            dplyr::across(dplyr::ends_with("_TotalBaseAndLSFFSupply"), ~ sum(.x, na.rm = TRUE), .names = "{.col}")
        )

    medianNutrientSupplySummaries <- enrichedHouseholdConsumption |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            dplyr::across(dplyr::ends_with("_BaseSupply"), ~ round(mean(.x, na.rm = TRUE), 0), .names = "{.col}MeanSupply"),
            dplyr::across(dplyr::ends_with("_BaseSupply"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianSupply")
            # TOD0: Add the same for LSFFSupply
        )

    # # Per food item apparent intake and CND
    # foodLevelenrichedEneryApparentIntake <- enrichedHouseholdConsumption |>
    #     dplyr::mutate(dplyr::across(dplyr::all_of(c(MNList, "Energy")), ~ sum(.x / 100 * perDayAmountConsumedInGPerCapita, na.rm = TRUE), .names = "{.col}DailyApparentIntake"))

    # Calculate baseline nutrient density and adequacy
    for (nutrient in MNList) {
        if (nutrient != "Energy") {
            enrichedHouseholdConsumption[[paste0(nutrient, "_baseline_cnd")]] <- enrichedHouseholdConsumption[[paste0(nutrient, "_BaseSupply")]] * 1000 / enrichedHouseholdConsumption[["Energy_BaseSupply"]]

            if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"))) {
                enrichedHouseholdConsumption[[paste0(nutrient, "_cnd_inadequacy")]] <- ifelse(enrichedHouseholdConsumption[[paste0(nutrient, "_baseline_cnd")]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"), 0, 1)
            }

            if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL"))) {
                enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_Base_cul_exceedence")] <- ifelse(enrichedHouseholdConsumption[paste0(nutrient, "_baseline_cnd")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL"), 1, 0)
            }
        }

        for (year in years) {
            enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_BaseAndLSFF_cnd")]] <- enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_TotalBaseAndLSFFSupply")]] * 1000 / enrichedHouseholdConsumption[[paste0("Energy_BaseSupply")]]

            if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"))) {
                enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_BaseAndLSFF_cnd_inadequacy")]] <- ifelse(enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_BaseAndLSFF_cnd")]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"), 0, 1)
            }

            if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL"))) {
                enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_BaseAndLSFF_cul_exceedence")] <- ifelse(enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_BaseAndLSFF_cnd")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL"), 1, 0)
            }
        }
    }




    # Summarize prevalence of inadequacy
    inadequacySummarries <- enrichedHouseholdConsumption |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            dplyr::across(dplyr::ends_with("_cnd_inadequacy"), ~ sum(.x, na.rm = TRUE), .names = "{.col}_count"),
            dplyr::across(dplyr::ends_with("_cul_exceedence"), ~ sum(.x, na.rm = TRUE), .names = "{.col}_count")
        ) |>
        dplyr::left_join(initialSummaries) |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_count"), ~ round((.x * 100 / householdsCount), 2), .names = "{.col}_perc"))

    # # Calculate nutrient supplies
    #     for (nutrient in MNList) {
    #         enrichedHouseholdConsumption[paste0(nutrient, "_BaseSupply")] <- enrichedHouseholdConsumption[nutrient] / 100 * enrichedHouseholdConsumption["perDayAmountConsumedInGPerCapita"]

    #         for (year in years) {
    #             # Calculate the supply of the nutrient with LSFF per food item
    #             enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_LSFFSupply")] <- enrichedHouseholdConsumption[paste0(nutrient, "_BaseSupply")] * yearAverageFortificationLevel(fortification_vehicle = foodVehicleName, Year = year, MN = nutrient) * enrichedHouseholdConsumption["fortifiable_portion"] / 100
    #         }
    #     }

    # # Aggregate nutrient supplies by household
    # nutrientSupply <- enrichedHouseholdConsumption |>
    #     dplyr::group_by(householdId) |>
    #     dplyr::summarize(
    #         dplyr::across(dplyr::ends_with("_BaseSupply"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
    #         dplyr::across(dplyr::ends_with("_LSFFSupply"), ~ sum(.x, na.rm = TRUE), .names = "{.col}")
    #     )

    # Calculate mean and median nutrient supplies
    # # TODO: These were checked and are consistent with the maps tool.
    # medianNutrientSupplySummaries <- nutrientSupply |>
    #     dplyr::left_join(householdDetailsDf) |>
    #     dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #     dplyr::summarize(
    #         dplyr::across(dplyr::ends_with("_BaseSupply"), ~ round(mean(.x, na.rm = TRUE), 0), .names = "{.col}MeanSupply"),
    #         dplyr::across(dplyr::ends_with("_BaseSupply"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianSupply")
    #     )

    # # Add _BaseSupply and _LSFFSupply for each nutrient and year combo
    # for (nutrient in MNList) {
    #     for (year in years) {
    #         nutrientSupply[paste0(nutrient, "_", year, "_BaseAndLSFFTotalSupply")] <- nutrientSupply[paste0(nutrient, "_BaseSupply")] + nutrientSupply[paste0(nutrient, "_", year, "_LSFFSupply")]
    #     }
    # }

    # # Calculate CND for each nutrient and year combo

    # # Remerge the household details
    # enrichedNutrientSupply <- nutrientSupply |>
    #     dplyr::left_join(householdDetailsDf) |>
    #     dplyr::bind_cols(earThreshholds)

    # # Create adequacy columns for each Baseline and LSFF nutrient supply
    # for (nutrient in MNList) {
    #     if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"))) {
    #         enrichedNutrientSupply[paste0(nutrient, "_base_supply_ear_inadequacy")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_BaseSupply")] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"), 0, 1)
    #     }
    #     for (year in years) {
    #         if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"))) {
    #             enrichedNutrientSupply[paste0(nutrient, "_", year, "_base_and_lsff_ear_inadequacy")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_", year, "_BaseAndLSFFTotalSupply")] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"), 0, 1)
    #         }
    #     }
    # }

    # # Check if the intake is above the Upper Limit
    # for (nutrient in MNList) {
    #     if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"))) {
    #         enrichedNutrientSupply[paste0(nutrient, "_base_ul_exceedance")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_BaseSupply")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"), 1, 0)
    #     }
    #     for (year in years) {
    #         if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"))) {
    #             enrichedNutrientSupply[paste0(nutrient, "_", year, "_base_and_lsff_ul_exceedance")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_", year, "_BaseAndLSFFTotalSupply")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"), 1, 0)
    #         }
    #     }
    # }

    # Create adequacy summaries
    # inadequacySummarries <- enrichedNutrientSupply |>
    #     dplyr::left_join(householdDetailsDf) |>
    #     dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #     dplyr::summarize(
    #         dplyr::across(dplyr::ends_with("_base_supply_ear_inadequacy"), ~ sum(.x, na.rm = TRUE), .names = "{.col}_count"),
    #         dplyr::across(dplyr::ends_with("_base_ul_exceedance"), ~ sum(.x, na.rm = TRUE), .names = "{.col}_count")
    #     ) |>
    #     dplyr::left_join(initialSummaries) |>
    #     dplyr::mutate(dplyr::across(dplyr::ends_with("_count"), ~ round((.x * 100 / householdsCount), 2), .names = "{.col}_perc"))

    # Get the column order for the data
    columnOrder <- sort(names(inadequacySummarries))

    # Reorder the columns for better readability
    finalSummarries <- inadequacySummarries |>
        dplyr::select(dplyr::all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, householdsCount, dplyr::everything())

    return(finalSummarries)
}
