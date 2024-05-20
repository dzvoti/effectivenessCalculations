#' Calculate Baseline Nutrient Inadequacy (Nutrient Density Method)
#'
#' This function calculates the baseline inadequacy of nutrients for different administrative groups using the Nutrient Density Method.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data.
#' @param householdDetailsDf A dataframe containing household details.
#' @param nctListDf A dataframe containing nutrient composition tables.
#' @param intakeThresholdsDf A dataframe containing intake thresholds for nutrients.
#' @param aggregationGroup A character vector of administrative groups to aggregate the data. Must not be empty. Defaults to c("admin0Name", "admin1Name").
#' @param MNList A character vector of nutrients to be included in the analysis. If empty, defaults to a comprehensive list of nutrients.
#'
#' @return A dataframe with the baseline inadequacy of nutrients for the specified administrative groups.
#' @export
#'
#' @examples
#' calculateBaselineInadequacyCND(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     nctListDf = nctList,
#'     intakeThresholdsDf = intakeThresholds,
#'     aggregationGroup = c("admin0Name", "admin1Name"),
#'     MNList = c("Ca", "Carbohydrates")
#' )
calculateBaselineInadequacyCND <- function(
    householdConsumptionDf = householdConsumption,
    householdDetailsDf = householdDetails,
    nctListDf = nctList,
    intakeThresholdsDf = intakeThresholds,
    aggregationGroup = c("admin0Name", "admin1Name"),
    MNList = c("Ca", "Carbohydrates", "Cu", "Energy", "Fat", "Fe", "Fibre", "I", "IP6", "Mg", "Protein", "Se", "Zn", "Ash", "B6", "B2", "D", "N", "K", "P", "Moisture", "Cholesterol", "E", "Na", "A", "C", "B12", "B1", "B3", "B9", "B5", "B7", "Mn")) {
    # Check if MNList is a character vector
    requiredConsumptionCols <- c("householdId", "amountConsumedInG")
    requiredDetailsCols <- c("householdId", "memberCount")
    requiredNctCols <- c("micronutrientId")
    requiredIntakeCols <- c("nutrient", "CND")

    # Check if MNList is a character vector
    if (!is.character(MNList)) {
        stop("MNList must be a character vector")
    }

    # Check if aggregationGroup is a character vector
    if (!is.character(aggregationGroup)) {
        stop("aggregationGroup must be a character vector")
    }

    # Check if MNList and aggregationGroup are not empty
    if (length(aggregationGroup) == 0) {
        stop("aggregationGroup cannot be empty")
    }

    # Check if required columns are present in the dataframes
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
    masterNCT <- effectivenessCalculations::createMasterNct(nctListDf)

    ## Create a wider format for the intakeThresholds
    cndThreshholds <- intakeThresholdsDf |>
        dplyr::select(nutrient, CND) |>
        dplyr::filter(!is.na(CND)) |>
        dplyr::filter(nutrient %in% MNList) |>
        tidyr::pivot_wider(names_from = nutrient, values_from = CND) |>
        dplyr::mutate_all(as.numeric) |>
        dplyr::rename_with(~ paste0(., "ApparentIntakeCNDThreshold"), dplyr::everything())

    # Process the consumption data
    enrichedHouseholdConsumption <- householdConsumptionDf |>
        tibble::as_tibble() |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::left_join(masterNCT) |>
        dplyr::mutate_at(c("amountConsumedInG", "afeFactor", "memberCount", MNList, "Energy"), as.numeric) |>
        dplyr::mutate(averagePerDayAmountConsumed = amountConsumedInG / memberCount) |>
        dplyr::group_by(householdId) |>
        dplyr::summarize(dplyr::across(dplyr::all_of(c(MNList, "Energy")), ~ sum(.x / 100 * averagePerDayAmountConsumed, na.rm = TRUE), .names = "{.col}DailyApparentIntake")) |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::bind_cols(cndThreshholds)

    # Calculate baseline nutrient density and adequacy
    for (nutrient in MNList) {
        if (nutrient != "Energy") {
            baselineNDcol <- paste0(nutrient, "BaselineND")
            thresholdCol <- paste0(nutrient, "ApparentIntakeCNDThreshold")
            adequacyCol <- paste0(nutrient, "CNDAdequacy")

            enrichedHouseholdConsumption[[baselineNDcol]] <- enrichedHouseholdConsumption[[paste0(nutrient, "DailyApparentIntake")]] * 1000 / enrichedHouseholdConsumption[["EnergyDailyApparentIntake"]]

            if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"))) {
                enrichedHouseholdConsumption[[adequacyCol]] <- ifelse(enrichedHouseholdConsumption[[baselineNDcol]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"), 1, 0)
            }
        }
    }

    # Summarize prevalence of inadequacy
    statsHouseholdCount <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(households = dplyr::n())

    statsCountAdequateHH <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("CNDAdequacy"), ~ sum(.x, na.rm = TRUE), .names = "{.col}AdeCount"))

    statsCountInadequateHH <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("CNDAdequacy"), ~ sum(1 - .x, na.rm = TRUE), .names = "{.col}InadeCount"))

    statsPercentageInadequate <- statsHouseholdCount |>
        dplyr::left_join(statsCountAdequateHH) |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("CNDAdequacyInadeCount"), ~ round(100 - (.x * 100 / households), 2), .names = "{.col}PercInadequate"))

    statsMedianSupply <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("DailyApparentIntake"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianDailyApparentIntake"))

    baselineAdequacyPrevalence <- statsHouseholdCount |>
        dplyr::left_join(statsCountInadequateHH) |>
        dplyr::left_join(statsPercentageInadequate) |>
        dplyr::left_join(statsMedianSupply) |>
        dplyr::bind_cols(cndThreshholds)

    columnOrder <- sort(names(baselineAdequacyPrevalence))

    baselineCNDAdequacyPrevalence <- baselineAdequacyPrevalence |>
        dplyr::select(dplyr::all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, households, dplyr::everything())

    return(baselineCNDAdequacyPrevalence)
}
