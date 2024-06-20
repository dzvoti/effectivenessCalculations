#' Calculate Baseline High Intake Risk of Nutrients (AFE Method)
#'
#' This function calculates the baseline risk of high nutrient intake for different administrative groups using the Adequate Food Energy (AFE) Method. It compares daily apparent micronutrient intake per AFE to the tolerable upper intake levels (UL) to classify household diets as at risk of high intake. The results are summarized at national and subnational levels.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "householdId", "amountConsumedInG", "memberCount".
#' @param householdDetailsDf A dataframe containing household details. Must contain column: "householdId".
#' @param nctListDf A dataframe containing nutrient composition tables. Must contain columns: "nutrient", "foodId".
#' @param intakeThresholdsDf A dataframe containing intake thresholds for nutrients. Must contain columns: "nutrient", "CND".
#' @param aggregationGroup A character vector of administrative groups to aggregate the data. Must not be empty. Defaults to c("admin0Name", "admin1Name").
#' @param MNList A character vector of nutrients to be included in the analysis. If empty, defaults to a comprehensive list of nutrients.
#'
#' @return A dataframe with the baseline inadequacy of nutrients for the specified administrative groups.
#' @export
#'
#' @examples
#' \dontrun{
#' calculateBaselineInadequacyAfe(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     nctListDf = nctList,
#'     intakeThresholdsDf = intakeThresholds,
#'     aggregationGroup = c("admin0Name", "admin1Name"),
#'     MNList = c("Ca", "Carbohydrates")
#' )
#' }
calculateBaselinePrevalenceHighIntakeRiskAfe <- function(
    householdConsumptionDf = householdConsumption,
    householdDetailsDf = householdDetails,
    nctListDf = nctList,
    intakeThresholdsDf = intakeThresholds,
    aggregationGroup = c("admin0Name", "admin1Name"),
    MNList = c("Ca", "Carbohydrates", "Cu", "Energy", "Fat", "Fe", "Fibre", "I", "IP6", "Mg", "Protein", "Se", "Zn", "Ash", "B6", "B2", "D", "N", "K", "P", "Moisture", "Cholesterol", "E", "Na", "A", "C", "B12", "B1", "B3", "B9", "B5", "B7", "Mn")) {
    requiredConsumptionCols <- c("householdId", "amountConsumedInG")
    requiredDetailsCols <- c("householdId", "memberCount")
    requiredNctCols <- c("micronutrientId")
    requiredIntakeCols <- c("nutrient", "ul")

    if (!is.character(MNList)) {
        stop("MNList must be a character vector e.g. c('A', 'Ca')")
    }

    if (!is.character(aggregationGroup)) {
        stop("aggregationGroup must be a character vector e.g. c('admin0Name', 'admin1Name')")
    }

    if (length(aggregationGroup) == 0) {
        stop("aggregationGroup cannot be empty")
    }

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

    masterNCT <- effectivenessCalculations::createMasterNct(nctList)

    upperIntakeLevel <- intakeThresholdsDf |>
        dplyr::select(nutrient, ul) |>
        dplyr::filter(!is.na(ul)) |>
        dplyr::filter(nutrient %in% MNList) |>
        tidyr::pivot_wider(names_from = nutrient, values_from = ul) |>
        dplyr::mutate_all(as.numeric) |>
        dplyr::rename_with(~ paste0(., "SupplyAfeUpperIntakeLevel"), dplyr::everything())

    enrichedHouseholdConsumption <- householdConsumptionDf |>
        tibble::as_tibble() |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::left_join(masterNCT) |>
        dplyr::mutate_at(c("amountConsumedInG", "afeFactor", MNList), as.numeric) |>
        dplyr::mutate(dplyr::across(MNList, ~ . / afeFactor)) |>
        dplyr::group_by(householdId) |>
        dplyr::summarize(dplyr::across(dplyr::all_of(MNList), ~ sum(.x / 100 * amountConsumedInG, na.rm = TRUE), .names = "{.col}Supply")) |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::bind_cols(upperIntakeLevel)

    for (nutrient in MNList) {
        supply_col <- paste0(nutrient, "Supply")
        threshold_col <- paste0(nutrient, "AfeUpperIntakeLevel")
        adequacy_col <- paste0(nutrient, "HighIntakeRisk")

        if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholds, nutrient, "ul"))) {
            enrichedHouseholdConsumption[[adequacy_col]] <- ifelse(enrichedHouseholdConsumption[[supply_col]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"), 1, 0)
        }
    }

    statsHouseholdCount <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(households = dplyr::n())

    statsCountHighIntakeRisk <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("HighIntakeRisk"), ~ sum(.x, na.rm = TRUE), .names = "{.col}HighIntakeRiskCount"))

    statsCountLowIntakeRisk <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("HighIntakeRisk"), ~ sum(1 - .x, na.rm = TRUE), .names = "{.col}LowIntakeRiskCount"))

    statsPercentageHighIntakeRisk <- statsHouseholdCount |>
        dplyr::left_join(statsCountHighIntakeRisk) |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("HighIntakeRiskCount"), ~ round((.x * 100 / households), 2), .names = "{.col}PercHighIntakeRisk"))

    statsMedianSupply <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("Supply"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianSupply"))

    baselineHighIntakeRisk <- statsHouseholdCount |>
        dplyr::left_join(statsCountHighIntakeRisk) |>
        dplyr::left_join(statsPercentageHighIntakeRisk) |>
        dplyr::left_join(statsMedianSupply) |>
        dplyr::bind_cols(upperIntakeLevel)

    columnOrder <- sort(names(baselineHighIntakeRisk))

    baselineHighIntakeRisk <- baselineHighIntakeRisk |>
        dplyr::select(dplyr::all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, households, dplyr::everything())

    return(baselineHighIntakeRisk)
}
