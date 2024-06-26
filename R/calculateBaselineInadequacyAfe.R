#' Calculate Baseline Nutrient Inadequacy (AFE Method)
#'
#' This function calculates the baseline inadequacy of nutrients for different administrative groups using the Adequate Food Energy (AFE) Method.
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
calculateBaselineInadequacyAfe <- function(
    householdConsumptionDf = householdConsumption,
    householdDetailsDf = householdDetails,
    nctListDf = nctList,
    intakeThresholdsDf = intakeThresholds,
    aggregationGroup = c("admin0Name", "admin1Name"),
    MNList = c("Ca", "Carbohydrates", "Cu", "Energy", "Fat", "Fe", "Fibre", "I", "IP6", "Mg", "Protein", "Se", "Zn", "Ash", "B6", "B2", "D", "N", "K", "P", "Moisture", "Cholesterol", "E", "Na", "A", "C", "B12", "B1", "B3", "B9", "B5", "B7", "Mn")) {
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
        # Convert all columns needed for calculations to numeric
        dplyr::mutate_at(c("amountConsumedInG", "afeFactor", MNList), as.numeric) |>
        # Calculate the amount consumed per AFE by dividing the amount consumed by the AFE factor
        # dplyr::mutate(amountConsumedInGPerAfe = amountConsumedInG / afeFactor) |>
        # Balance consumption over the recall period i.e. 7 days for this data
        # dplyr::mutate(amountConsumedInGPerAfe = amountConsumedInG / 7) |> # Trying without this line
        # Calculate the MNs content by multiplying the amount consumed per AFE by the nutrient composition for each nutrient
        # TODO: This is the possible source of the errors. Check the calculation formula
        # NOTE: Update 28/02/2024: The micronutrient composition is in 100g of the food item. We need to divide by 100 to get the nutrient content per gram see Andy's email
        dplyr::mutate(dplyr::across(MNList, ~ . / afeFactor)) |> # NOTE This is a trial of Andy's suggestion
        # Group the data by householdId so that we can summarise the data at the household level
        dplyr::group_by(householdId) |>
        # Summarise the data to get the total nutrient intake per afe per day for each household and name the columns with the nutrient name and "Supply"
        # dplyr::summarize(dplyr::across(all_of(MNList), ~ sum(.x, na.rm = TRUE), .names = "{.col}Supply")) |> # TODO: Divide by the afe factor here
        # NOTE: Trial of Andy's suggestion
        dplyr::summarize(dplyr::across(dplyr::all_of(MNList), ~ sum(.x / 100 * amountConsumedInG, na.rm = TRUE), .names = "{.col}Supply")) |> # TODO: Divide by the afe factor here
        # The summaries remove the household details so we need to join them back to the data
        dplyr::left_join(householdDetailsDf) |>
        # Bind thresholds to the data. The thresholds data has one row so it should be recycled to the number of rows in the data
        dplyr::bind_cols(earThreshholds)

    # Create adequacy columns for each nutrient
    # NOTE: This code is not pretty and can be improved. It works for now
    for (nutrient in MNList) {
        # Create the supply column for each nutrient
        supply_col <- paste0(nutrient, "Supply")
        # Create the threshold column for each nutrient
        threshold_col <- paste0(nutrient, "SupplyEarThreshold") # Adjust if the naming convention is different

        # Create the adequacy column for each nutrient to store the adequacy of the nutrient supply
        adequacy_col <- paste0(nutrient, "EarAdequacy")

        # Only create the adequacy column if enrichedHouseholdConsumption[[threshold_col]] is not NA.
        if (!is.na(effectivenessCalculations::getMnThresholds(intakeThresholds, nutrient, "ear"))) {
            enrichedHouseholdConsumption[[adequacy_col]] <- ifelse(enrichedHouseholdConsumption[[supply_col]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"), 1, 0)
        }
    }

    # Prevalence of Inadequacy Summaries
    # Households count summaries
    statsHouseholdCount <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(households = dplyr::n())

    ## Count Adequate and Inadequate
    statsCountAdequateHH <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("Adequacy"), ~ sum(.x, na.rm = TRUE), .names = "{.col}AdeCount"))

    ## Count Inadequate
    statsCountInadequateHH <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("Adequacy"), ~ sum(1 - .x, na.rm = TRUE), .names = "{.col}InadeCount"))

    # Percentage Inadequate
    statsPercentageInadequate <- statsHouseholdCount |>
        dplyr::left_join(statsCountAdequateHH) |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("AdeCount"), ~ round(100 - (.x * 100 / households), 2), .names = "{.col}PercInadequate"))

    ## Median Supply
    statsMedianSupply <- enrichedHouseholdConsumption |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(dplyr::across(dplyr::ends_with("Supply"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianSupply"))

    ## baselineAdequacyPrevalence
    # Merge the stats data into one dataframe
    baselineAdequacyPrevalence <- statsHouseholdCount |>
        dplyr::left_join(statsCountInadequateHH) |>
        dplyr::left_join(statsPercentageInadequate) |>
        dplyr::left_join(statsMedianSupply) |>
        dplyr::bind_cols(earThreshholds)

    # Get the column order for the data
    columnOrder <- sort(names(baselineAdequacyPrevalence))

    # Reorder the columns for better readability
    baselineAdequacyPrevalence <- baselineAdequacyPrevalence |>
        dplyr::select(dplyr::all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, households, dplyr::everything())

    return(baselineAdequacyPrevalence)
}
