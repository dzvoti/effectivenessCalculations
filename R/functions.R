#' Load CSV Files
#'
#' This function loads all CSV files from a specified folder into the global environment.
#' Each CSV file is read into a data frame, its column names are cleaned, and it is
#' assigned to a variable named after the file (without the .csv extension).
#'
#' @param folder_path A string. The path to the folder containing the CSV files.
#'
#' @examples
#' loadCsvFiles("path/to/your/csvfiles")
#'
#' @return None. This function does not return a value; it assigns variables in the global environment.
loadCsvFiles <- function(folder_path) {
    file_paths <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

    # Iterate over each file path
    for (file_path in file_paths) {
        # Extract the base name without the extension
        file_name <- tools::file_path_sans_ext(basename(file_path))

        # Read the CSV file and clean names
        data <- readr::read_csv(file_path) |>
            janitor::clean_names()

        # Assign it to a dynamically named variable in the global environment
        assign(file_name, data, envir = .GlobalEnv)
    }
}

#' Load RDA Files from a Directory
#'
#' This function lists all `.rda` files in a specified directory and loads them into the global environment.
#'
#' @param dir A character string specifying the directory from which to load the `.rda` files. Defaults to `"data/sd123"`.
#' @return This function does not return a value. It loads the `.rda` files into the global environment.
#' @examples
#' \dontrun{
#' # Load .rda files from the default directory
#' load_rda_files()
#'
#' # Load .rda files from a custom directory
#' load_rda_files("your/custom/path")
#' }
#' @export
load_rda_files <- function(dir = "data/sd123") {
    # List .rda files in the specified directory
    rdaFiles <- list.files(dir, pattern = "\\.rda$", full.names = TRUE)

    # Load the .rda files into the global environment
    for (rdaFile in rdaFiles) {
        load(here::here(rdaFile), envir = .GlobalEnv)
    }
}




#' Check if Required Data is Loaded
#'
#' This function checks if the required data ("householdConsumption", "householdDetails", and "nctList") is loaded in the global environment.
#'
#' @return A logical value indicating whether the required data is loaded. Returns TRUE if all required data is loaded, and FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' checkData()
#' }
#'
#' @export
checkData <- function() {
    # Check if the data is loaded
    if (exists("householdConsumption") && exists("householdDetails") && exists("nctList")) {
        return(TRUE)
    } else {
        print("All expected data not loaded, please load data first.")
        return(FALSE)
    }
}



#' Enforce Numeric Data Type for Specified Columns
#'
#' This function converts specified columns of a dataframe to numeric data type.
#'
#' @param df A dataframe.
#' @param cols A vector of column names that should be converted to numeric.
#'
#' @return A dataframe with the specified columns converted to numeric.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = c("1", "2", "3"), b = c("4", "5", "6"))
#' df <- enforceNumeric(df, c("a", "b"))
#' }
#'
#' @export
enforceNumeric <- function(df, cols) {
    df <- df |> dplyr::mutate_at(cols, as.numeric)
    return(df)
}



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

#' Get Micronutrient Thresholds
#'
#' This function retrieves the thresholds for a specified micronutrient from a dataframe of intake thresholds.
#'
#' @param intakeThresholds A dataframe containing intake thresholds for various micronutrients.
#' @param Mn A string specifying the micronutrient to retrieve thresholds for.
#' @param param A string specifying the parameter to retrieve. Can be "ear", "ul", or "unitAdequacy". Default is "ear".
#'
#' @return A list containing the thresholds for the specified micronutrient. If param is "ul" or "unitAdequacy", a single value is returned.
#'
#' @examples
#' \dontrun{
#' intakeThresholds <- data.frame(nutrient = c("vitaminA", "vitaminB", "vitaminC"), ear = c(0.5, 0.6, 0.7), ul = c(1, 1.2, 1.3), unitAdequacy = c(0.8, 0.9, 1))
#' thresholds <- getMnThresholds(intakeThresholds, "vitaminA", "ul")
#' }
#'
#' @export
getMnThresholds <- function(intakeThresholds, nutrient, param = "ear") {
    threshold <- as.numeric(intakeThresholds[intakeThresholds$nutrient == nutrient, param])
    return(threshold)
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
#' calculateBaselineInadequacyAfe(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     nctListDf = nctList,
#'     intakeThresholdsDf = intakeThresholds,
#'     aggregationGroup = c("admin0Name", "admin1Name"),
#'     MNList = c("Ca", "Carbohydrates")
#' )
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
#' @export
createMasterNct <- function(nctList, fctListIdCol = "fctListId", foodGenusIdCol = "foodGenusId", micronutrientIdCol = "micronutrientId", micronutrientCompositionCol = "micronutrientComposition") {
    # if (checkData()) {
    masterNct <- nctList |>
        dplyr::select(fctListIdCol, foodGenusIdCol, micronutrientIdCol, micronutrientCompositionCol) |>
        tidyr::pivot_wider(names_from = micronutrientIdCol, values_from = micronutrientCompositionCol)
    # }
    return(masterNct)
}


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

#' Calculate Food Vehicle Household Reach
#'
#' This function calculates the household reach for each food vehicle. It filters the fortifiable food items to get the food vehicle, processes the consumption data, and then calculates the household reach.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "foodGenusId", "householdId".
#' @param householdDetailsDf A dataframe containing household details. Must contain column: "householdId".
#' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items. Must contain columns: "food_genus_id", "food_vehicle_name".
#' @param foodVehicleName A string specifying the food vehicle name. Default is "wheat flour".
#' @param aggregationGroup A character vector specifying the columns to group by. Default is `c("admin0Name", "admin1Name")`.
#'
#' @return A dataframe with the household reach for the specified food vehicle.
#'
#' @examples
#' calculateFoodVehicleHouseholdReach(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     fortifiableFoodItemsDf = fortifiable_food_items,
#'     foodVehicleName = "wheat flour",
#'     aggregationGroup = c("admin0Name", "admin1Name")
#' )
#'
#' @export
calculateFoodVehicleHouseholdReach <- function(householdConsumptionDf = householdConsumption, householdDetailsDf = householdDetails, fortifiableFoodItemsDf = fortifiable_food_items, foodVehicleName = "wheat flour", aggregationGroup = c("admin0Name", "admin1Name")) {
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
    requiredConsumptionCols <- c("foodGenusId", "householdId")
    requiredDetailsCols <- c("householdId")
    requiredFortifiableCols <- c("food_genus_id", "food_vehicle_name")

    # Check if required columns exist in the dataframes
    if (!all(requiredConsumptionCols %in% colnames(householdConsumptionDf))) {
        stop(paste("householdConsumptionDf must contain the following columns:", paste(requiredConsumptionCols, collapse = ", ")))
    }

    if (!all(requiredDetailsCols %in% colnames(householdDetailsDf))) {
        stop(paste("householdDetailsDf must contain the following column:", paste(requiredDetailsCols, collapse = ", ")))
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
        dplyr::arrange(desc(food_vehicle_name)) |>
        dplyr::distinct(householdId, .keep_all = TRUE) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(households = dplyr::n(), foodVehicleReachHH = sum(!is.na(food_vehicle_name)), foodVehicleReachHHPerc = round(foodVehicleReachHH / households * 100, 0))

    return(foodVehicleHouseholdReach)
}

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
#' calculateBaselineInadequacyAfe(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     nctListDf = nctList,
#'     intakeThresholdsDf = intakeThresholds,
#'     aggregationGroup = c("admin0Name", "admin1Name"),
#'     MNList = c("Ca", "Carbohydrates")
#' )
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

    baselineAdequacyPrevalence <- statsHouseholdCount |>
        dplyr::left_join(statsCountHighIntakeRisk) |>
        dplyr::left_join(statsPercentageHighIntakeRisk) |>
        dplyr::left_join(statsMedianSupply) |>
        dplyr::bind_cols(upperIntakeLevel)

    columnOrder <- sort(names(baselineAdequacyPrevalence))

    baselineAdequacyPrevalence <- baselineAdequacyPrevalence |>
        dplyr::select(dplyr::all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, households, dplyr::everything())

    return(baselineAdequacyPrevalence)
}


