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

#' Load all .rda files in a directory
#'
#' This function loads all .rda files in the specified directory into the global environment.
#'
#' @param dir A character string specifying the directory to load .rda files from.
#'
#' @return Invisible NULL. This function is called for its side effect of loading .rda files into the global environment.
#'
#' @examples
#' \dontrun{
#' load_all_rda_files("data/sd123")
#' }
#'
#' @export
load_all_rda_files <- function(dir) {
    # Get all .rda files in the dir
    rda_files <- list.files(dir, pattern = "\\.rda$", full.names = TRUE)

    # Load all .rda files
    lapply(rda_files, load, .GlobalEnv)
}

#' Load all .rda files in a directory
#'
#' This function loads all .rda files in the specified directory into the global environment.
#'
#' @param dir A character string specifying the directory to load .rda files from.
#'
#' @return Invisible NULL. This function is called for its side effect of loading .rda files into the global environment.
#'
#' @examples
#' \dontrun{
#' load_all_rda_files("data/sd123")
#' }
#'
#' @export
loadMapsRdaTables <- function(dir) {
    # Get all .rda files in the dir
    rda_files <- list.files(dir, pattern = "\\.rda$", full.names = TRUE)

    # Initialize an empty list to store the data
    data <- list()

    # Load all .rda files into the list
    for (file in rda_files) {
        # Get the name of the file without the extension
        name <- tools::file_path_sans_ext(basename(file))

        # Load the file into a temporary environment
        e <- new.env()
        load(file, envir = e)

        # Add the loaded data to the list
        data[[name]] <- e[[ls(e)[1]]] # Just store the first object from the environment
    }

    return(data) # Return data but make the return invisible
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


#' Calculate Baseline Nutrient Inadequacy
#'
#' This function calculates the baseline inadequacy of nutrients for different administrative groups.
#'
#' @param MNList A character vector of nutrients. If empty, defaults to a list of all nutrients.
#' @param aggregationGroup A character vector of administrative groups. Must not be empty.
#' @param dataDir The directory where the data is stored.
#'
#' @return A dataframe with the baseline inadequacy of nutrients for the specified administrative groups.
#' @export
#'
#' @examples
#' calculateBaselineInadequacy(MNList = c("A", "Ca"))
calculateBaselineInadequacy <- function(MNList = c("A"), aggregationGroup = c("admin0Name", "admin1Name"), dataDir = here::here("data/sd123")) {
    # Check if MNList is a character vector
    if (!is.character(MNList)) {
        stop("MNList must be a character vector")
    }

    # check if aggregationGroup is a character vector
    if (!is.character(aggregationGroup)) {
        stop("aggregationGroup must be a character vector")
    }

    # Check if MNLIst and aggregationGroup are not empty
    if (length(aggregationGroup) == 0) {
        stop("aggregationGroup cannot be empty")
    }

    # Check if dataDir exists
    if (!dir.exists(dataDir)) {
        stop("The specified dataDir does not exist")
    }

    # Load the data
    data <- loadMapsRdaTables(dataDir)

    # Check if all the required data is loaded in the data list
    if (!all(c("householdConsumption", "householdDetails", "nctList", "intakeThresholds") %in% names(data))) {
        # Explain which data is missing
        missingData <- c("householdConsumption", "householdDetails", "nctList", "intakeThresholds")[!(c("householdConsumption", "householdDetails", "nctList", "intakeThresholds") %in% names(data))]
        stop(paste("The following data is missing:", paste(missingData, collapse = ", ")))
    }

    if (length(MNList) == 0) {
        # Default to the list of all nutrients
        MNList <- c(
            "Ca",
            "Carbohydrates",
            "Cu",
            "Energy",
            "Fat",
            "Fe",
            "Fibre",
            "I",
            "IP6",
            "Mg",
            "Protein",
            "Se",
            "Zn",
            "Ash",
            "B6",
            "B2",
            "D",
            "N",
            "K",
            "P",
            "Moisture",
            "Cholesterol",
            "E",
            "Na",
            "A",
            "C",
            "B12",
            "B1",
            "B3",
            "B9",
            "B5",
            "B7",
            "Mn"
        )
        # Tell the user that the default list is being used
        message("No MNList provided. Using the default list of all nutrients")
    }



    # Extract the data from the list
    householdConsumption <- data$householdConsumption
    householdDetails <- data$householdDetails
    nctList <- data$nctList
    intakeThresholds <- data$intakeThresholds

    # Use the createMasterNct function to create a master NCT
    masterNCT <- createMasterNct(nctList)

    ## Create a wider format for the intakeThresholds

    # Create a wider format for the intakeThresholds
    earThreshholds <- intakeThresholds |>
        dplyr::select(nutrient, ear) |>
        # Remove rows where ear is NA
        dplyr::filter(!is.na(ear)) |>
        # Leave thresholds for the nutrients in the MNList
        dplyr::filter(nutrient %in% MNList) |>
        tidyr::pivot_wider(names_from = nutrient, values_from = ear) |>
        # Convert all columns to numeric
        dplyr::mutate_all(as.numeric) |>
        # Add a suffix of "ear" to the column names
        dplyr::rename_with(~ paste0(., "SupplyEarThreshold"), everything())

    # Process the consumption data

    # Load the consumption data
    enrichedHouseholdConsumption <- householdConsumption |>
        # Not necessary by its a personal preference
        tibble::as_tibble() |>
        # Join the household details to the consumption data (Joining columns with the same name)
        dplyr::left_join(householdDetails) |>
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
        dplyr::summarize(dplyr::across(all_of(MNList), ~ sum(.x / 100 * amountConsumedInG, na.rm = TRUE), .names = "{.col}Supply")) |> # TODO: Divide by the afe factor here
        # The summaries remove the household details so we need to join them back to the data
        dplyr::left_join(householdDetails) |>
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
        if (!is.na(getMnThresholds(intakeThresholds, nutrient, "ear"))) {
            enrichedHouseholdConsumption[[adequacy_col]] <- ifelse(enrichedHouseholdConsumption[[supply_col]] >= getMnThresholds(intakeThresholds, nutrient, "ear"), 1, 0)
        }
    }





    # Prevalence of Inadequacy Summaries
    ## Households count summaries

    # TODO: Start HERE on 27/02/2024
    # Households count summaries
    statsHouseholdCount <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(households = dplyr::n())



    ## Count Adequate and Inadequate

    statsCountAdequateHH <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(dplyr::across(dplyr::ends_with("Adequacy"), ~ sum(.x, na.rm = TRUE), .names = "{.col}AdeCount"))

    ## Count Inadequate
    statsCountInadequateHH <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(dplyr::across(dplyr::ends_with("Adequacy"), ~ sum(1 - .x, na.rm = TRUE), .names = "{.col}InadeCount"))


    ## Percentage Inadequate

    # Percentage Inadequate
    statsPercentageInadequate <- statsHouseholdCount |>
        dplyr::left_join(statsCountAdequateHH) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::mutate(dplyr::across(dplyr::ends_with("AdeCount"), ~ round(100 - (.x * 100 / households), 2), .names = "{.col}PercInadequate"))



    ## Median Supply

    statsMedianSupply <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(dplyr::across(dplyr::ends_with("Supply"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianSupply"))




    ## Mean Supply


    # statsMeanSupply <- enrichedHouseholdConsumption |>
    #     # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
    #     dpdplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #     # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
    #     dplyr::summarize(dplyr::across(dplyr::ends_with("Supply"), ~ round(mean(.x, na.rm = TRUE), 0), .names = "{.col}MeanSupply"))



    ## baselineAdequacyPrevalence

    # Merge the stats data into one dataframe
    baselineAdequacyPrevalence <- statsHouseholdCount |>
        dplyr::left_join(statsCountInadequateHH) |>
        # dplyr::left_join(statsCountAdequateHH) |>
        dplyr::left_join(statsPercentageInadequate) |>
        dplyr::left_join(statsMedianSupply) |>
        # dplyr::left_join(statsMeanSupply) |>
        dplyr::bind_cols(earThreshholds)

    # NOTE: The code below is not necessary but it makes the data look better
    # Arrange the data by the "aggregationGroup",households then everything else in alphabetical order. This allows each nutrients' data to be together

    # Get the column order for the data
    columnOrder <- sort(names(baselineAdequacyPrevalence))

    # Reorder the columns for better readability
    baselineAdequacyPrevalence <- baselineAdequacyPrevalence |>
        dplyr::select(all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, households, everything())


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


#' Calculate Baseline Nutrient Inadequacy
#'
#' This function calculates the baseline inadequacy of nutrients for different administrative groups.
#'
#' @param MNList A character vector of nutrients. If empty, defaults to a list of all nutrients.
#' @param aggregationGroup A character vector of administrative groups. Must not be empty.
#' @param dataDir The directory where the data is stored.
#'
#' @return A dataframe with the baseline inadequacy of nutrients for the specified administrative groups.
#' @export
#'
#' @examples
#' calculateBaselineInadequacy(MNList = c("A", "Ca"))
calculateBaselineInadequacyCND <- function(MNList = c("A"), aggregationGroup = c("admin0Name", "admin1Name"), dataDir = here::here("data/sd123")) {
    # Check if MNList is a character vector
    if (!is.character(MNList)) {
        stop("MNList must be a character vector")
    }

    # check if aggregationGroup is a character vector
    if (!is.character(aggregationGroup)) {
        stop("aggregationGroup must be a character vector")
    }

    # Check if MNLIst and aggregationGroup are not empty
    if (length(aggregationGroup) == 0) {
        stop("aggregationGroup cannot be empty")
    }

    # Check if dataDir exists
    if (!dir.exists(dataDir)) {
        stop("The specified dataDir does not exist")
    }

    # Load the data
    data <- loadMapsRdaTables(dataDir)

    # Check if all the required data is loaded in the data list
    if (!all(c("householdConsumption", "householdDetails", "nctList", "intakeThresholds") %in% names(data))) {
        # Explain which data is missing
        missingData <- c("householdConsumption", "householdDetails", "nctList", "intakeThresholds")[!(c("householdConsumption", "householdDetails", "nctList", "intakeThresholds") %in% names(data))]
        stop(paste("The following data is missing:", paste(missingData, collapse = ", ")))
    }


    if (length(MNList) == 0) {
        # Default to the list of all nutrients
        MNList <- c(
            "Ca",
            "Carbohydrates",
            "Cu",
            "Energy",
            "Fat",
            "Fe",
            "Fibre",
            "I",
            "IP6",
            "Mg",
            "Protein",
            "Se",
            "Zn",
            "Ash",
            "B6",
            "B2",
            "D",
            "N",
            "K",
            "P",
            "Moisture",
            "Cholesterol",
            "E",
            "Na",
            "A",
            "C",
            "B12",
            "B1",
            "B3",
            "B9",
            "B5",
            "B7",
            "Mn"
        )
        # Tell the user that the default list is being used
        message("No MNList provided. Using the default list of all nutrients")
    }



    # Extract the data from the list
    householdConsumption <- data$householdConsumption
    householdDetails <- data$householdDetails
    nctList <- data$nctList
    intakeThresholds <- data$intakeThresholds

    # Use the createMasterNct function to create a master NCT
    masterNCT <- createMasterNct(nctList)

    ## Create a wider format for the intakeThresholds


    # Create a wider format for the intakeThresholds
    # earThreshholds <- createThresholdsTable(intakeThresholds, "ear")
    earThreshholds <- intakeThresholds |>
        dplyr::select(nutrient, ear) |>
        # Remove rows where ear is NA
        dplyr::filter(!is.na(ear)) |>
        # Leave thresholds for the nutrients in the MNList
        dplyr::filter(nutrient %in% MNList) |>
        tidyr::pivot_wider(names_from = nutrient, values_from = ear) |>
        # Convert all columns to numeric
        dplyr::mutate_all(as.numeric) |>
        # Add a suffix of "ear" to the column names
        dplyr::rename_with(~ paste0(., "ApparentIntakeEarThreshold"), everything())


    # Create CND thresholds
    # cndThreshholds <- createThresholdsTable(intakeThresholds, "CND")
    cndThreshholds <- intakeThresholds |>
        dplyr::select(nutrient, CND) |>
        # Remove rows where ear is NA
        dplyr::filter(!is.na(CND)) |>
        # Leave thresholds for the nutrients in the MNList
        dplyr::filter(nutrient %in% MNList) |>
        tidyr::pivot_wider(names_from = nutrient, values_from = CND) |>
        # Convert all columns to numeric
        dplyr::mutate_all(as.numeric) |>
        # Add a suffix of "ear" to the column names
        dplyr::rename_with(~ paste0(., "ApparentIntakeCNDThreshold"), everything())

    # Process the consumption data

    # Load the consumption data
    enrichedHouseholdConsumption <- householdConsumption |>
        # Not necessary by its a personal preference
        tibble::as_tibble() |>
        # Join the household details to the consumption data (Joining columns with the same name)
        dplyr::left_join(householdDetails) |>
        # Join the master NCT to the consumption data
        dplyr::left_join(masterNCT) |>
        # Convert all columns needed for calculations to numeric
        dplyr::mutate_at(c("amountConsumedInG", "afeFactor", "memberCount", MNList, "Energy"), as.numeric) |>
        # Calculate the average daily apparent intake
        dplyr::mutate(averagePerDayAmountConsumed = amountConsumedInG / memberCount) |>
        # Balance consumption over the recall period i.e. 7 days for this data
        # dplyr::mutate(amountConsumedInGPerAfe = amountConsumedInG / 7) |> # Trying without this line
        # Calculate the MNs content by multiplying the amount consumed per AFE by the nutrient composition for each nutrient
        # TODO: This is the possible source of the errors. Check the calculation formula
        # NOTE: Update 28/02/2024: The micronutrient composition is in 100g of the food item. We need to divide by 100 to get the nutrient content per gram see Andy's email
        # dplyr::mutate(dplyr::across(MNList, ~ . / afeFactor)) |> # NOTE This is a trial of Andy's suggestion
        # Group the data by householdId so that we can summarise the data at the household level
        dplyr::group_by(householdId) |>
        # Summarise the data to get the total nutrient intake per afe per day for each household and name the columns with the nutrient name and "Supply"
        # dplyr::summarize(dplyr::across(all_of(MNList), ~ sum(.x, na.rm = TRUE), .names = "{.col}Supply")) |> # TODO: Divide by the afe factor here
        # NOTE: Trial of Andy's suggestion
        dplyr::summarize(dplyr::across(all_of(c(MNList, "Energy")), ~ sum(.x / 100 * averagePerDayAmountConsumed, na.rm = TRUE), .names = "{.col}DailyApparentIntake")) |> # TODO: Divide by the afe factor here
        # The summaries remove the household details so we need to join them back to the data
        dplyr::left_join(householdDetails) |>
        # Bind thresholds to the data. The thresholds data has one row so it should be recycled to the number of rows in the data
        dplyr::bind_cols(cndThreshholds)


    # , calculate baseline nutrient density of the household
    for (nutrient in MNList) {
        if (nutrient != "Energy") {
            #
            baselineNDcol <- paste0(nutrient, "BaselineND")
            # Create the threshold column for each nutrient
            thresholdCol <- paste0(nutrient, "ApparentIntakeCNDThreshold") # Adjust if the naming convention is different

            # Create the adequacy column for each nutrient to store the adequacy of the nutrient supply
            adequacyCol <- paste0(nutrient, "CNDAdequacy")

            # Calculate the baseline nutrient density
            enrichedHouseholdConsumption[[baselineNDcol]] <- enrichedHouseholdConsumption[[paste0(nutrient, "DailyApparentIntake")]] * 1000 / enrichedHouseholdConsumption[["EnergyDailyApparentIntake"]] # TODO: Check this formula

            # Only create the adequacy column if enrichedHouseholdConsumption[[threshold_col]] is not NA.
            if (!is.na(getMnThresholds(intakeThresholds, nutrient, "CND"))) {
                enrichedHouseholdConsumption[[adequacyCol]] <- ifelse(enrichedHouseholdConsumption[[baselineNDcol]] >= getMnThresholds(intakeThresholds, nutrient, "CND"), 1, 0)
            }
        }
    }





    # Prevalence of Inadequacy Summaries
    ## Households count summaries

    # TODO: Start HERE on 27/02/2024
    # Households count summaries
    statsHouseholdCount <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(households = dplyr::n())



    ## Count Adequate and Inadequate

    statsCountAdequateHH <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(dplyr::across(dplyr::ends_with("CNDAdequacy"), ~ sum(.x, na.rm = TRUE), .names = "{.col}AdeCount"))

    ## Count Inadequate
    statsCountInadequateHH <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(dplyr::across(dplyr::ends_with("CNDAdequacy"), ~ sum(1 - .x, na.rm = TRUE), .names = "{.col}InadeCount"))


    ## Percentage Inadequate

    # Percentage Inadequate
    statsPercentageInadequate <- statsHouseholdCount |>
        dplyr::left_join(statsCountAdequateHH) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::mutate(dplyr::across(dplyr::ends_with("CNDAdequacyInadeCount"), ~ round(100 - (.x * 100 / households), 2), .names = "{.col}PercInadequate"))



    ## Median Supply

    statsMedianSupply <- enrichedHouseholdConsumption |>
        # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
        dplyr::summarize(dplyr::across(dplyr::ends_with("DailyApparentIntake"), ~ round(median(.x, na.rm = TRUE), 0), .names = "{.col}MedianDailyApparentIntake"))




    ## Mean Supply


    # statsMeanSupply <- enrichedHouseholdConsumption |>
    #     # Re_group the data by district_name and adequacy to get the total number of households with adequate and inadequate Vitamin A intake
    #     dpdplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
    #     # Summarise the data to get the total number of households with adequate and inadequate Vitamin A intake
    #     dplyr::summarize(dplyr::across(dplyr::ends_with("Supply"), ~ round(mean(.x, na.rm = TRUE), 0), .names = "{.col}MeanSupply"))



    ## baselineAdequacyPrevalence

    # Merge the stats data into one dataframe
    baselineAdequacyPrevalence <- statsHouseholdCount |>
        dplyr::left_join(statsCountInadequateHH) |>
        # dplyr::left_join(statsCountAdequateHH) |>
        dplyr::left_join(statsPercentageInadequate) |>
        dplyr::left_join(statsMedianSupply) |>
        # dplyr::left_join(statsMeanSupply) |>
        dplyr::bind_cols(cndThreshholds)

    # NOTE: The code below is not necessary but it makes the data look better
    # Arrange the data by the "aggregationGroup",households then everything else in alphabetical order. This allows each nutrients' data to be together

    # Get the column order for the data
    columnOrder <- sort(names(baselineAdequacyPrevalence))

    # Reorder the columns for better readability
    baselineCNDAdequacyPrevalence <- baselineAdequacyPrevalence |>
        dplyr::select(all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, households, everything())


    return(baselineCNDAdequacyPrevalence)
}

# TODO:10 May 2024 Update
#' Calculate Food Vehicle Household Reach
#'
#' This function calculates the household reach for each food vehicle. It filters the fortifiable food items to get the food vehicle, processes the consumption data, and then calculates the household reach.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data. Default is `householdConsumption`.
#' @param householdDetailsDf A dataframe containing household details. Default is `householdDetails`.
#' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items. Default is `fortifiable_food_items`.
#' @param foodVehicleName A string specifying the food vehicle name. Default is "wheat flour".
#' @param aggregationGroup A character vector specifying the columns to group by. Default is `c("admin0Name", "admin1Name")`.
#'
#' @return A dataframe with the household reach for the specified food vehicle.
#'
#' @examples
#' calculateFoodVehicleHouseholdReach(householdConsumption, householdDetails, fortifiable_food_items, "wheat flour", c("admin0Name", "admin1Name"))
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

    # # Check if the required columns exist in the dataframes
    # requiredColumns <- c("foodGenusId", "householdId", "food_vehicle_name")
    # if (!all(requiredColumns %in% colnames(householdConsumptionDf))) {
    #     stop("householdConsumptionDf must contain the following columns: ", paste(requiredColumns, collapse = ", "))
    # }

    # if (!all(requiredColumns %in% colnames(householdDetailsDf))) {
    #     stop("householdDetailsDf must contain the following columns: ", paste(requiredColumns, collapse = ", "))
    # }

    # if (!all(requiredColumns %in% colnames(fortifiableFoodItemsDf))) {
    #     stop("fortifiableFoodItemsDf must contain the following columns: ", paste(requiredColumns, collapse = ", "))
    # }

    # Filter the fortifiable food items to get the food vehicle
    fortifiableFoodVehicle <- fortifiableFoodItemsDf |>
        dplyr::filter(food_vehicle_name == foodVehicleName)

    # Process the consumption data
    foodVehicleHouseholdReach <- householdConsumptionDf |>
        # Not necessary by its a personal preference
        tibble::as_tibble() |>
        dplyr::left_join(fortifiableFoodVehicle, by = c("foodGenusId" = "food_genus_id")) |>
        # Filter our rows where the food_vehicle_name is not subject to fortification leaving only foods that are subject to fortification
        # dplyr::filter(food_vehicle_name == foodVehicleName) |>
        # Join the household details to the consumption data (Joining columns with the same name)
        dplyr::left_join(householdDetailsDf) |>
        # Distinct household
        dplyr::arrange(desc(food_vehicle_name)) |>
        dplyr::distinct(householdId, .keep_all = TRUE) |>
        # Convert all columns needed for calculations to numeric
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(households = dplyr::n(), foodVehicleReachHH = sum(!is.na(food_vehicle_name)), foodVehicleReachHHPerc = round(foodVehicleReachHH / households * 100, 0))

    return(foodVehicleHouseholdReach)
}