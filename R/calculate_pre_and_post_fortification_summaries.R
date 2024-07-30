#' Calculate Pre and Post Fortification Summaries
#'
#' This function calculates summaries of nutrient inadequacy before and after different fortification methods for different administrative groups using either the Adult Female Equivalent (AFE) Method, the Critical Nutrient Density (CND) Method, or both.
#'
#' @param householdConsumptionDf A dataframe containing household consumption data. Must contain columns: "householdId", "amountConsumedInG".
#' @param householdDetailsDf A dataframe containing household details. Must contain columns: "householdId", "memberCount".
#' @param nctListDf A dataframe containing nutrient composition tables. Must contain column: "nutrient".
#' @param intakeThresholdsDf A dataframe containing intake thresholds for nutrients. Must contain columns: "nutrient", "CND", "CUL".
#' @param aggregationGroup A character vector of administrative groups to aggregate the data. Must not be empty. Defaults to c("admin0Name", "admin1Name").
#' @param fortifiableFoodItemsDf A dataframe containing fortifiable food items.`.
#' @param foodVehicleName A character string specifying the name of the food vehicle for fortification. Defaults to "wheat flour".
#' @param fortificationLevelsDf A dataframe containing the fortification levels for different nutrients and years.
#' @param years A numeric vector specifying the years for which LSFF is analyzed. Defaults to 2021:2024.
#' @param MNList A character vector of nutrients to be included in the analysis. Defaults to "A". Must not be empty.
#' @param metric A character string specifying the method to be used for the analysis. Must be one of "AFE", "CND", or "BOTH". Defaults to "AFE".
#' @param method A character string specifying the fortification method to be used for the analysis. Must be one of "LSFF", "BIO", or "FERT". Defaults to "LSFF".
#' @return A dataframe containing summaries of nutrient inadequacy before and after fortification for different administrative groups.
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_pre_and_post_fortification_summaries(
#'     householdConsumptionDf = householdConsumption,
#'     householdDetailsDf = householdDetails,
#'     nctListDf = nctList,
#'     intakeThresholdsDf = intakeThresholds,
#'     aggregationGroup = c("admin0Name", "admin1Name"),
#'     fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
#'     foodVehicleName = "wheat flour",
#'     fortificationLevelsDf = fortificationLevels,
#'     years = c(2021:2024),
#'     MNList = c("A", "Ca"),
#'     metric = "AFE",
#'     method = "LSFF"
#' )
#' }
calculate_pre_and_post_fortification_summaries <- function(householdConsumptionDf = householdConsumption,
                                                           householdDetailsDf = householdDetails,
                                                           nctListDf = nctList,
                                                           intakeThresholdsDf = intakeThresholds,
                                                           aggregationGroup = c("admin0Name", "admin1Name"),
                                                           fortifiableFoodItemsDf = fortifiableFoodItem,
                                                           foodVehicleName = "wheat flour",
                                                           fortificationLevelsDf = fortificationLevels,
                                                           years = c(2021:2024),
                                                           MNList = "A",
                                                           metric = "afe",
                                                           method = "LSFF") {
    # Define required columns
    requiredConsumptionCols <- c("householdId", "amountConsumedInG")
    requiredDetailsCols <- c("householdId", "memberCount")
    requiredNctCols <- c("micronutrientId")
    requiredIntakeCols <- c("nutrient", "CND")

    # reformat input characters
    metric <- toupper(metric)
    metric_lower <- tolower(metric)
    method <- toupper(method)
    method_lower <- tolower(method)

    # List of supported MNs
    supportedMNList <- c(
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

    # Check if MN is in the supported list
    if (!MNList %in% supportedMNList) {
        stop(paste(
            "MN must be one of the following:",
            paste(supportedMNList, collapse = ", ")
        ))
    }

    if (!method %in% c("LSFF", "BIO", "FERT")) {
        stop("Method must be one of 'LSFF', 'BIO', 'FERT'")
    }

    if (!metric %in% c("AFE", "CND", "BOTH")) {
        stop("Metric must be one of 'AFE', 'CND', 'BOTH'")
    }


    # Check if MNList is a character vector
    if (!is.character(MNList)) {
        stop("MNList must be a character vector e.g. c('A', 'Ca')")
    }

    # Check if aggregationGroup is a character vector
    if (!is.character(aggregationGroup)) {
        stop(
            "aggregationGroup must be a character vector e.g. c('admin0Name', 'admin1Name')"
        )
    }

    # Check if MNList and aggregationGroup are not empty
    if (length(aggregationGroup) == 0) {
        stop("aggregationGroup cannot be empty")
    }

    # Check if input dataframes have required columns
    if (!all(requiredConsumptionCols %in% names(householdConsumptionDf))) {
        stop(paste(
            "householdConsumptionDf must contain the following columns:",
            paste(requiredConsumptionCols, collapse = ", ")
        ))
    }

    if (!all(requiredDetailsCols %in% names(householdDetailsDf))) {
        stop(paste(
            "householdDetailsDf must contain the following column:",
            paste(requiredDetailsCols, collapse = ", ")
        ))
    }

    if (!all(requiredNctCols %in% names(nctListDf))) {
        stop(paste(
            "nctListDf must contain the following columns:",
            paste(requiredNctCols, collapse = ", ")
        ))
    }

    if (!all(requiredIntakeCols %in% names(intakeThresholdsDf))) {
        stop(paste(
            "intakeThresholdsDf must contain the following columns:",
            paste(requiredIntakeCols, collapse = ", ")
        ))
    }



    # Mutate fortificationLevel values to numeric
    fortificationLevelsDf <- fortificationLevelsDf |>
        dplyr::mutate_at(
            c(
                "perc_fortifiable",
                "perc_fortified",
                "perc_average_fortification_level",
                MNList
            ),
            as.numeric
        )

    # Use the createMasterNct function to create a master NCT
    masterNCT <- effectivenessCalculations::createMasterNct(nctList) |>
        # Remove MNs not in the MNList but keep "Energy" needed for CND calculations
        dplyr::select(-dplyr::all_of(setdiff(supportedMNList, c(
            MNList, "Energy"
        ))))

    # Filter the fortifiable food items to get the food vehicle and method of interest
    fortifiableFoodVehicle <- fortifiableFoodItemsDf |>
        dplyr::filter(food_vehicle_name == foodVehicleName) |>
        dplyr::filter(!!rlang::sym(method) == 1) |>
        dplyr::filter(fortifiable_portion > 0) |>
        dplyr::select(
            food_genus_id,
            food_vehicle_id,
            food_vehicle_name,
            fortifiable_portion,
            !!rlang::sym(method)
        )



    if (metric == "AFE" | metric == "BOTH") {
        # Create a wider format for the EAR intakeThresholds
        earThreshholds <- intakeThresholdsDf |>
            dplyr::select(nutrient, ear, ul) |>
            dplyr::rename(EAR = ear, UL = ul) |>
            # Remove rows where ear is NA
            dplyr::filter(!is.na(EAR)) |>
            # Leave thresholds for the nutrients in the MNList
            dplyr::filter(nutrient %in% MNList) |>
            tidyr::pivot_wider(
                names_from = nutrient,
                values_from = c(EAR, UL)
            ) |>
            # Convert all columns to numeric
            dplyr::mutate_all(as.numeric) |>
            # Add a suffix of "ear" to the column names
            dplyr::rename_with(
                ~ paste0(., "SupplyEarThreshold"),
                dplyr::everything()
            )
    }

    if (metric == "CND" | metric == "BOTH") {
        ## Create a wider format for the CND intakeThresholds
        cndThreshholds <- intakeThresholdsDf |>
            dplyr::select(nutrient, CND, CUL) |>
            dplyr::filter(!is.na(CND)) |>
            dplyr::filter(nutrient %in% MNList) |>
            tidyr::pivot_wider(
                names_from = nutrient,
                values_from = c(CND, CUL)
            ) |>
            dplyr::mutate_all(as.numeric) |>
            dplyr::rename_with(~ paste0(., "_SupplyThreshold"), dplyr::everything())
    }

    if (metric == "AFE") {
        threshholds <- earThreshholds
    } else if (metric == "CND") {
        threshholds <- cndThreshholds
    } else if (metric == "BOTH") {
        threshholds <- earThreshholds |>
            dplyr::bind_cols(cndThreshholds)
    } else {
        stop("Method must be either 'AFE','CND' or 'BOTH'")
    }

    # Process the consumption data
    # Load the consumption data
    enrichedHouseholdConsumption <- householdConsumptionDf |>
        # Not necessary by its a personal preference
        tibble::as_tibble() |>
        # Join the household details to the consumption data (Joining columns with the same name)
        dplyr::left_join(householdDetailsDf) |>
        # Join the master NCT to the consumption data
        dplyr::left_join(masterNCT) |>
        dplyr::left_join(fortifiableFoodVehicle,
            by = c("foodGenusId" = "food_genus_id")
        ) |>
        # Convert all columns needed for calculations to numeric
        dplyr::mutate(dplyr::across(
            c(
                "amountConsumedInG",
                "afeFactor",
                "fortifiable_portion",
                "memberCount",
                "Energy",
                MNList
            ),
            as.numeric
        )) |>
        # TODO: Deal with the deprecation warning for external vectors here and switch to all_of
        dplyr::mutate(
            dplyr::across(MNList, ~ . / afeFactor, .names = "{.col}_perAFE"),
            amountConsumedInGAfe = amountConsumedInG / afeFactor
        ) |>
        dplyr::bind_cols(threshholds)

    # COMMON CALCULATIONS FOR BOTH AFE AND CND
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
        dplyr::filter(!is.na(food_vehicle_name)) |>
        dplyr::filter(amountConsumedInG > 0) |>
        dplyr::group_by(householdId) |>
        dplyr::summarize(dailyAmountConsumedPerAfe = sum(amountConsumedInG / afeFactor, na.rm = TRUE)) |>
        dplyr::left_join(householdDetailsDf) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
        dplyr::summarize(
            meanDailyamountConsumedContainingFortificantInGPerAfe = mean(dailyAmountConsumedPerAfe, na.rm = TRUE),
            medianDailyamountConsumedContainingFortificantInGPerAfe = median(dailyAmountConsumedPerAfe, na.rm = TRUE)
        )

    # Merge the summaries
    initialSummaries <- HHCountSummaries |>
        dplyr::left_join(fortificationVehicleReach) |>
        dplyr::left_join(amountConsumedContainingFortificant)

    if (metric == "AFE" | metric == "BOTH") {
        for (nutrient in MNList) {
            if (grepl(
                "mg",
                effectivenessCalculations::getMnThresholdUnits(intakeThresholdsDf, nutrient, "unitAdequacy")
            )) {
                enrichedHouseholdConsumption[paste0(nutrient, "_BaseSupply")] <- enrichedHouseholdConsumption[nutrient] / 100 * enrichedHouseholdConsumption["amountConsumedInG"]
                # TODO: This is verified as correct. Line 112 divided the nuttrient by AFE factor hence we use amountConsumedInG and not amountConsumedInGAfe here. It's quacky I know but it's correct.

                for (year in years) {
                    # Calculate the supply of the nutrient with LSFF per food item
                    # TODO: Check if this calculation is correct when all foods are tagged as fortifiable.
                    enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_", method, "Supply")] <- enrichedHouseholdConsumption["amountConsumedInGAfe"] * (
                        yearAverageFortificationLevel(
                            fortification_vehicle = foodVehicleName,
                            Year = year,
                            MN = nutrient,
                            fortificationLevels = fortificationLevelsDf
                        ) / 1000
                    ) * (enrichedHouseholdConsumption["fortifiable_portion"] / 100)
                }
            } else if (grepl(
                "mcg",
                effectivenessCalculations::getMnThresholdUnits(intakeThresholdsDf, nutrient, "unitAdequacy")
            )) {
                if (nutrient != "B9") {
                    enrichedHouseholdConsumption[paste0(nutrient, "_BaseSupply")] <- enrichedHouseholdConsumption[nutrient] / 100 * enrichedHouseholdConsumption["amountConsumedInG"]
                    # TODO: This is verified as correct. Line 112 divided the nuttrient by AFE factor hence we use amountConsumedInG and not amountConsumedInGAfe here. It's quacky I know but it's correct.

                    for (year in years) {
                        # Calculate the supply of the nutrient with LSFF per food item
                        # TODO: Check if this calculation is correct when all foods are tagged as fortifiable.
                        enrichedHouseholdConsumption[paste0(
                            nutrient,
                            "_",
                            year,
                            "_",
                            method,
                            "Supply"
                        )] <- enrichedHouseholdConsumption["amountConsumedInGAfe"] * yearAverageFortificationLevel(
                            fortification_vehicle = foodVehicleName,
                            Year = year,
                            MN = nutrient,
                            fortificationLevels = fortificationLevelsDf
                        ) * (enrichedHouseholdConsumption["fortifiable_portion"] / 100)
                    }
                } else {
                    enrichedHouseholdConsumption[paste0(nutrient, "_BaseSupply")] <- enrichedHouseholdConsumption[nutrient] / 100 * enrichedHouseholdConsumption["amountConsumedInG"]
                    # TODO: This is verified as correct. Line 112 divided the nuttrient by AFE factor hence we use amountConsumedInG and not amountConsumedInGAfe here. It's quacky I know but it's correct.

                    for (year in years) {
                        # Calculate the supply of the nutrient with LSFF per food item
                        # TODO: Check if this calculation is correct when all foods are tagged as fortifiable.
                        enrichedHouseholdConsumption[paste0(
                            nutrient,
                            "_",
                            year,
                            "_",
                            method,
                            "Supply"
                        )] <- enrichedHouseholdConsumption["amountConsumedInGAfe"] * (
                            yearAverageFortificationLevel(
                                fortification_vehicle = foodVehicleName,
                                Year = year,
                                MN = nutrient,
                                fortificationLevels = fortificationLevelsDf
                            ) * 1.7
                        ) * (enrichedHouseholdConsumption["fortifiable_portion"] / 100)
                    }
                }
            }
        }

        # Aggregate nutrient supplies by household
        nutrientSupply <- enrichedHouseholdConsumption |>
            dplyr::group_by(householdId) |>
            dplyr::summarize(
                dplyr::across(
                    dplyr::ends_with("_BaseSupply"),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}"
                ),
                dplyr::across(
                    dplyr::ends_with(paste0("_", method, "Supply")),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}"
                )
            )


        # Calculate mean and median nutrient supplies
        # TODO: These were checked and are consistent with the maps tool.
        medianNutrientSupplySummaries <- nutrientSupply |>
            dplyr::left_join(householdDetailsDf) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
            dplyr::summarize(
                dplyr::across(
                    dplyr::ends_with("_BaseSupply"),
                    ~ round(mean(.x, na.rm = TRUE), 0),
                    .names = "{.col}MeanSupply"
                ),
                dplyr::across(
                    dplyr::ends_with("_BaseSupply"),
                    ~ round(median(.x, na.rm = TRUE), 0),
                    .names = "{.col}MedianSupply"
                )
                # TOD0: Add the same for LSFFSupply
            )

        # Add _BaseSupply and _LSFFSupply for each nutrient and year combo
        for (nutrient in MNList) {
            for (year in years) {
                nutrientSupply[paste0(
                    nutrient,
                    "_",
                    year,
                    "_BaseAnd",
                    method,
                    "TotalSupply"
                )] <- nutrientSupply[paste0(nutrient, "_BaseSupply")] + nutrientSupply[paste0(nutrient, "_", year, "_", method, "Supply")]
            }
        }


        # Remerge the household details
        enrichedNutrientSupply <- nutrientSupply |>
            dplyr::left_join(householdDetailsDf) |>
            dplyr::bind_cols(earThreshholds)

        # TODO: Calculate the gap between the {ear}threshold and the nutrient supply for both basesuply and baseandlsffsupply and the next loop should then calculate the mean and media of the gap
        # Create adequacy columns for each Baseline and LSFF nutrient supply
        for (nutrient in MNList) {
            if (!is.na(
                effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear")
            )) {
                enrichedNutrientSupply[paste0(nutrient, "_base_supply_ear_inadequacy")] <- ifelse(
                    enrichedNutrientSupply[paste0(nutrient, "_BaseSupply")] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"),
                    0,
                    1
                )
            }
            for (year in years) {
                if (!is.na(
                    effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear")
                )) {
                    enrichedNutrientSupply[paste0(
                        nutrient,
                        "_",
                        year,
                        "_base_and_",
                        method_lower,
                        "_ear_inadequacy"
                    )] <- ifelse(
                        enrichedNutrientSupply[paste0(
                            nutrient,
                            "_",
                            year,
                            "_BaseAnd",
                            method,
                            "TotalSupply"
                        )] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ear"),
                        0,
                        1
                    )
                }
            }
        }

        # Check if the intake is above the Upper Limit
        for (nutrient in MNList) {
            if (!is.na(
                effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul")
            )) {
                #
                enrichedNutrientSupply[paste0(nutrient, "_base_ul_exceedance")] <- ifelse(
                    enrichedNutrientSupply[paste0(nutrient, "_BaseSupply")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"),
                    1,
                    0
                )
            }
            for (year in years) {
                if (!is.na(
                    effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul")
                )) {
                    enrichedNutrientSupply[paste0(
                        nutrient,
                        "_",
                        year,
                        "_base_and_",
                        method_lower,
                        "_ul_exceedance"
                    )] <- ifelse(
                        enrichedNutrientSupply[paste0(
                            nutrient,
                            "_",
                            year,
                            "_BaseAnd",
                            method,
                            "TotalSupply"
                        )] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "ul"),
                        1,
                        0
                    )
                }
            }
        }

        # Calculate effective coverage
        for (nutrient in MNList) {
            for (year in years) {
                enrichedNutrientSupply[paste0(nutrient, "_", year, "_effective_coverage")] <- ifelse(enrichedNutrientSupply[paste0(nutrient, "_base_supply_ear_inadequacy")] != enrichedNutrientSupply[paste0(
                    nutrient,
                    "_",
                    year,
                    "_base_and_",
                    method_lower,
                    "_ear_inadequacy"
                )],
                1,
                0
                )
            }
        }

        # Create adequacy summaries
        inadequacySummarriesAfe <- enrichedNutrientSupply |>
            dplyr::left_join(householdDetailsDf) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
            dplyr::summarize(
                dplyr::across(
                    dplyr::ends_with("_ear_inadequacy"),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}_count"
                ),
                dplyr::across(
                    dplyr::ends_with("_ul_exceedance"),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}_count"
                ),
                dplyr::across(
                    dplyr::ends_with("_effective_coverage"),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}_count"
                )
            ) |>
            dplyr::left_join(initialSummaries) |>
            dplyr::left_join(medianNutrientSupplySummaries) |>
            dplyr::mutate(dplyr::across(dplyr::ends_with("_count"), ~ round((
                .x * 100 / householdsCount
            ), 2), .names = "{.col}_perc"))
    }

    if (metric == "CND" | metric == "BOTH") {
        # Household apparent intake and CND
        enrichedHouseholdConsumption <- enrichedHouseholdConsumption |>
            # dplyr::group_by(householdId) |>
            dplyr::mutate(dplyr::across(
                dplyr::all_of(c(MNList, "Energy")),
                ~ (.x / 100 * amountConsumedInG),
                .names = "{.col}_BaseSupply"
            )) |>
            dplyr::bind_cols(threshholds)

        # Calculate median and mean supply of MNList balanced by Afe



        # calculate LSFF Supply
        for (nutrient in MNList) {
            for (year in years) {
                if (grepl(
                    "mg",
                    effectivenessCalculations::getMnThresholdUnits(intakeThresholdsDf, nutrient, "unitAdequacy")
                )) {
                    # Calculate the supply of the nutrient with LSFF per food item
                    enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_LSFFSupply")] <- enrichedHouseholdConsumption["amountConsumedInG"] * (
                        yearAverageFortificationLevel(
                            fortification_vehicle = foodVehicleName,
                            Year = year,
                            MN = nutrient,
                            fortificationLevels = fortificationLevelsDf
                        ) / 1000
                    ) * enrichedHouseholdConsumption["fortifiable_portion"] / 100
                } else if (grepl(
                    "mcg",
                    effectivenessCalculations::getMnThresholdUnits(intakeThresholdsDf, nutrient, "unitAdequacy")
                )) {
                    if (nutrient != "B9") {
                        # Calculate the supply of the nutrient with LSFF per food item
                        enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_LSFFSupply")] <- enrichedHouseholdConsumption["amountConsumedInG"] * yearAverageFortificationLevel(
                            fortification_vehicle = foodVehicleName,
                            Year = year,
                            MN = nutrient,
                            fortificationLevels = fortificationLevelsDf
                        ) * enrichedHouseholdConsumption["fortifiable_portion"] / 100
                    } else {
                        # Calculate the supply of the nutrient with LSFF per food item
                        enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_LSFFSupply")] <- enrichedHouseholdConsumption["amountConsumedInG"] * (
                            yearAverageFortificationLevel(
                                fortification_vehicle = foodVehicleName,
                                Year = year,
                                MN = nutrient,
                                fortificationLevels = fortificationLevelsDf
                            ) * 1.7
                        ) * enrichedHouseholdConsumption["fortifiable_portion"] / 100
                    }
                }

                enrichedHouseholdConsumption[paste0(
                    nutrient,
                    "_",
                    year,
                    "_TotalBaseAndLSFFSupply"
                )] <- enrichedHouseholdConsumption[[paste0(nutrient, "_BaseSupply")]] + ifelse(
                    is.na(enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_LSFFSupply")]]),
                    0,
                    enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_LSFFSupply")]]
                )
            }
        }

        # Sum up the food item supplies to Household level
        enrichedHouseholdConsumption <- enrichedHouseholdConsumption |>
            dplyr::group_by(householdId) |>
            dplyr::summarize(
                dplyr::across(
                    dplyr::ends_with("_BaseSupply"),
                    ~ sum(., na.rm = TRUE),
                    .names = "{.col}"
                ),
                dplyr::across(
                    dplyr::ends_with("_LSFFSupply"),
                    ~ sum(., na.rm = TRUE),
                    .names = "{.col}"
                ),
                dplyr::across(
                    dplyr::ends_with("_TotalBaseAndLSFFSupply"),
                    ~ sum(., na.rm = TRUE),
                    .names = "{.col}"
                )
            )

        medianNutrientSupplySummaries <- enrichedHouseholdConsumption |>
            dplyr::left_join(householdDetailsDf) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
            dplyr::summarize(
                dplyr::across(
                    dplyr::ends_with("_BaseSupply"),
                    ~ round(mean(.x, na.rm = TRUE), 0),
                    .names = "{.col}MeanSupply"
                ),
                dplyr::across(
                    dplyr::ends_with("_BaseSupply"),
                    ~ round(median(.x, na.rm = TRUE), 0),
                    .names = "{.col}MedianSupply"
                )
                # TOD0: Add the same for LSFFSupply
            )


        # Calculate baseline nutrient density and adequacy
        for (nutrient in MNList) {
            if (nutrient != "Energy") {
                enrichedHouseholdConsumption[[paste0(nutrient, "_baseline_cnd")]] <- enrichedHouseholdConsumption[[paste0(nutrient, "_BaseSupply")]] * 1000 / enrichedHouseholdConsumption[["Energy_BaseSupply"]]

                if (!is.na(
                    effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND")
                )) {
                    enrichedHouseholdConsumption[[paste0(nutrient, "_cnd_inadequacy")]] <- ifelse(
                        enrichedHouseholdConsumption[[paste0(nutrient, "_baseline_cnd")]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"),
                        0,
                        1
                    )
                }

                if (!is.na(
                    effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL")
                )) {
                    enrichedHouseholdConsumption[paste0(
                        nutrient,
                        "_",
                        year,
                        "_Base_cul_exceedence"
                    )] <- ifelse(
                        enrichedHouseholdConsumption[paste0(nutrient, "_baseline_cnd")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL"),
                        1,
                        0
                    )
                }
            }

            for (year in years) {
                enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_BaseAndLSFF_cnd")]] <- enrichedHouseholdConsumption[[paste0(
                    nutrient,
                    "_",
                    year,
                    "_TotalBaseAndLSFFSupply"
                )]] * 1000 / enrichedHouseholdConsumption[[paste0("Energy_BaseSupply")]]

                if (!is.na(
                    effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND")
                )) {
                    enrichedHouseholdConsumption[[paste0(
                        nutrient,
                        "_",
                        year,
                        "_BaseAndLSFF_cnd_inadequacy"
                    )]] <- ifelse(
                        enrichedHouseholdConsumption[[paste0(nutrient, "_", year, "_BaseAndLSFF_cnd")]] >= effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CND"),
                        0,
                        1
                    )
                }

                if (!is.na(
                    effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL")
                )) {
                    enrichedHouseholdConsumption[paste0(
                        nutrient,
                        "_",
                        year,
                        "_BaseAndLSFF_cul_exceedence"
                    )] <- ifelse(
                        enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_BaseAndLSFF_cnd")] > effectivenessCalculations::getMnThresholds(intakeThresholdsDf, nutrient, "CUL"),
                        1,
                        0
                    )
                }
            }
        }

        # Calculate effective coverage
        for (nutrient in MNList) {
            for (year in years) {
                enrichedHouseholdConsumption[paste0(nutrient, "_", year, "_effective_coverage")] <- ifelse(
                    enrichedHouseholdConsumption[paste0(nutrient, "_cnd_inadequacy")] != enrichedHouseholdConsumption[paste0(
                        nutrient,
                        "_",
                        year,
                        "_BaseAndLSFF_cnd_inadequacy"
                    )],
                    1,
                    0
                )
            }
        }


        # Summarize prevalence of inadequacy
        inadequacySummarriesCnd <- enrichedHouseholdConsumption |>
            dplyr::left_join(householdDetailsDf) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(aggregationGroup))) |>
            dplyr::summarize(
                dplyr::across(
                    dplyr::ends_with("_cnd_inadequacy"),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}_count"
                ),
                dplyr::across(
                    dplyr::ends_with("_cul_exceedence"),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}_count"
                ),
                dplyr::across(
                    dplyr::ends_with("_effective_coverage"),
                    ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}_count"
                )
            ) |>
            dplyr::left_join(initialSummaries) |>
            dplyr::mutate(dplyr::across(dplyr::ends_with("_count"), ~ round((
                .x * 100 / householdsCount
            ), 2), .names = "{.col}_perc"))
    }

    if (metric == "AFE") {
        inadequacySummarries <- inadequacySummarriesAfe
    } else if (metric == "CND") {
        inadequacySummarries <- inadequacySummarriesCnd
    } else if (metric == "BOTH") {
        inadequacySummarries <- inadequacySummarriesAfe |>
            dplyr::left_join(inadequacySummarriesCnd)
    } else {
        stop("Method must be either 'AFE','CND' or 'BOTH'")
    }

    # Get the column order for the data
    columnOrder <- sort(names(inadequacySummarries))

    # Reorder the columns for better readability
    finalSummarries <- inadequacySummarries |>
        dplyr::select(dplyr::all_of(columnOrder)) |>
        dplyr::select(aggregationGroup, householdsCount, dplyr::everything())

    return(finalSummarries)
}
