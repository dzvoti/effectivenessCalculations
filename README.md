
<!-- README.md is generated from README.Rmd. Please edit that file -->

# effectivenessCalculations

<!-- badges: start -->
<!-- [![CRAN status]()]()
[![License](https://github.com/dzvoti/effectivenessCalculations?tab=MIT-2-ov-file)](https://github.com/dzvoti/effectivenessCalculations?tab=MIT-2-ov-file) -->

[![Version](https://img.shields.io/badge/version-v0.2.0-blue)](https://img.shields.io/badge/version-v0.2.0-blue)
[![Life
cycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/dzvoti/effectivenessCalculations/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dzvoti/effectivenessCalculations/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of effectivenessCalculations is to provide functions for
calculating various metrics related to nutritional effectiveness, such
as baseline inadequacy and household reach for food vehicles.

## Installation

You can install the development version of effectivenessCalculations
from [GitHub](https://github.com/dzvoti/effectivenessCalculations) with:

``` r
# install.packages("devtools")
devtools::install_github("dzvoti/effectivenessCalculations")
```

## Baseline Inadequacy Statistics Example

This is a basic example which shows you how to calculate the baseline
inadequacy for a micronutrient. The function
`calculateBaselineInadequacyAfe` calculates the baseline inadequacy for
a micronutrient.The function `calculateBaselineInadequacyCND` calculates
the baseline inadequacy based on nutrient density.
<!-- The function takes the following arguments: -->
<!-- - `MNList`: A character vector containing the names of the micronutrients for which the baseline inadequacy is to be calculated.
- `aggregationGroup`: A character vector containing the names of the columns to be used for aggregation. The baseline inadequacy will be calculated for each unique combination of the values in the columns specified in this vector.
- `householdConsumptionDf`: A data frame containing the household consumption data.
- `householdDetailsDf`: A data frame containing the household details.
- `intakeThresholds`: A data frame containing the intake thresholds for the micronutrients.
- `nctList`: A data frame containing the nutrient content of the food items. -->

### Baseline Inadequacy Based on apparent intake per Adult Female Equivalent(AFE)

``` r
library(effectivenessCalculations)
# Calculate baseline inadequacy for a micronutrient
calculateBaselineInadequacyAfe(
  MNList = c("A"),
  householdConsumptionDf = householdConsumption,
  householdDetailsDf = householdDetails,
  nctListDf = nctList,
  intakeThresholdsDf = intakeThresholds,
  aggregationGroup = c("admin0Name", "admin1Name")
)
```

### Baseline Inadequacy Based on Based on nutrient density

``` r
# Calculate baseline inadequacy for a micronutrient
calculateBaselineInadequacyCND(
  MNList = c("A"),
  householdConsumptionDf = householdConsumption,
  householdDetailsDf = householdDetails,
  nctListDf = nctList,
  intakeThresholdsDf = intakeThresholds,
  aggregationGroup = c("admin0Name", "admin1Name")
)
```

## Food Vehicle Household Reach Example

This is how to calculate the household reach for a food vehicle. The
function `calculateFoodVehicleHouseholdReach` calculates the household
reach for a food vehicle.
<!-- The function takes the following arguments:

- `householdConsumptionDf`: A data frame containing the household consumption data. 
- `householdDetailsDf`: A data frame containing the household details.
- `fortifiableFoodItemsDf`: A data frame containing the fortifiable food items. 
- `foodVehicleName`: The name of the food vehicle for which the household reach is to be calculated.
- `aggregationGroup`: A character vector containing the names of the columns to be used for aggregation. The household reach will be calculated for each unique combination of the values in the columns specified in this vector. -->

### Calculate Household Reach for a Food Vehicle

``` r
# Calculate household reach for each food vehicle
calculateFoodVehicleHouseholdReach(
  householdConsumptionDf = householdConsumption,
  householdDetailsDf = householdDetails,
  fortifiableFoodItemsDf = fortifiable_food_items,
  foodVehicleName = "wheat flour",
  aggregationGroup = c("admin0Name", "admin1Name")
)
```
