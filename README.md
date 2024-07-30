
<!-- README.md is generated from README.Rmd. Please edit that file -->

# effectivenessCalculations

<!-- badges: start -->

[![R-CMD-check](https://github.com/dzvoti/effectivenessCalculations/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dzvoti/effectivenessCalculations/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
![GitHub R package
version](https://img.shields.io/github/r-package/v/dzvoti/effectivenessCalculations)
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

## Basic Examples

### Calculate pre and post fortification intake supplies, fortification vehicle reach and Inadequacy Summaries

This is how to calculate the pre and post fortification intake supplies,
fortification vehicle reach and Inadequacy Summaries. The function
`calculate_pre_and_post_fortification_summaries` calculates the pre and
post fortification intake supplies, fortification vehicle reach and
Inadequacy Summaries for chosen fortification method and metric.

``` r
calculate_pre_and_post_fortification_summaries(
    householdConsumptionDf = householdConsumption,
    householdDetailsDf = householdDetails,
    nctListDf = nctList,
    intakeThresholdsDf = intakeThresholds,
    aggregationGroup = c("admin0Name", "admin1Name"),
    fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
    foodVehicleName = "wheat flour",
    fortificationLevelsDf = fortificationLevels,
    years = c(2021:2024),
    MNList = c("A", "Ca"),
    metric = "AFE",
    method = "LSFF"
)
```

## Deprecated Examples

These examples are deprecated and will be removed in future versions of
the package. Please use the new functions above.

``` r
library(effectivenessCalculations)

calculate_pre_and_post_lsff_summaries_afe(
    householdConsumptionDf = householdConsumption,
    householdDetailsDf = householdDetails,
    nctListDf = nctList,
    intakeThresholdsDf = intakeThresholds,
    aggregationGroup = c("admin0Name", "admin1Name"),
    fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
    foodVehicleName = "wheat flour",
    fortificationLevels = fortificationLevels,
    years = c(2021:2024),
    MNList = c("A")
)
```

``` r
calculate_pre_and_post_lsff_summaries_cnd(
  householdConsumptionDf = householdConsumption,
  householdDetailsDf = householdDetails,
  nctListDf = nctList,
  intakeThresholdsDf = intakeThresholds,
  aggregationGroup = c("admin0Name", "admin1Name"),
  fortifiableFoodItemsDf = createFortifiableFoodItemsTable(),
  foodVehicleName = "wheat flour",
  fortificationLevelsDf = fortificationLevels,
  years = c(2021:2024),
  MNList = c("A")
)
```
