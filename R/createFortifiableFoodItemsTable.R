#' Create Fortifiable Food Items Table
#'
#' This function loads food group, food genus, and food vehicle data from Excel sheets,
#' processes the data to identify fortifiable food groups and items, and returns a table
#' of fortifiable food items with associated vehicle IDs and fortifiable portions.
#'
#' @return A data frame containing fortifiable food items with their respective vehicle IDs
#' and fortifiable portions.
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate case_when left_join
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fortifiable_food_items <- createFortifiableFoodItemsTable()
#' }
createFortifiableFoodItemsTable <- function(
    food_file = "data/sd123/food group genus vehicle data.xlsx",
    food_groups_sheet = "food_group",
    food_genus_sheet = "food_genus",
    food_vehicle_sheet = "food_vehicle") {
    # Load data
    food_groups <- readxl::read_excel(food_file, sheet = food_groups_sheet)
    food_genus <- readxl::read_excel(food_file, sheet = food_genus_sheet)
    food_vehicle <- readxl::read_excel(food_file, sheet = food_vehicle_sheet)

    # Create columns for food_vehicle_id and fortifiable_portion
    # Create fortificants at food group level and merge with food_vehicle
    fortifiable_food_groups <- food_groups |>
        dplyr::select(food_group_id, food_group_name) |>
        dplyr::mutate(
            food_vehicle_id = dplyr::case_when(
                food_group_id == "2035" ~ 1,
                food_group_id == "2028" ~ 4,
                food_group_id == "2037" ~ 2,
                food_group_id == "2020" ~ 4,
                TRUE ~ NA_integer_
            ),
            fortifiable_portion = dplyr::case_when(
                food_group_id == "2035" & food_vehicle_id == 1 ~ 100,
                food_group_id == "2028" & food_vehicle_id == 4 ~ 100,
                food_group_id == "2020" & food_vehicle_id == 4 ~ 50,
                TRUE ~ NA_integer_
            )
        ) |>
        dplyr::left_join(food_vehicle, by = "food_vehicle_id")

    # Merge fortifiable_food_groups with food_genus
    fortifiable_food_items <- food_genus |>
        dplyr::left_join(fortifiable_food_groups, by = c("food_group_id", "food_group_name"))

    # Override fortifiable_portion for food_genus with multiple fortificants
    fortifiable_food_items <- fortifiable_food_items |>
        dplyr::mutate(
            fortifiable_portion = dplyr::case_when(
                food_genus_id == "F0020.06" & food_vehicle_id == 1 ~ 80,
                food_genus_id == "23140.03.02" & food_vehicle_id == 1 ~ 0,
                food_genus_id == "F0020.01" & food_vehicle_id == 1 ~ 75, # Bread
                food_genus_id == "F0020.02" & food_vehicle_id == 1 ~ 75, # Bread
                food_genus_id == "F0022.02" & food_vehicle_id == 1 ~ 33, # Buns, scones
                food_genus_id == "F0022.01" & food_vehicle_id == 1 ~ 33, # Buns, scones
                food_genus_id == "F0022.05" & food_vehicle_id == 1 ~ 13, # Samosa (vendor) from Tanzania recipe
                food_genus_id == "F0022.06" & food_vehicle_id == 1 ~ 13, # Samosa (vendor) from Tanzania recipe
                food_genus_id == "23110.02" & food_vehicle_id == 1 ~ 100, # Wheat flour
                food_genus_id == "23110.01" & food_vehicle_id == 1 ~ 100, # Wheat flour
                food_genus_id == "F0022.04" & food_vehicle_id == 1 ~ 41, # Mandazi, doughnut (vendor)
                # "bread" %in% food_genus_name & food_vehicle_id == 1 ~ 75, # Katie email. 08-May-2024
                # food_genus_name == "buns" & food_vehicle_id == 1 ~ 33,
                TRUE ~ fortifiable_portion
            )
        )

    # Return fortifiable_food_items
    return(fortifiable_food_items)
}
