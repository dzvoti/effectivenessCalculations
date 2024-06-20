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
#' @keywords internal
enforceNumeric <- function(df, cols) {
    df <- df |> dplyr::mutate_at(cols, as.numeric)
    return(df)
}
