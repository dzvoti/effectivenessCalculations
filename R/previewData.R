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
#' \dontrun{
#' # Create a dataframe
#' df <- data.frame(
#'     x = rnorm(5000),
#'     y = rnorm(5000),
#'     z = rnorm(5000)
#' )
#'
#' # Preview the dataframe
#' previewData(df)
#' }
#'
#' @keywords internal
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
