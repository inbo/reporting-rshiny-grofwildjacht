#' Construct the right popup information to use with functions such as
#' `addCircleMarkers` for leaflet maps.
#' Will also display `year` as first item in the list.
#'
#' @param data The dataframe containing the information to be displayed
#' @param popup_vars The columns of the dataframe that will be displayed
construct_popup <- function(data, popup_vars) {
  sapply(
    rownames(data),
    function(i) {
      row <- data[i, ]
      row_items <- paste(
        sapply(popup_vars, function(var) {
          glue::glue("<li><strong>{var}</strong>: {row[[var]]}</li>")
        }),
        collapse = ""
      )
      glue::glue(
        "<h4>Info</h4><ul><li><strong>Jaar</strong>: {row$year}{row_items}</ul>"
      )
    },
    USE.NAMES = FALSE
  )
}
