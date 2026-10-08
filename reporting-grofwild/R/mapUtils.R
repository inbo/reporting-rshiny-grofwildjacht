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

#' Add padding on rendering of a leaflet map
#'
#' @param map The leaflet map to fit bounds to
#' @param padding Padding to use, by default 20 on the right side. Provide a
#' vector with four values: left, top, right, bottom
#' 
#' @importFrom htmlwidgets onRender
#' @importFrom glue glue
leaflet_pad_bounds <- function(map, padding = NULL) {
  if (is.null(padding)) {
    # padding: left, top, right, bottom
    padding <- c(20, 20, 20, 20)
  }

  # Don't bound to flanders with padding, just add padding on rendering of the map
  map <- map |> htmlwidgets::onRender(
    glue::glue("
      function(el, x) {{
        var map = this;
        var bounds = map.getBounds();

        map.fitBounds(bounds, {{
          paddingTopLeft: [{padding[[1]]}, {padding[[2]]}],
          paddingBottomRight: [{padding[[3]]}, {padding[[4]]}]
        }});
      }}
    ")
  )

  map

}
