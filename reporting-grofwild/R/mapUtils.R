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

#' Bound a leaflet map to the flanders region, with padding
#'
#' @param map The leaflet map to fit bounds to
#' @param padding Padding to use, by default 20 on each side. Provide a vector
#' with four values: top, left, bottom, right
leaflet_bound_flanders <- function(map, padding = NULL) {
  if (is.null(padding)) {
    # padding: top, left, bottom, right
    padding <- c(0, 0, 0, 20)
  }
  # This is the bounding box for flanders
  # Gotten through: getCenterView(spatialdata),
  # with spatialdata containing flanders shape
  bounding_flanders <- c(2.541329, 5.911206, 50.687413, 51.505112)
  map <- map |>
    leaflet::fitBounds(
      lng1 = bounding_flanders[1],
      lng2 = bounding_flanders[2],
      lat1 = bounding_flanders[3],
      lat2 = bounding_flanders[4],
      options = list(
        paddingTopLeft = padding[1:2],
        paddingBottomRight = padding[3:4]
      )
    )

  map
}
