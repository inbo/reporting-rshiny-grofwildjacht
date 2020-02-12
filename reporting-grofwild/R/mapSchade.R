# Map(s) for wildschade
# 
# Author: mvarewyck
###############################################################################

#' Summarized data for perceelplot
#' 
#' Create object of type schadedata, filtered for cases within \code{timeRange}
#' whilst retaining only a select number of columns relevant for the perceelPlot 
#' created by \code{\link{mapSchade}} and relevant for data download.
#' 
#' @inheritParams mapSchade
#' @param timeRange numeric vector, year span of interest
#' @return a filtered spatialPointsDataFrame
#' 
#' @author Eva Adriaensen
#' @export
createSchadeSummaryData <- function(schadeData, timeRange) {
	
  
  # filter columns
  colnamesToRetain <- c("season", "afschotjaar", "wildsoort", "gemeente_afschot_locatie", "schadeBasisCode",
                        "provincie", "NISCODE")
  plotData <- schadeData[, colnames(schadeData@data) %in% colnamesToRetain]
  
  # filter cases by timeRange
  plotData <- plotData[plotData$afschotjaar %in% timeRange[1]:timeRange[2], ]
  
  plotData
}

#' Create map for Wildschade percelen
#' @param schadeData spatialPointsDataFrame contains the points where there was
#' wildschade and descriptives in data.frame
#' @inheritParams mapFlanders 
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles
#' @export
mapSchade <- function(
        schadeData, 
        regionLevel, 
        allSpatialData,
        addGlobe = FALSE,
        legend = "topright"
) {
    
    
    # Color palette
    palette <- colorFactor(inbo.2015.colours(n = 4),
            c("winter", "lente", "zomer", "herfst"))
    
    myMap <- leaflet(schadeData) %>%
            
            addCircleMarkers(
                    fillColor = ~palette(season),
                    stroke = TRUE, color = "black", weight = 1, 
                    fillOpacity = 0.5,
                    popup = paste0("<h4>Info</h4>",  
                            "<ul>", 
                            "<li><strong> Jaar </strong>: ", schadeData$afschotjaar,
                            "<li><strong> Wildsoort </strong>: ", schadeData$wildsoort, 
                            "<li><strong> Gemeente </strong>: ", schadeData$gemeente_afschot_locatie,
                            "<li><strong> Schade type </strong>: ", schadeData$schadeBasisCode,
                            "</ul>"
                    )
            )
    
    # Add black borders
    if (!is.null(regionLevel)) {
        
        myMap <- addPolylines(
                map = myMap,
                data = allSpatialData[[regionLevel]], 
                weight = 3, 
                color = "black",
                opacity = 1,
                group = "borderRegion"
        )
        
    }
    
    
    # Add world map
    if (addGlobe) {
        
        myMap <- addProviderTiles(myMap, "Hydda.Full")
        
    }
    
    # Add legend
    if (legend != "none") {
        
        myMap <- addLegend(
                map = myMap,
                position = legend,
                pal = palette, 
                values = ~season,
                opacity = 0.8,
                na.label = "onbekend",
                title = "Legende",
                layerId = "legend"
        )
        
        
    }
    
    
    myMap
    
    
}

