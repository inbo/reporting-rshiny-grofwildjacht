# Map(s) for wildschade
# 
# Author: mvarewyck
###############################################################################



#' Create map for Wildschade percelen
#' @param schadeData spatialPointsDataFrame contains the points where there was
#' wildschade and descriptives in data.frame
#' @param species character vector, species which need to be shown in the plot
#' @inheritParams mapFlanders 
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addMarkers addProviderTiles
#' @export
mapSchade <- function(
        schadeData, 
        regionLevel, 
        allSpatialData,
        addGlobe = FALSE
) {
    
    
    
    
    myMap <- leaflet(schadeData) %>%
            
            addMarkers(
                    popup = paste0("<h4>Schadegeval ID: ", schadeData$caseID, "</h4>",  
                            "<ul>", 
                            "<li><strong> Jaar </strong>: ", schadeData$afschotjaar,
                            "<li><strong> Wildsoort </strong>: ", schadeData$wildsoort, 
                            "<li><strong> Gemeente </strong>: ", schadeData$gemeente_afschot_locatie,
                            "<li><strong> Soortnaam </strong>: ", schadeData$Soortnaam,
                            "<li><strong> Schadebeschrijving </strong>: ", schadeData$SchadeBeschrijving,
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
    
    
    myMap
    
    
}

