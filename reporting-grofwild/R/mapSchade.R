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
#' @inheritParams readShapeData
#' @param timeRange numeric vector, year span of interest
#' @return a filtered spatialPointsDataFrame
#' 
#' @author Eva Adriaensen
#' @export
createSchadeSummaryData <- function(schadeData, timeRange,
    dataDir = system.file("extdata", package = "reportingGrofwild")) {
	
  
  # filter columns
  colnamesToRetain <- c("season", "afschotjaar", "wildsoort", "gemeente_afschot_locatie", "schadeBasisCode",
                        "schadeCode", "provincie")
  plotData <- schadeData[, colnames(schadeData@data) %in% colnamesToRetain]
  
  # filter cases by timeRange
  plotData <- plotData[plotData$afschotjaar %in% timeRange[1]:timeRange[2], ]
  
  # add nis and postcode
  gemeenteData <- read.csv(file.path(dataDir, "gemeentecodes.csv"), header = TRUE, sep = ",")
  
  plotData$niscode <- gemeenteData$NIS.code[match(plotData$gemeente_afschot_locatie, 
                                                  gemeenteData$Gemeente)]
  plotData$postcode <- gemeenteData$Postcode[match(plotData$gemeente_afschot_locatie, 
                                                    gemeenteData$Gemeente)]
                                            
  # decrypte schade names
  plotData$schadeBasisCode <- names(fullNames(plotData$schadeBasisCode))
  plotData$schadeCode <- names(fullNames(plotData$schadeCode))
  
  plotData
}

#' Format summary schadedata data for download with nice column names and correct column order
#' @param summarySchadeData spatialPointsDataFrame as obtained from \code{\link{createSchadeSummaryData}}
#' @return data.frame, dataframe with nicely formatted columnnames and specific column order  
#' 
#' @author Eva Adriaensen
#' @export
formatSchadeSummaryData <- function(summarySchadeData) {
	
  formatData <- summarySchadeData@data
  
  # change variable names
  names(formatData)[names(formatData) == "afschotjaar"] <- "jaar"
  names(formatData)[names(formatData) == "gemeente_afschot_locatie"] <- "locatie"
  names(formatData)[names(formatData) == "season"] <- "seizoen"
  names(formatData)[names(formatData) == "schadeBasisCode"] <- "basisTypeSchade"
  names(formatData)[names(formatData) == "schadeCode"] <- "typeSchade"
  
  
  # re-arrange columns
  firstColumns <- c("jaar", "locatie", "niscode", "postcode")
  formattedData <- cbind(
                        formatData[na.omit(match(firstColumns, names(formatData)))],
                        formatData[setdiff(names(formatData), firstColumns)]
                        )
  
  return(formattedData)                        

}

#' Create map for Wildschade percelen
#' @param schadeData spatialPointsDataFrame contains the points where there was
#' wildschade and descriptives in data.frame
#' @inheritParams mapFlanders 
#' @param variable character, indicates the variable of interest to color points by. 
#' Should be one of \code{c("season", "schadeCode", "afschotjaar")}
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles
#' @importFrom RColorBrewer brewer.pal
#' @importFrom INBOtheme inbo_palette
#' @export
mapSchade <- function(
        schadeData, 
        regionLevel, 
        variable = c("season", "schadeCode", "afschotjaar"),
        allSpatialData,
        addGlobe = FALSE,
        legend = "topright"
) {
    
    variable <- match.arg(variable)
    schadeData$variable <- schadeData[[variable]]
    schadeData$variable <- as.factor(schadeData$variable)
    
  
    # Color palette
    nColors <- length(levels(schadeData$variable))
    colors <- if (nColors < 10) {
        inbo_palette(n = nColors) 
      } else {
        paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
        unlist(sapply(paletteNames, function(x)
              suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
      }

    palette <- colorFactor(colors, levels(schadeData$variable))
          
    
    myMap <- leaflet(schadeData) %>%
            
            addCircleMarkers(
                    fillColor = ~palette(variable),
                    stroke = TRUE, color = "black", weight = 1, 
                    fillOpacity = 0.5,
                    popup = paste0("<h4>Info</h4>",  
                            "<ul>", 
                            "<li><strong> Jaar </strong>: ", schadeData$afschotjaar,
                            "<li><strong> Wildsoort </strong>: ", schadeData$wildsoort, 
                            "<li><strong> Gemeente </strong>: ", schadeData$gemeente_afschot_locatie,
                            "<li><strong> Schade type </strong>: ", schadeData$schadeBasisCode,
                            "<li><strong> Seizoen </strong>: ", schadeData$season,
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
        
        myMap <- myMap %>%
                    addProviderTiles("Esri.WorldImagery") %>%
                    addProviderTiles("Hydda.RoadsAndLabels")
        
    }
    
    # Add legend
    if (legend != "none") {
        
        myMap <- addLegend(
                map = myMap,
                position = legend,
                pal = palette, 
                values = ~variable,
                opacity = 0.8,
                na.label = "onbekend",
                title = "Legende",
                layerId = "legend"
        )
        
        
    }
    
    
    myMap
    
    
}

