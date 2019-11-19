# Functions to plot the interactive map for flanders
# 
# Author: mvarewyck
###############################################################################



#' Define province based on commune NIS code
#' @param NISCODE character vector, NIS codes from communes, for which to define province
#' @inheritParams createSpaceData
#' @return character vector, same length and order as \code{NISCODE}
#' with corresponding province for each communeS
#' @author mvarewyck
#' @export
getProvince <- function(NISCODE, allSpatialData) {
    
    communeCode <- substr(NISCODE, start = 1, stop = 1)
    
    provinceData <- allSpatialData$provinces@data[, c("NISCODE", "NAAM")]
    provinceData$NISCODE <- substr(provinceData$NISCODE, start = 1, stop = 1)
    
    sapply(communeCode, function(iCode) {
                
                if (is.na(iCode))
                    NA else
                    as.character(provinceData[provinceData$NISCODE == iCode, ]$NAAM)
                
            })
    
}


#' Create summary data of geographical data for selected year, species and region level
#' @param data data.frame, geographical data
#' @param allSpatialData list of sp, spatial data for all spatial levels
#' @param year integer, year of interest
#' @param species character, species of interest
#' @param regionLevel character, regional level of interest should be one of 
#' \code{c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes")}
#' @param unit character, whether absolute or relative frequencies (aantal/100ha) 
#' should be reported
#' @return data.frame
#' @author mvarewyck
#' @export
createSpaceData <- function(data, allSpatialData, year, species, regionLevel,
        unit = c("absolute", "relative", "absoluteCases")) {
    
    
    # To prevent warnings with R CMD check
    afschotjaar <- NULL
    wildsoort <- NULL
    
    unit <- match.arg(unit)
    
    # Select correct spatial data
    if ("Wild zwijn" %in% species & regionLevel == "provinces") {
        
        spatialData <- allSpatialData[["provincesVoeren"]]
        
    } else {
        
        spatialData <- allSpatialData[[regionLevel]]
        
    }
    
    # Framework for summary data
    fullData <- cbind(expand.grid(
                    wildsoort = species,
                    afschotjaar = year),
            if (regionLevel %in% c("communes", "fbz_gemeentes"))
                        spatialData@data[, c("NAAM", "AREA", "NISCODE")] else
                        spatialData@data[, c("NAAM", "AREA")])
    names(fullData)[3] <- "locatie"
    
    
    # For communes -> provide corresponding province in downloaded data
    if (regionLevel %in% c("communes", "fbz_gemeentes")) {
        
        fullData$provincie <- getProvince(
                NISCODE = fullData$NISCODE, 
                allSpatialData = allSpatialData)
        
        fullData$NISCODE <- NULL
        
    }
    
    
    
    # Select subset for time
    plotData <- subset(data, subset = afschotjaar %in% year & wildsoort %in% species)
    
    if (nrow(plotData) == 0) {
        
        allData <- fullData 
        allData$freq <- 0
        
    } else {
        
        # Create general plot data names
        plotData$locatie <- switch(regionLevel,
                flanders = "Vlaams Gewest",
                provinces = plotData$provincie, 
                communes = plotData$gemeente_afschot_locatie,
                faunabeheerzones = plotData$FaunabeheerZone,
                fbz_gemeentes = plotData$fbz_gemeente
        )
        
        # Exclude data with missing time or space
        plotData <- subset(plotData, !is.na(plotData$afschotjaar) & 
                        !is.na(plotData$locatie) & plotData$locatie != "",
                c("afschotjaar", "locatie", if (grepl("Cases", unit)) "caseID")
        )
        
        # Remove duplicate cases if needed
        if ("caseID" %in% colnames(plotData)) {
            
            plotData <- plotData[!duplicated(plotData), ]
            plotData$caseID <- NULL
            
        }
        
        # Summarize data over years
        summaryData <- plyr::count(df = plotData, vars = names(plotData))
        
        # Add names & times with 0 observations
        allData <- merge(summaryData, fullData, all = TRUE)
        allData$freq[is.na(allData$freq)] <- 0
        
    }
    
    allData$afschotjaar <- as.factor(allData$afschotjaar)
    
    
    summaryData2 <- plyr::count(df = allData, vars = names(allData)[!names(allData) %in% "freq"], 
            wt_var = "freq")
    
    
    # unit taken into account
    if (unit == "relative")
        summaryData2$freq <- summaryData2$freq/summaryData2$AREA 
    
    
    # Create group variable
    if (regionLevel %in% c("flanders", "provinces")) {
        
        if (unit == "absolute")
            otherBreaks <- unique(sort(summaryData2$freq)) else
            otherBreaks <- unique(sort(ceiling(summaryData2$freq*100)/100))
        
        summaryData2$group <- cut(x = summaryData2$freq, 
                breaks = c(-Inf, otherBreaks),
                labels = otherBreaks) 
        
    } else if (regionLevel == "faunabeheerzones" & "Ree" %in% species) {
        
        if (unit == "absolute")
            summaryData2$group <- cut(x = summaryData2$freq, 
                    breaks = c(-Inf, 0, 100, 200, 500, 1000, Inf),
                    labels = c("0", "1-100", "100-200", "200-500", "500-1000", ">1000")) else
            summaryData2$group <- cut(x = summaryData2$freq, 
                    breaks = c(-Inf, 0, 0.25, 0.5, 1, 2, 3, Inf),
                    labels = c("0", "0-0.25", "0.25-0.5", "0.5-1", "1-2", "2-3", ">3"))
        
    } else {
        
        if (unit == "absolute") {
            
            if (any(c("Wild zwijn", "Ree") %in% species))
                summaryData2$group <- cut(x = summaryData2$freq, 
                        breaks = c(-Inf, 0, 10, 20, 40, 80, Inf),
                        labels = c("0", "1-10", "11-20", "21-40", "41-80", ">80")) else
                summaryData2$group <- cut(x = summaryData2$freq, 
                        breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),
                        labels = c("0", "1-5", "6-10", "11-15", "16-20", ">20"))
            
        } else {
            
            summaryData2$group <- cut(x = summaryData2$freq, 
                    breaks = c(-Inf, 0, 0.25, 0.5, 1, 2, 3, Inf),
                    labels = c("0", "0-0.25", "0.25-0.5", "0.5-1", "1-2", "2-3", ">3"))
            
        }
        
    }
    
    # remove redundant variables
    summaryData2$afschotjaar <- NULL
    summaryData2$wildsoort <- NULL
    summaryData2$AREA <- NULL
    
    return(summaryData2)
    
    
}




#' Create map for Flanders - color by incidence
#' @inheritParams createSpaceData
#' @param summaryData data.frame, as returned by \code{\link{createSpaceData}}
#' @param colorScheme character vector, specifies the color palette for the different groups
#' in the summary data
#' @param legend character, legend placement; default is "none", no legend
#' @param addGlobe boolean, whether to add world map to background; default is FALSE 
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addPolygons addPolylines colorFactor addLegend addProviderTiles
#' @export
mapFlanders <- function(
        regionLevel = c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes"), 
        species, 
        allSpatialData, summaryData, colorScheme, 
        legend = "none", addGlobe = FALSE) {
    
    
    # Select correct spatial data
    if ("Wild zwijn" %in% species & regionLevel == "provinces") {
        
        spatialData <- allSpatialData[["provincesVoeren"]]
        
    } else {
        
        spatialData <- allSpatialData[[regionLevel]]
        
    }
    
    palette <- colorFactor(palette = colorScheme, levels = levels(summaryData$group))
    
    valuesPalette <- summaryData[match(spatialData$NAAM, summaryData$locatie),
            "group"]
    
    if (any(!summaryData$locatie %in% spatialData$NAAM))
        stop("De geo-data kan niet gematcht worden aan de shape data.")
    
    
    myMap <- leaflet(spatialData) %>%
            
            addPolygons(
                    weight = 1, 
                    color = "gray",
                    fillColor = ~ palette(valuesPalette),
                    fillOpacity = 0.8,
                    layerId = spatialData$NAAM,
                    group = "region"
            ) 
    
    # Add legend
    if (legend != "none") { 
        
        myMap <- addLegend(
                map = myMap,
                position = legend,
                pal = palette, 
                values = valuesPalette,
                opacity = 0.8,
                title = "Legende",
                layerId = "legend"
        )
        
    }
    
    
    # Add black borders
    if (regionLevel %in% c("communes", "fbz_gemeentes")) {
        
        borderRegion <- switch(regionLevel,
                "communes" = "provinces",
                "fbz_gemeentes" = "faunabeheerzones")  
        
        myMap <- addPolylines(map = myMap,
                data = allSpatialData[[borderRegion]], 
                color = "black", 
                weight = 3,
                opacity = 0.8,
                group = "borderRegion"
        )
        
    }
    
    if (addGlobe) {
        
        myMap <- addProviderTiles(myMap, "Hydda.Full")
        
    }
    
    
    myMap
    
}
