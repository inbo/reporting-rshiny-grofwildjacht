# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



context("Test WBE")

# Load all data
load(file = file.path(dataDir, "spatialData.RData"))
years <- as.numeric(gsub("WBE_binnengrenzen_", "", grep("WBE_binnengrenzen_", names(spatialData), value = TRUE)))


ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")

currentKbo <- 445465768
#currentKbo <- unique(geoData$KboNummer_Toek) # multiple -> INBO

currentYear <- 2020

species <- c("Ree", "Wild zwijn", "Damhert", "Edelhert")

geoData <- geoData[geoData$KboNummer_Toek %in% currentKbo, ]

#for (iVar in c("WBE_Naam_Toek", "WBE_Naam_Georef", "KboNummer_Toek", "KboNummer_Georef", "PartijNummer")) {
#  
#  print(iVar)
#  print(head(unique(geoData[, iVar])))
#  
#}


test_that("The map", {
    
    for (iYear in years) {
      
      if (doPrint)
      cat("*", iYear, "\n")
      
      for (iSpecies in species) {
        
        spaceData <- createSpaceData(
          data = geoData, 
          allSpatialData = spatialData,
          year = iYear,
          species = iSpecies,
          regionLevel = "WBE_binnengrenzen",
          unit = "relative"
        )
        
        if (doPrint)
        cat("*", iSpecies, "\n")
        
        if (!is.null(spaceData) && nrow(spaceData$data) && !all(spaceData$freq == 0)) {
          
          myPlot <- mapFlanders(
            allSpatialData = spatialData, 
            regionLevel = "WBE_binnengrenzen", 
            year = iYear,
            colorScheme = c("white", RColorBrewer::brewer.pal(
                n = nlevels(spaceData$data$group) - 1, name = "YlOrBr")),
            summaryData = spaceData$data,
            legend = "topright",
            species = iSpecies
          )
          
        }
      }
    }
    
  })




test_that("Trend plot", {
    
    for (iSpecies in species) {
      
      trendRegionData <- createTrendData(
        data = geoData[geoData$wildsoort == iSpecies, ],
        allSpatialData = spatialData,
        timeRange = range(years),
        species = iSpecies,
        regionLevel = "WBE_binnengrenzen",
        unit = "relative"
      )
      
      trendYearRegion(
        data = trendRegionData,
        locaties = 441,
        timeRange = range(years),
        unit = "relative"
      )$plot
      
    }  
    
  })