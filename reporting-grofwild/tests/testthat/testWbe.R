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

# Filter data

tmpData <- ecoData[, c("type_comp", "geslacht_comp", "leeftijd_comp")]
tmpData[!duplicated(tmpData), ]

ecoData <- ecoData[ecoData$doodsoorzaak == "afschot", ]
geoData <- geoData[geoData$KboNummer_Toek %in% currentKbo, ]

# Combine data
commonNames <- names(ecoData)[names(ecoData) %in% names(geoData)]



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



test_that("Summary Table", {
    
    for (iSpecies in species) {
      
      combinedData <- merge(geoData[geoData$wildsoort == iSpecies, ], 
        ecoData, by = commonNames, all.x = TRUE)
      
      tableSpecies(
        data = combinedData, 
        jaar = 2020
      )  
    
    }  
    
  })


test_that("Verdeling afschot over de jaren", {
    
      combinedData <- merge(geoData[geoData$wildsoort == "Ree", ], 
        ecoData, by = commonNames, all.x = TRUE)
      
      countYearShotAnimals(data = combinedData,
        jaartallen = 2019:2020,
        interval = c("Per maand", "Per seizoen", "Per twee weken")[3]
      )
    
  })
