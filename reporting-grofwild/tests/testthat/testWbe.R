# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



context("Test WBE")

# Load all data
load(file = file.path(dataDir, "spatialDataWBE.RData"))
spatialData <- spatialDataWBE
rm(spatialDataWBE)

years <- as.numeric(gsub("WBE_", "", grep("WBE_", names(spatialData), value = TRUE)))
years <- years[!is.na(years)]


ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")
schadeData <- loadRawData(type = "wildschade")
toekenningsData <- loadToekenningen()
biotoopData <- loadHabitats(spatialData = spatialData, regionLevels = "wbe")[["wbe"]]


currentKbo <- 445465768
#currentKbo <- unique(geoData$KboNummer_Toek) # multiple -> INBO

currentYear <- 2020

species <- c("Ree", "Wild zwijn", "Damhert", "Edelhert")

# Filter data

ecoData <- ecoData[ecoData$doodsoorzaak == "afschot", ]
geoData <- geoData[geoData$KboNummer_Toek %in% currentKbo, ]
schadeData <- schadeData[schadeData$KboNummer %in% currentKbo, ]
toekenningsData <- toekenningsData[toekenningsData$KboNummer_Toek %in% currentKbo, ]
currentPartij <- unique(geoData$PartijNummer)
biotoopData <- biotoopData[biotoopData$regio == currentPartij, ]

# Combine data
commonNames <- names(ecoData)[names(ecoData) %in% names(geoData)]
combinedRee <- merge(geoData[geoData$wildsoort == "Ree", ], 
  ecoData, by = commonNames, all.x = TRUE)

spatialData <- filterSpatialWbe(allSpatialData = spatialData, partijNummer = currentPartij)

gc()


test_that("The map", {
    
    for (iYear in years) {
      
      if (doPrint)
      cat("*", iYear, "\n")
      
        spaceData <- createSpaceData(
          data = geoData, 
          allSpatialData = spatialData,
          biotoopData = biotoopData,
          year = iYear,
          species = "",
          regionLevel = "WBE_buitengrenzen",
          unit = "region"
        )
        
        if (!is.null(spaceData) && nrow(spaceData$data) && !all(spaceData$data$freq == 0)) {
          
          expect_is(spaceData$data, "data.frame")
          
          myPlot <- mapFlanders(
            allSpatialData = spatialData, 
            regionLevel = "WBE_buitengrenzen", 
            year = iYear,
            colorScheme = c("grey", RColorBrewer::brewer.pal(
                n = nlevels(spaceData$data$group), name = "YlOrBr")),
            summaryData = spaceData$data,
            legend = "topright",
            species = ""
          )
          
          expect_is(myPlot, "plotly")
          
        }
     }
    
  })




test_that("Trend plot", {
    
    unit <- c("absolute", "relative", "relativeDekking")[3]
    
    for (iSpecies in species) {
      
      trendRegionData <- createTrendData(
        data = geoData[geoData$wildsoort == iSpecies, ],
        allSpatialData = spatialData,
        biotoopData = biotoopData,
        timeRange = range(years),
        species = iSpecies,
        regionLevel = "WBE_buitengrenzen",
        unit = unit
      )
      
      myPlot <- trendYearRegion(
        data = trendRegionData,
        locaties = unique(geoData$WBE_Naam_Toek[geoData$KboNummer_Toek == currentKbo]),
        timeRange = range(years),
        unit = unit
      )$plot
      
      expect_is(myPlot, "plotly")
      
    }  
    
  })



test_that("Biotoop", {
    
    result <- barBiotoop(data = biotoopData, jaar = 2020)
    
    expect_equal(sum(result$data$Waarde[-(1:2)]), 100)
    
    
  })



test_that("Summary Table", {
    
    for (iSpecies in species) {
      
      combinedData <- merge(geoData[geoData$wildsoort == iSpecies, ], 
        ecoData, by = commonNames, all.x = TRUE)
      
      if (nrow(combinedData) > 0) {
        
      myTable <- tableSpecies(
        data = combinedData, 
        jaar = 2020
      ) 
      
      expect_is(myTable, "data.frame")
      
    }
    
    }  
    
    tableSpecies(
      data = combinedRee, 
      jaar = 2021,
      categorie = c("labeltype", "type_comp", "geslacht_comp", "leeftijd_comp")[3]
    ) 
    
  })


test_that("Map schade", {
    
    schadeDataSub <- subset(schadeData, wildsoort == "ree")  
    schadeDataSub <- createSchadeSummaryData(
      schadeData = schadeDataSub,
      timeRange = range(schadeDataSub$afschotjaar))
    
    for (var in c("season", "schadeCode", "afschotjaar")) {
      
      myPlot <- mapSchade(
        schadeData = schadeDataSub,
        regionLevel = "provinces",
        variable = var,
        allSpatialData = spatialData,
        addGlobe = TRUE)
      
    }
    
  })


test_that("Additional plots", {
    
    countYearShotAnimals(data = combinedRee,
      jaartallen = 2019:2020,
      groupVariable = "labeltype",
      interval = c("Per maand", "Per seizoen", "Per twee weken")[1]
    )$plot
    
    countYearShotAnimals(data = combinedRee,
      jaartallen = 2019:2020,
      groupVariable = "jachtmethode_comp",
      interval = c("Per maand", "Per seizoen", "Per twee weken")[1]
    )$plot
    
    countAgeGender(data = combinedRee)
    
    countAgeCheek(data = combinedRee, 
      jaartallen = 2009:2020)
    
    boxAgeGenderLowerJaw(data = combinedRee, type = c("Kits", "Jongvolwassen", "Volwassen"))
    
    percentageRealisedShot(data = toekenningsData,
      type = unique(toekenningsData$labeltype),
      jaartallen = 2009:2020,
      unit = "percentage")
    
    plotBioindicator(data = combinedRee, bioindicator = "onderkaaklengte")
    
    plotBioindicator(data = combinedRee, bioindicator = "ontweid_gewicht")
    
    countEmbryos(data = combinedRee, jaartallen = 2009:2020,
      sourceIndicator = "both", sourceIndicator_leeftijd = "both", sourceIndicator_geslacht = "both")
    
  })