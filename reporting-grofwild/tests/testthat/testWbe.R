# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



context("Test WBE")


# Load all data

ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")
schadeData <- suppressWarnings(loadRawData(type = "wildschade"))
toekenningsData <- loadToekenningen()


# Define KBO

# currentKbo <- 445465768
# currentKbo <- unique(geoData$KboNummer_Toek) # multiple -> INBO
currentKbo <- unique(geoData$KboNummer_Toek[geoData$WBE_Naam_Toek %in% "De Zwarte Beek"])
# Find KBO with many species
# which.max(sapply(allWbe, function(wbe) length(unique(geoData$wildsoort[geoData$KboNummer_Toek == wbe]))))
matchingWbeData <- loadRawData(type = "kbo_wbe")


# Load spatial data

spatialData <- loadShapeData(WBE_NR = matchingWbeData$PartijNummer[match(currentKbo, matchingWbeData$KboNummer_Partij)])

years <- suppressWarnings(as.numeric(gsub("WBE_", "", grep("WBE_", names(spatialData), value = TRUE))))
years <- years[!is.na(years)]

biotoopData <- loadHabitats(regionLevels = "wbe")[["wbe"]]

defaultYear <- config::get("defaultYear", file = system.file("config.yml", package = "reportingGrofwild"))

species <- c("Ree", "Wild zwijn", "Damhert", "Edelhert")

# Filter data

geoData <- geoData[geoData$KboNummer_Toek %in% currentKbo, ]
schadeData <- schadeData[schadeData$KboNummer %in% currentKbo, ]
toekenningsData <- toekenningsData[toekenningsData$KboNummer_Toek %in% currentKbo, ]
biotoopData <- biotoopData[biotoopData$regio == unique(geoData$PartijNummer), ]

species <- species[species %in% unique(geoData$wildsoort)]

# Combine data
commonNames <- names(ecoData)[names(ecoData) %in% names(geoData)]
combinedRee <- merge(geoData[geoData$wildsoort == "Ree", ], 
  ecoData, by = commonNames, all.x = TRUE)

gc()


test_that("The map", {
    
    for (iYear in years) {
      
      if (doPrint)
      cat("*", iYear, "\n")
      
      for (iSpecies in species) {
        
        spaceData <- createSpaceData(
          data = geoData, 
          allSpatialData = spatialData,
          biotoopData = biotoopData,
          year = iYear,
          species = iSpecies,
          regionLevel = "WBE_buitengrenzen",
          unit = "region"
        )
        
        if (doPrint)
        cat("*", iSpecies, "\n")
        
        if (!is.null(spaceData) && nrow(spaceData$data) && !all(spaceData$data$freq == 0)) {
          
          expect_is(spaceData$data, "data.frame")
          
          myPlot <- mapFlanders(
            allSpatialData = spatialData, 
            regionLevel = "WBE_buitengrenzen", 
            year = iYear,
            colorScheme = suppressWarnings(RColorBrewer::brewer.pal(
                n = nlevels(spaceData$data$group), name = "YlOrBr")),
            summaryData = spaceData$data,
            legend = "topright",
            species = iSpecies
          )
          
          expect_is(myPlot, "leaflet")
          
        }
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
      
      myResult <- trendYearRegion(
        data = trendRegionData,
        locaties = unique(trendRegionData$locatie),
        timeRange = range(years),
        unit = unit
      )
      
      expect_s3_class(myResult$plot, "plotly")
      
    }  
    
  })



test_that("Biotoop", {
    
    result <- barBiotoop(data = biotoopData, jaar = 2020)
    
    expect_equal(round(sum(result$data$Waarde[-(1:2)])), 100)
        
  })


test_that("Afschot locaties", {
    
    mapData <- createAfschotLocationsData(data = combinedRee, accuracy = 1, 
      timeRange = range(combinedRee$afschotjaar))
    myResult <- mapSchade(schadeData = mapData, 
      regionLevel = "WBE_buitengrenzen_2018", 
      variable = c("season", "schadeCode", "afschotjaar")[3],
      allSpatialData = spatialData,
      addGlobe = TRUE)
    
    expect_s3_class(myResult, "leaflet")
    
  })



test_that("Summary Table", {
    
    for (iSpecies in species) {
      
      combinedData <- merge(geoData[geoData$wildsoort == iSpecies, ], 
        ecoData, by = commonNames, all.x = TRUE)
      
      if (nrow(combinedData) > 0) {
        
        myTable <- tableSpecies(
          data = combinedData, 
          jaar = defaultYear
        ) 
        
        expect_is(myTable$data, "data.frame")
        
      }
      
    }  
    
    tableSpecies(
      data = combinedRee, 
      jaar = 2021,
      categorie = c("labeltype", "type_comp", "geslacht_comp", "leeftijd_comp")[3]
    ) 
    
  })


test_that("Map schade", {
    
    schadeDataSub <- subset(schadeData, wildsoort == "Ree")  
    schadeDataSub <- createSchadeSummaryData(
      schadeData = schadeDataSub,
      timeRange = range(schadeDataSub$afschotjaar))
    
    for (var in c("season", "schadeCode", "afschotjaar")) {
      
      myPlot <- mapSchade(
        schadeData = schadeDataSub,
        regionLevel = "WBE_buitengrenzen_2020",
        variable = var,
        allSpatialData = filterSpatialWbe(allSpatialData = spatialData, partijNummer = unique(geoData$PartijNummer)),
        addGlobe = TRUE)
      
      expect_s3_class(myPlot, "leaflet")
      
    }
    
  })

test_that("Trend schade", {
    
    schadeDataSub <- subset(schadeData, wildsoort = species[1])
    
    trendRegionData <- createTrendData(
      data = sf::st_drop_geometry(schadeDataSub),
      allSpatialData = spatialData,
      timeRange = range(years),
      species = species[1],
      regionLevel = "WBE_buitengrenzen"
    )
    
    myResult <- trendYearRegion(
      data = trendRegionData,
      locaties = unique(trendRegionData$locatie),
      timeRange = range(years)
    )
    
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })


test_that("Additional plots", {
    
    myResult <- countYearShotAnimals(data = combinedRee,
      jaartallen = 2019:2020,
      groupVariable = c("labeltype", "jachtmethode_comp")[1],
      interval = c("Per maand", "Per seizoen", "Per twee weken")[1]
    )
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- countAgeGender(data = combinedRee)
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- countAgeCheek(data = combinedRee, jaartallen = 2009:2020)
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- boxAgeGenderLowerJaw(data = combinedRee, type = c("Kits", "Jongvolwassen", "Volwassen"))
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- percentageRealisedShot(data = toekenningsData,
      type = unique(toekenningsData$labeltype),
      jaartallen = 2009:2020)
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- plotBioindicator(data = combinedRee, bioindicator = "onderkaaklengte")
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- plotBioindicator(data = combinedRee, bioindicator = "ontweid_gewicht")
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- countEmbryos(data = combinedRee, jaartallen = 2009:2020,
      sourceIndicator = "both", sourceIndicator_leeftijd = "both", sourceIndicator_geslacht = "both")
    expect_is(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })