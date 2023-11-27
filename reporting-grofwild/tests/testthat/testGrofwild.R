# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


context("Test Grofwildjacht")

# Load all data
readS3(file = "spatialData_sf.RData")

ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")
biotoopData <- loadHabitats()


test_that("Load grofwild data", {
    
    expect_equal(nrow(ecoData), nrow(geoData))
    
  })

species <- c("Ree", "Wild zwijn", "Damhert", "Edelhert")

wildEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
reeEcoData <- ecoData[ecoData$wildsoort == "Ree", ]

openingstijdenData <- loadOpeningstijdenData()


## TABLES: Summary tables for Wild zwijn and Ree

test_that("Summary table for age", {
    
# For age
    allTables <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
        
        plotData <- subset(ecoData, wildsoort == iSoort)
        
        expectedNames <- c("Provincie", loadMetaEco(species = iSoort)$leeftijd_comp, "Onbekend")
        timeRange <- range(plotData$afschotjaar)
        
        wildTables <- lapply(timeRange[1]:timeRange[2], function(jaar) {
            
            myTable <- tableProvince(data = plotData, jaar = jaar, categorie = "leeftijd")$data
            
            expect_true(all(expectedNames %in% colnames(myTable)))
            
            myTable
            
          })
        
        expect_equal(length(wildTables), diff(timeRange) + 1)
        
      })
    
  })


## PLOT 1: Counts per year and province ##

test_that("Counts per year and province", {
    
    allPlots <- lapply(species, function(iSoort) {
        
        plotData <- subset(ecoData, wildsoort == iSoort)
        if (iSoort == "Edelhert")
          timeRange <- 2008:max(plotData$afschotjaar) else
          timeRange <- min(plotData$afschotjaar):max(plotData$afschotjaar)
        
        res <- countYearProvince(data = plotData, jaartallen = timeRange)
        
        expect_equal(names(res), c("plot", "data", "warning"))
        expect_equal(names(res$data), c("afschotjaar", "locatie", "aantal"))
        
        res
        
      })
    
    
    # Some special cases
    countYearProvince(data = wildEcoData, jaartallen = 2016)
    countYearProvince(data = wildEcoData, jaartallen = 2016:2017)
    
  })


## PLOT 2: Map with counts and corresponding line plot ##

test_that("Map with counts and corresponding line plot", {
    
    wildGeoData <- geoData[geoData$wildsoort == "Wild zwijn", ]
    
    # Check province names
    provinceNames <- levels(spatialData$provinces$NAAM)
    levels(wildGeoData$provincie)[which(!levels(wildGeoData$provincie) %in% provinceNames)]
    
    provinceNames <- levels(spatialData$provincesVoeren$NAAM)
    levels(wildGeoData$provincie)[which(!levels(wildGeoData$provincie) %in% provinceNames)]
    
    
    # Check commune names
    communeNames <- levels(spatialData$communes$NAAM)
    levels(wildGeoData$gemeente_afschot_locatie)[
      which(!levels(wildGeoData$gemeente_afschot_locatie) %in% communeNames)]

    # Interactive map
    myMap <- leaflet(spatialData$provinces) %>% 
      addProviderTiles("Hydda.Full") %>%
      addPolygons(
        weight = 1, 
        color = "gray",
        fillColor = "red",
        fillOpacity = 0.8,
        group = "region"
      ) 
    
    expect_is(myMap, "leaflet")
    
  })


## PLOT 3: Counts age based on cheek ##

test_that("Counts age based on cheek", {
    
    allPlots <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
        
        categories <- loadMetaEco(species = iSoort)$leeftijd_comp
        plotData <- ecoData[ecoData$wildsoort == iSoort, ]
        
        if (iSoort == "Ree")
          timeRange <- 2004:max(plotData$afschotjaar) else
          timeRange <- min(plotData$afschotjaar):max(plotData$afschotjaar)
        
        res <- countAgeCheek(data = plotData, jaartallen = timeRange)
        
        expect_equal(levels(res$data$jager), categories)
        expect_equal(levels(res$data$kaak), categories)
        
        res
        
      })
    
    
    # Some special cases
    countAgeCheek(data = wildEcoData, jaartallen = 2016)
    countAgeCheek(data = wildEcoData, jaartallen = 2016:2017)
    
  })


## PLOT 4: Counts per year and age ##

test_that("Counts per year and age", {
    
    allPlots <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
        
        categories <- loadMetaEco(species = iSoort)$leeftijd_comp
        
        plotData <- ecoData[ecoData$wildsoort == iSoort, ]
        
        lapply(c("count", "percent"), function(summarizeBy) {
            
            res <- countYearAge(data = plotData, 
              jaartallen = min(plotData$afschotjaar):max(plotData$afschotjaar),
              summarizeBy = summarizeBy)
            
            expect_equal(levels(res$data$kaak), c(categories, "Niet ingezameld"))
            
            res
          })
        
      })
    
    # Some special cases
    countYearAge(data = wildEcoData, jaartallen = 2016)
    countYearAge(data = wildEcoData, jaartallen = 2016, summarizeBy = "percent")
    
    
    countYearAge(data = wildEcoData, jaartallen = 2016:2017)
    countYearAge(data = wildEcoData, jaartallen = 2016:2017, summarizeBy = "percent")
    
  })


## PLOT 5: yearly percentage of shot animals

test_that("yearly percentage of shot animals", {
    
    allPlots <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
        
        plotData <- ecoData[ecoData$wildsoort == iSoort, ]
        
        openingSeasonData <- openingstijdenData[
          openingstijdenData$Soort == iSoort, ]
        
        openingstijdRange <- c(
          max(
            min(plotData$afschotjaar), 
            min(openingSeasonData$Jaar)
          ),
          min(
            max(plotData$afschotjaar), 
            max(openingSeasonData$Jaar)
          )-1
        )
        
        openingstijd <- seq(openingstijdRange[1], openingstijdRange[2])
        
        types <- unique(openingSeasonData$Type)
        types[types == ""] <- "all"
        
        lapply(types, function(type){
            
            lapply(openingstijd, function(jaar){
                
                myResult <- percentageYearlyShotAnimals(
                  data = plotData, 
                  openingstijdenData = openingSeasonData,
                  type = type,
                  jaar = jaar,
                  jaartallen = openingstijd
                )
                
                expect_type(myResult, "list")
                expect_s3_class(myResult$plot, "plotly")
                expect_s3_class(myResult$data, "data.frame")
                
              })
            
          })
        
      })
    
  })


## PLOT 6: Percentages per age and gender

test_that("Percentages per age and gender", {
    
    allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
        
        plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
        res <- countAgeGender(data = plotData)
        
        expect_equal(levels(res$data$leeftijd),loadMetaEco(species = wildsoort)$leeftijd_comp)
        
        res
        
      })
    
    countAgeGender(data = wildEcoData, jaartallen = 2016)
    countAgeGender(data = wildEcoData, jaartallen = 2016:2017)
    
  })


## PLOT 7: Distribution of weight ifo age ##

test_that("Distribution of weight ifo age", {
    
    allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
        
        plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
        myResult <- boxAgeWeight(data = plotData, type = levels(plotData$Leeftijdscategorie_onderkaak), 
          sourceIndicator_leeftijd = "both")
        
        expect_type(myResult, "list")
        expect_s3_class(myResult$plot, "plotly")
        expect_s3_class(myResult$data, "data.frame")
        
      })
    
    # Some special cases
    tmp <- boxAgeWeight(data = wildEcoData, jaartallen = 2016, 
      type = levels(wildEcoData$Leeftijdscategorie_onderkaak))
    tmp <- boxAgeWeight(data = wildEcoData, jaartallen = 2016:2017,
      type = levels(wildEcoData$Leeftijdscategorie_onderkaak))
    tmp <- boxAgeWeight(data = wildEcoData, jaartallen = 2006:2020,
      type = c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen"),
      sourceIndicator_leeftijd = "both",
      sourceIndicator_geslacht = "both")$plot
    
  })

test_that("Afschot per jachtmethode", {
    
    myResult <- countYearShotAnimals(data = wildEcoData,
#      jaartallen = 2014:2020,
      groupVariable = "jachtmethode_comp",
      interval = c("Per jaar", "Per maand", "Per seizoen", "Per twee weken")[1]
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    countYearShotAnimals(data = reeEcoData,
      jaartallen = 2019:2020,
      groupVariable = "jachtmethode_comp",
      interval = c("Per jaar", "Per maand", "Per seizoen", "Per twee weken")[2]
    )$plot
    
    countYearShotAnimals(data = wildEcoData,
#      jaartallen = 2014:2020,
      groupVariable = "leeftijd_comp",
      interval = c("Per jaar", "Per maand", "Per seizoen", "Per twee weken")[4]
    )$plot
    
  })


test_that("Verwezenlijkt afschot", {
    
    toekenningsData <- loadToekenningen()
    
    myResult <- percentageRealisedShot(data = toekenningsData,
      type = unique(toekenningsData$labeltype),
      jaartallen = 2009:2020)

    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    myResult <- boxRealisedShot(data = toekenningsData,
      type = unique(toekenningsData$labeltype),
      jaartallen = 2009:2020)
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })



## PLOT 9: Distribution of cheek length vs class ##

test_that("Distribution of cheek length vs class", {
    
    myResult <- boxAgeGenderLowerJaw(
      data = reeEcoData, 
#      jaartallen = unique(reeEcoData$afschotjaar)
      type = levels(reeEcoData$leeftijd_comp)
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })

## PLOT 10: Number of embryos (bio-indicator)

test_that("Number of embryos (bio-indicator)", {
    
    myResult <- countEmbryos(
      data = reeEcoData,
#    jaartallen = unique(reeEcoData$afschotjaar)
      jaartallen = 2002:2010
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    for (sourceIndicator in c("inbo", "meldingsformulier", "both"))
      pl <- countEmbryos(
        data = reeEcoData,
        sourceIndicator = sourceIndicator)$plot
    
    
    countEmbryos(
      data = wildEcoData,
      type = c("Frisling (v)", "Overloper (v)", "Zeug"),
      sourceIndicator = "both",
      sourceIndicator_leeftijd = "inbo",
      sourceIndicator_geslacht = "both")$plot
    
    
    
    indicators <- c("onderkaaklengte", "ontweid_gewicht")
    
    tmp <- lapply(indicators, function(bioindicator){
        message("Plot for ", bioindicator, " in progress")
        pl <- plotBioindicator(
          data = reeEcoData, 
#		type = "Geitkits",	
          jaartallen = unique(reeEcoData$afschotjaar),
          bioindicator = bioindicator,
          sourceIndicator = c("inbo", "meldingsformulier", "both")[3]
        )
#	print(pl)
      })
    
  })


## THE MAP

test_that("The interactive map", {
    
    regionLevels <- names(spatialData)[1:5]
    
    for (regionLevel in regionLevels) {
      
      if (doPrint)
        print(regionLevel)
      
      for (iSpecies in species) {
        
        if (doPrint)
          print(iSpecies)
        
        spaceData <- createSpaceData(
          data = geoData, 
          allSpatialData = spatialData,
          biotoopData = biotoopData[[regionLevel]],
          year = 2016,
          species = iSpecies,
          regionLevel = regionLevel,
          unit = c("absolute", "relative")[2]
        )
        
        if (doPrint) {
          cat("*", regionLevel, "\n")
          print(summary(spaceData$data$freq))
        }
        
        myPlot <- mapFlanders(
          allSpatialData = spatialData, 
          regionLevel = regionLevel, 
          colorScheme = suppressWarnings(c("white", RColorBrewer::brewer.pal(
              n = nlevels(spaceData$data$group) - 1, name = "YlOrBr"))),
          summaryData = spaceData$data,
          legend = "topright",
          species = iSpecies
        )
        
        expect_is(myPlot, "leaflet")
        
      }
      
    }
    
  })



# TREND plots

test_that("Trend plots according with the interactive map", {
    
    for (iSpecies in species) {
      
#    print(iSpecies)
      unitChoice <- c("absolute", "relative")[2]
      
      trendData <-  createTrendData(
        data = geoData[geoData$wildsoort == iSpecies, ],
        biotoopData = biotoopData[[regionLevel]],
        allSpatialData = spatialData,
        timeRange = c(2014, 2019),
        species = iSpecies,
        regionLevel = "flanders",
        unit = unitChoice
      )
      
      myResult <- trendYearFlanders(
        data = trendData,
        timeRange = c(2014, 2019),
        unit = unitChoice
      )
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
      
      for (regionLevel in names(spatialData)[1:6]) {
        
#        print(regionLevel)
        
        trendRegionData <- createTrendData(
          data = geoData[geoData$wildsoort == iSpecies, ],
          biotoopData = biotoopData[[regionLevel]],
          allSpatialData = spatialData,
          timeRange = c(2014, 2019),
          species = iSpecies,
          regionLevel = regionLevel,
          unit = unitChoice
        )
        
        trendYearRegion(
          data = trendRegionData,
          locaties = sample(trendRegionData$locatie, 5),
          timeRange = c(2014, 2019),
          unit = unitChoice
        )$plot
        
      }
      
    }
    
  })
  
  
  test_that("Biotoop plot according with the interactive map", {
      
      for (iName in names(spatialData)[1:6]) {
        if (iName != "fbz_gemeentes")
        barBiotoop(data = biotoopData[[ iName ]])$plot
      }
      
      myResult <- barBiotoop(
        data = subset(biotoopData[[ "provinces" ]], regio %in% c("West-Vlaanderen", "Oost-Vlaanderen")))
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
      
      expect_error(barBiotoop(data = 
            subset(biotoopData[[ names(spatialData)[2] ]], regio %in% "Vlaanderen")))
      
    })