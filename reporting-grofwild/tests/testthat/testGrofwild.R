# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


context("Test Grofwildjacht")

# Load all data
load(file = file.path(dataDir, "spatialData.RData"))

ecoData <- loadRawData(type = "eco")
geoData <- loadRawData(type = "geo")
biotoopData <- loadHabitats(spatialData = spatialData)


test_that("Load grofwild data", {
    
    expect_equal(nrow(ecoData), nrow(geoData))
    
  })

species <- c("Ree", "Wild zwijn", "Damhert", "Edelhert")

getCategories <- function(wildSoort) {
  
  switch(wildSoort,
    "Wild zwijn" = c("Frisling", "Overloper", "Volwassen"),
    "Ree" = c("Kits", "Jongvolwassen", "Volwassen")
  )
}

wildEcoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]
reeEcoData <- ecoData[ecoData$wildsoort == "Ree", ]

openingstijdenData <- loadOpeningstijdenData()


## TABLES: Summary tables for Wild zwijn and Ree

test_that("Summary table for age", {
    
# For age
    allTables <- lapply(c("Wild zwijn", "Ree"), function(iSoort) {
        
        plotData <- subset(ecoData, wildsoort == iSoort & doodsoorzaak == "afschot")
        
        expectedNames <- c("Provincie", getCategories(iSoort), "Onbekend")
        timeRange <- range(plotData$afschotjaar)
        
        wildTables <- lapply(timeRange[1]:timeRange[2], function(jaar) {
            
            myTable <- tableProvince(data = plotData, jaar = jaar, categorie = "leeftijd")
            
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
    provinceNames <- levels(spatialData$provinces@data$NAAM)
    levels(wildGeoData$provincie)[which(!levels(wildGeoData$provincie) %in% provinceNames)]
    
    provinceNames <- levels(spatialData$provincesVoeren@data$NAAM)
    levels(wildGeoData$provincie)[which(!levels(wildGeoData$provincie) %in% provinceNames)]
    
    
# Check commune names
    communeNames <- levels(spatialData$communes@data$NAAM)
    levels(wildGeoData$gemeente_afschot_locatie)[
      which(!levels(wildGeoData$gemeente_afschot_locatie) %in% communeNames)]
    
# Check differences with reported map by INBO
    geoData[which(as.character(geoData$gemeente_afschot_locatie) == "Dessel" & geoData$wildsoort == "Wild zwijn"), ]
    
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
        
        categories <- getCategories(iSoort)
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
        
        categories <- getCategories(iSoort)
        
        plotData <- ecoData[ecoData$wildsoort == iSoort, ]
        
        lapply(c("count", "percent"), function(summarizeBy) {
            
            res <- countYearAge(data = plotData, 
              jaartallen = min(plotData$afschotjaar):max(plotData$afschotjaar),
              summarizeBy = summarizeBy)
            
            expect_equal(levels(res$data$kaak), c(categories, "Niet ingezameld"))
            
            res
          })
        
      })
    allPlots
    
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
                
                percentageYearlyShotAnimals(
                  data = plotData, 
                  openingstijdenData = openingSeasonData,
                  type = type,
                  jaar = jaar,
                  jaartallen = openingstijd
                )
                
              })
            
          })
        
      })
    
  })


## PLOT 6: Percentages per age and gender

test_that("Percentages per age and gender", {
    
    allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
        
        plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
        res <- countAgeGender(data = plotData)
        
        expect_equal(levels(res$data$leeftijd), getCategories(wildsoort))
        
        res
        
      })
    allPlots
    
    countAgeGender(data = wildEcoData, sourceIndicator_leeftijd = "inbo", 
      sourceIndicator_geslacht = "both", jaartallen = 2007:2021)
    
    countAgeGender(data = wildEcoData, jaartallen = 2016)
    countAgeGender(data = wildEcoData, jaartallen = 2016:2017)
    
  })


## PLOT 7: Distribution of weight ifo age ##

test_that("Distribution of weight ifo age", {
    
# Inspect age
    toInspect <- with(ecoData, which(leeftijd_comp == "Onbekend" & !is.na(leeftijd_maanden)))
    xtabs(~ afschotjaar + wildsoort, ecoData[toInspect, ])
    xtabs(~ afschotjaar + leeftijd_comp_bron, ecoData[toInspect, ], addNA = TRUE)
    
# How many Frislings removed?
    xtabs(~ leeftijd_maanden + leeftijd_comp, ecoData[ecoData$wildsoort == "Wild zwijn", ], addNA = TRUE)
    areRemoved <- with(ecoData, which(leeftijd_comp == "Frisling" & is.na(leeftijd_maanden))) 
    length(areRemoved)
    
    allPlots <- lapply(c("Wild zwijn", "Ree"), function(wildsoort) {
        
        plotData <- ecoData[ecoData$wildsoort == wildsoort, ]
        boxAgeWeight(data = plotData, type = loadMetaEco(species = wildsoort)$leeftijd_comp, 
          sourceIndicator_leeftijd = "both")$plot
        boxAgeWeight(data = plotData, type = loadMetaEco(species = wildsoort)$leeftijd_comp_inbo, 
          sourceIndicator_leeftijd = "inbo")$plot        
        
      })
    allPlots
    
  })

test_that("Afschot per jachtmethode", {
    
    countYearShotAnimals(data = wildEcoData,
#      jaartallen = 2014:2020,
      groupVariable = "labeltype",
      interval = c("Per jaar", "Per maand", "Per seizoen", "Per twee weken")[1]
    )$plot
    
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
    
    percentageRealisedShot(data = toekenningsData,
      type = unique(toekenningsData$labeltype),
      jaartallen = 2009:2020)
    
    boxRealisedShot(data = toekenningsData,
      type = unique(toekenningsData$labeltype),
      jaartallen = 2009:2020)
    
  })



## PLOT 9: Distribution of cheek length vs class ##

test_that("Distribution of cheek length vs class", {
    
    boxAgeGenderLowerJaw(
      data = reeEcoData, 
      jaartallen = unique(reeEcoData$afschotjaar),
      type = loadMetaEco(species = "Ree")$leeftijd_comp
    )$plot
    
  })

## PLOT 10: Number of embryos (bio-indicator)

test_that("Number of embryos (bio-indicator)", {
    
    xtabs(~ aantal_embryos_onbekend + aantal_embryos_bron, 
      data = reeEcoData[reeEcoData$type_comp %in% c("Reegeit", "Smalree"), ], 
      addNA = TRUE)
    xtabs(~ aantal_embryos + aantal_embryos_bron, 
      data = reeEcoData[reeEcoData$type_comp %in% c("Reegeit", "Smalree"), ], 
      addNA = TRUE)
    xtabs(~ aantal_embryos_MF + aantal_embryos_bron, 
      data = reeEcoData[reeEcoData$type_comp %in% c("Reegeit", "Smalree"), ], 
      addNA = TRUE)
    head(reeEcoData[reeEcoData$type_comp %in% c("Reegeit", "Smalree") & 
          is.na(reeEcoData$aantal_embryos_bron), 
        c("type_comp", "aantal_embryos", "aantal_embryos_onbekend", 
          "aantal_embryos_MF", "aantal_embryos_bron")])
    
    pl <- countEmbryos(
      data = reeEcoData,
#    jaartallen = unique(reeEcoData$afschotjaar)
      jaartallen = 2002:2010
    )
    pl$plot
    head(pl$data)
    
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
    
    for (regionLevel in names(spatialData)[1:5]) {
      
      for (iSpecies in species) {
        
        spaceData <- createSpaceData(
          data = geoData, 
          allSpatialData = spatialData,
          year = 2016,
          species = iSpecies,
          regionLevel = regionLevel,
          unit = c("absolute", "relative")[2]
        )
        
        if (doPrint) {
          cat("*", regionLevel, "\n")
          print(summary(spaceData$data$freq))
        }
        
        colorScheme <- suppressWarnings(c("white", RColorBrewer::brewer.pal(
              n = nlevels(spaceData$data$group) - 1, name = "YlOrBr"))) 
        
        myPlot <- mapFlanders(
          allSpatialData = spatialData, 
          regionLevel = regionLevel, 
          colorScheme = colorScheme,
          summaryData = spaceData$data,
          legend = "topright",
          species = iSpecies
        )
        if (doPrint)
          print(myPlot)
        
      }
      
    }
    
  })


# TREND plots

test_that("Trend plots according with the interactive map", {
    
    for (iSpecies in species) {
      
      if (doPrint)
        print(iSpecies)
      unitChoice <- c("absolute", "relative")[1]
      
      trendData <-  createTrendData(
        data = geoData[geoData$wildsoort == iSpecies, ],
        allSpatialData = spatialData,
        timeRange = c(2014, 2019),
        species = iSpecies,
        regionLevel = "flanders",
        unit = unitChoice
      )
      
      trendYearFlanders(
        data = trendData,
        timeRange = c(2014, 2019),
        unit = unitChoice
      )$plot
      
      for (regionLevel in names(spatialData)[1:6]) {
        
        if (doPrint)
          print(regionLevel)
        
        trendRegionData <- createTrendData(
          data = geoData[geoData$wildsoort == iSpecies, ],
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
      
      for (iName in names(spatialData)[1:4])
         if (doPrint) {
            print(iName)
            print(barBiotoop(data = biotoopData[[ iName ]])$plot)
          }
      
      barBiotoop(data = subset(biotoopData[[ "provinces" ]], regio %in% c("West-Vlaanderen", "Oost-Vlaanderen")))$plot
          
    })