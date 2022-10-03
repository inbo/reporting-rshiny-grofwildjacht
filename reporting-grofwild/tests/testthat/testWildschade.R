# Tests plots and summaries for wildschade
# 
# Author: mvarewyck
###############################################################################


context("Test wildschade")


# Load all data
load(file = file.path(dataDir, "spatialData.RData"))

schadeData <- loadRawData(type = "wildschade")

metaSchade <- loadMetaSchade()
schadeWildsoorten <- metaSchade$wildsoorten
schadeTypes <- metaSchade$types
schadeCodes <- metaSchade$codes
schadeCodes <- metaSchade$codes
names(schadeCodes) <- NULL
schadeCodes <- unlist(schadeCodes)
fullNames <- c(schadeTypes, schadeCodes, schadeWildsoorten)

wildSchadeData <- subset(schadeData@data, wildsoort %in% 
    metaSchade$wildsoorten$`Grof wild`)

species <- unlist(metaSchade$wildsoorten)


## THE MAP

### 1. Number of cases per region level

test_that("Number of cases per region level", {
    
    xtabs(~ wildsoort + afschotjaar, data = subset(schadeData@data, wildsoort %in% species))
    
    regionLevels <- setdiff(names(spatialData), "provincesVoeren")
    regionLevels <- regionLevels[!grepl("WBE", regionLevels)]
    
    for (regionLevel in regionLevels) {
      
      for (iSpecies in species) {
        
        spaceData <- createSpaceData(
          data = schadeData@data, 
          allSpatialData = spatialData,
          year = 2020,
          species = iSpecies,
          regionLevel = regionLevel,
          unit = "absolute"
        )
        
        if (doPrint) {
          cat("*", regionLevel, "\n")
          cat("*", iSpecies, "\n")
          print(sum(spaceData$data$freq))
        }
        
        trendData <- createTrendData(
          data = schadeData@data,
          allSpatialData = spatialData,
          timeRange = c(2018, 2019),
          species = iSpecies,
          regionLevel = regionLevel,
          unit = "absolute")
        
        mapPlot <- mapFlanders(
          allSpatialData = spatialData, 
          regionLevel = regionLevel, 
          colorScheme = suppressWarnings(c("white", RColorBrewer::brewer.pal(
                n = nlevels(spaceData$data$group) - 1, name = "YlOrBr"))),
          summaryData = spaceData$data,
          legend = "topright",
          species = iSpecies
        )
        
        expect_is(mapPlot, "leaflet")
        
        if (doPrint)
          print(mapPlot)
        
        if (regionLevel == "flanders")
          trendPlot <- trendYearFlanders(
            data = trendData,
            timeRange = c(2018, 2019),
            unit = "absolute",
            isSchade = TRUE) else 
          trendPlot <- trendYearRegion(
            data = trendData,
            timeRange = c(2018, 2019),
            unit = "absolute",
            locaties = trendData$locatie[1:7],
            isSchade = TRUE)
        
        expect_is(trendPlot$plot, "plotly")
        
        if (doPrint)
          print(trendPlot)
        
      }
      
    }
    
  })


### 2. Map with exact location and description of each case

test_that("Map with exact location and description of each case", {
    
    for (iSpecies in species) {
      
      schadeDataSub <- subset(schadeData, wildsoort == iSpecies)  
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
        
        expect_is(myPlot, "leaflet")
        
        if (doPrint)
          print(myPlot)
      }    
    }
    
  })



### 3. Descriptive plots


## PLOT 1: Counts per year and province ##

test_that("Counts per year and province", {
    
    allPlots <- lapply(species, function(iSpecies) {
        
        if (doPrint)
          print(iSpecies)
        
        plotData <- subset(schadeData, wildsoort == iSpecies & afschotjaar >= 2018)
        timeRange <- min(plotData$afschotjaar):max(plotData$afschotjaar)
        
        res <- countYearProvince(data = plotData@data, jaartallen = timeRange)
        
        expect_equal(names(res), c("plot", "data", "warning"))
        expect_equal(names(res$data), c("afschotjaar", "locatie", "aantal"))
        
        res
        
      })
    
    
# Some special cases
    countYearProvince(data = wildSchadeData, jaartallen = 2018, type = "faunabeheerzones")
    countYearProvince(data = wildSchadeData, jaartallen = 2018:2019, type = "flanders")
    
    countYearProvince(data = wildSchadeData, jaartallen = 2018:2020, type = "provinces",
      sourceIndicator = "E-loket")$plot
    
  })


## PLOT 2: Counts per year and variable of interest ##

test_that("Counts per year and variable of interest", {
    
    # count
    countYearSchade(data = wildSchadeData, jaartallen = 2018:2019, type = "SoortNaam")$plot
    countYearSchade(data = schadeData@data, jaartallen = 2018:2019, type = "wildsoort")$plot
    countYearSchade(data = schadeData@data, jaartallen = 2018:2019, type = "schadeCode",
      fullNames = unlist(metaSchade$codes))$plot
    
    # percent
    myPlot <- countYearSchade(data = schadeData@data, jaartallen = 2018:2019, type = "schadeCode", 
      fullNames = unlist(metaSchade$codes), summarizeBy = "percent")$plot
    
    expect_is(myPlot, "plotly")
    
  })


### 4. Descriptive tables

## TABLE 1: Counts per type schade ##

test_that("Counts per type schade", {
    
# generate all tables
    allSchadeTables <- lapply(species, function(iSpecies) {
        
        choicesSchadecode <- c("GEWAS", "VRTG", "ANDERE")[1:3]
        choicesSchadeGewas <- c("VRTSCHD", "WLSCHD", "GEWASANDR")[1:3]
        choicesSchadeVrtg <- c("GNPERSLTSL", "PERSLTSL", "ONBEKEND")[1:3]
        
        plotData <- subset(schadeData, wildsoort == iSpecies & afschotjaar >= 2018)
        
        schadeTables <- lapply(c("provinces", "flanders", "faunabeheerzones"), function(type)
            tableSchadeCode(data = plotData@data,
              type = type,
              schadeChoices = choicesSchadecode,
              schadeChoicesVrtg = choicesSchadeVrtg, 
              schadeChoicesGewas = choicesSchadeGewas,
              fullNames = fullNames)
        )
        
        totalValues <- sapply(schadeTables, function(schadeTable) tail(schadeTable$data$Totaal, n = 1))
        expect_equal(totalValues[2], totalValues[1])
        expect_equal(totalValues[3], totalValues[1])
        schadeTable <- schadeTables[[1]]
        
        # some tests
        expect_equal(names(schadeTable), c("data", "header"))
        expect_equal(names(schadeTable$data)[1], "Locatie")
        expect_equal(tail(names(schadeTable$data), n = 1), "Totaal")
        if ("ANDERE" %in% choicesSchadecode)
          expect("Andere" %in% names(schadeTable$data), "columns do not match user choices")
        if ("VRTG" %in% choicesSchadecode & "ONBEKEND" %in% choicesSchadeVrtg)
          expect("Verkeersongeluk onbekend" %in% names(schadeTable$data), "columns do not match user choices")
        
        DT::datatable(schadeTable$data, rownames = FALSE, container = schadeTable$header,
          selection = "single", options = list(dom = 't', pageLength = -1))
        
      })
    
    names(allSchadeTables) <- species
    
# use for special cases
    schadeTable <- tableSchadeCode(data = wildSchadeData,
      schadeChoices = c("GEWAS", "VRTG", "ANDERE")[3],
      schadeChoicesVrtg = c("GNPERSLTSL", "PERSLTSL", "ONBEKEND")[1:2], 
      schadeChoicesGewas = c("VRTSCHD", "WLSCHD", "GEWASANDR")[1:3],
      fullNames = fullNames)
    
# testing for special cases
    expect("Andere" %in% names(schadeTable$data), "columns do not match user choices")
    
    DT::datatable(schadeTable$data, rownames = FALSE, container = schadeTable$header,
      selection = "single", options = list(dom = 't', pageLength = -1))
    
  })


## TABLE 2: Counts per type gewas ##

test_that("Counts per type gewas", {
    
    typeOptions <- c("provinces", "flanders", "faunabeheerzones")
    
# loop over all species-location combinations
    allGewasTables <- lapply(typeOptions, function(iType) {
        allTablesPerLocation <- lapply(species, function(iSpecies) {
            
            subData <- subset(schadeData, wildsoort == iSpecies & afschotjaar >= 2018)
            timeRange <- min(subData@data$afschotjaar):max(subData@data$afschotjaar)
            
            res <- tableGewas(data = subData@data, jaartallen = timeRange,
              type = iType,
              variable = "SoortNaam")
            
            if (!is.null(res)) {
              expect(nrow(res) > 0, "table with 0 rows detected")
              expect("Gewas" %in% names(res), "colnames table faulty")
              expect("Alle" %in% res$Gewas, "colnames table faulty")
              if (!"Vlaams Gewest" %in% names(res)) {
                expect("Vlaanderen" %in% names(res), "colnames table faulty")
              }
            }
            
            
            res
            
          })
        
        names(allTablesPerLocation) <- species
        allTablesPerLocation
      })
    
    names(allGewasTables) <- typeOptions
    
  })
