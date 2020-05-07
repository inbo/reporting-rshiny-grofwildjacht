# Tests plots and summaries for wildschade
# 
# Author: mvarewyck
###############################################################################


library(reportingGrofwild)
library(testthat)



# Load all data
#readShapeData()  # create shape data
dataDir <- system.file("extdata", package = "reportingGrofwild")
load(file = file.path(dataDir, "spatialData.RData"))

schadeData <- loadRawData(type = "wildschade")
wildSchadeData <- subset(schadeData@data, wildsoort %in% c("wild zwijn", "edelhert", "ree", "smient")[1])

species <- unique(schadeData$wildsoort)
#  [1] "vos"         "wild zwijn"  "houtduif"    "smient"      "konijn"     
#  [6] "edelhert"    "haas"        "grauwe gans" "wilde eend"  "wolf"       
# [11] "ree"         "fazant"     



## THE MAP

### 1. Number of cases per region level

xtabs(~ wildsoort + afschotjaar, data = schadeData@data)

for (regionLevel in setdiff(names(spatialData), "provincesVoeren")) {
    
    for (iSpecies in species) {
        
        spaceData <- createSpaceData(
                data = schadeData@data, 
                allSpatialData = spatialData,
                year = 2020,
                species = iSpecies,
                regionLevel = regionLevel,
                unit = "absolute"
        )
        
        cat("*", regionLevel, "\n")
        cat("*", iSpecies, "\n")
        print(sum(spaceData$data$freq))
        
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
                colorScheme = c("white", RColorBrewer::brewer.pal(
                                n = nlevels(spaceData$data$group) - 1, name = "YlOrBr")),
                summaryData = spaceData$data,
                legend = "topright",
                species = iSpecies
        )
        print(mapPlot)
        
        if (regionLevel == "flanders")
            trendPlot <- trendYearFlanders(
                    data = trendData,
                    timeRange = c(2018, 2019),
                    unit = "absolute") else 
            trendPlot <- trendYearRegion(
                    data = trendData,
                    timeRange = c(2018, 2018),
                    unit = "absolute",
                    locaties = trendData$locatie[1:3])
        
        print(trendPlot)
        
        
        
    }
    
}



### 2. Map with exact location and description of each case

for (iSpecies in species) {
  
  schadeDataSub <- subset(schadeData, wildsoort == iSpecies)  
  schadeDataSub <- createSchadeSummaryData(
      schadeData = schadeDataSub,
      timeRange = range(schadeDataSub$afschotjaar))
  
  for (var in c("season", "schadeCode")) {
    myPlot <- mapSchade(
            schadeData = schadeDataSub,
            regionLevel = "provinces",
            variable = var,
            allSpatialData = spatialData,
            addGlobe = TRUE)
    
    print(myPlot)
  }    
}



### 3. Descriptive plots


## PLOT 1: Counts per year and province ##

allPlots <- lapply(species, function(iSpecies) {
            
            plotData <- subset(schadeData, wildsoort == iSpecies & afschotjaar >= 2018)
            timeRange <- min(plotData$afschotjaar):max(plotData$afschotjaar)
            
            res <- countYearProvince(data = plotData@data, jaartallen = timeRange)
            
            expect_equal(names(res), c("plot", "data"))
            expect_equal(names(res$data), c("afschotjaar", "locatie", "aantal"))
            
            res
            
        })


# Some special cases
countYearProvince(data = wildSchadeData, jaartallen = 2018, type = "faunabeheerzones")
countYearProvince(data = wildSchadeData, jaartallen = 2018:2019, type = "flanders")


## PLOT 2: Counts per year and variable of interest ##

# count
countYearSchade(data = wildSchadeData, jaartallen = 2018:2019, type = "SoortNaam")$plot
countYearSchade(data = schadeData@data, jaartallen = 2018:2019, type = "wildsoort")$plot
countYearSchade(data = schadeData@data, jaartallen = 2018:2019, type = "schadeCode")$plot

# percent
countYearSchade(data = schadeData@data, jaartallen = 2018:2019, type = "schadeCode", 
        summarizeBy = "percent")$plot


### 4. Descriptive tables

## TABLE 1: Counts per type schade ##

# generate all tables
allSchadeTables <- lapply(species, function(iSpecies) {
            
            choicesSchadecode <- c("GEWAS", "VRTG", "ANDERE")[1:3]
            choicesSchadeGewas <- c("VRTSCHD", "WLSCHD", "GEWASANDR")[1:3]
            choicesSchadeVrtg <- c("GNPERSLTSL", "PERSLTSL", "ONBEKEND")[1:3]
            
            plotData <- subset(schadeData, wildsoort == iSpecies & afschotjaar >= 2018)
            
            schadeTable <- tableSchadeCode(data = plotData@data,
#                  type = c("provinces", "flanders", "faunabeheerzones")[1],
                    schadeChoices = choicesSchadecode,
                    schadeChoicesVrtg = choicesSchadeVrtg, 
                    schadeChoicesGewas = choicesSchadeGewas)
            
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
        schadeChoicesGewas = c("VRTSCHD", "WLSCHD", "GEWASANDR")[1:3])

# testing for special cases
expect("Andere" %in% names(schadeTable$data), "columns do not match user choices")

DT::datatable(schadeTable$data, rownames = FALSE, container = schadeTable$header,
        selection = "single", options = list(dom = 't', pageLength = -1))

## TABLE 2: Counts per type gewas ##

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


#lapply(1:length(species))
