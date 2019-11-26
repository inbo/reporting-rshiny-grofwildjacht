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
wildSchadeData <- subset(schadeData@data, wildsoort %in% c("wild zwijn", "edelhert", "ree")[1])

species <- unique(schadeData$wildsoort)


## THE MAP

### 1. Number of cases per region level

xtabs(~ wildsoort + afschotjaar, data = schadeData@data)

for (regionLevel in names(spatialData)[1:5]) {
    
    for (iSpecies in species) {
    
    spaceData <- createSpaceData(
            data = schadeData@data, 
            allSpatialData = spatialData,
            year = 2018,
            species = iSpecies,
            regionLevel = regionLevel,
            unit = "absolute"
    )
    
    cat("*", regionLevel, "\n")
    cat("*", iSpecies, "\n")
    print(sum(spaceData$freq))
    
    trendData <- createTrendData(
            data = schadeData@data,
            allSpatialData = spatialData,
            timeRange = c(2006, 2019),
            species = iSpecies,
            regionLevel = regionLevel,
            unit = "absolute")
    
        mapPlot <- mapFlanders(
                allSpatialData = spatialData, 
                regionLevel = regionLevel, 
                colorScheme = c("white", RColorBrewer::brewer.pal(
                                n = nlevels(spaceData$group) - 1, name = "YlOrBr")),
                summaryData = spaceData,
                legend = "topright",
                species = iSpecies
        )
        print(mapPlot)
        
        if (regionLevel == "flanders")
            trendPlot <- trendYearFlanders(
                    data = trendData,
                    timeRange = c(2006, 2019),
                    unit = "absolute") else 
            trendPlot <- trendYearRegion(
                    data = trendData,
                    timeRange = c(2014, 2018),
                    unit = "absolute",
                    locaties = trendData$locatie[1:3])
        
        print(trendPlot)
        
        
        
    }
    
}



### 2. Map with exact location and description of each case

for (iSpecies in species) {
    
    myPlot <- mapSchade(
            schadeData = schadeData,
            regionLevel = "provinces",
            allSpatialData = spatialData,
            addGlobe = TRUE)
    
    print(myPlot)
    
}



### 3. Descriptive plots


## PLOT 1: Counts per year and province ##

allPlots <- lapply(species, function(iSpecies) {
            
            plotData <- subset(schadeData, wildsoort == iSpecies)
            timeRange <- min(plotData$afschotjaar):max(plotData$afschotjaar)
            
            res <- countYearProvince(data = plotData@data, jaartallen = timeRange)
            
            expect_equal(names(res), c("plot", "data"))
            expect_equal(names(res$data), c("afschotjaar", "locatie", "value"))
            
            res
            
        })


# Some special cases
countYearProvince(data = wildSchadeData, jaartallen = 2018, type = "faunabeheerzones")
countYearProvince(data = wildSchadeData, jaartallen = 2006:2019, type = "flanders")


## PLOT 2: Counts per year and variable of interest ##

# count
countYearSchade(data = wildSchadeData, type = "SoortNaam")$plot
countYearSchade(data = schadeData@data, type = "wildsoort")$plot
countYearSchade(data = schadeData@data, type = "schadeCode")$plot

# percent
countYearSchade(data = schadeData@data, type = "schadeCode", summarizeBy = "percent")$plot
