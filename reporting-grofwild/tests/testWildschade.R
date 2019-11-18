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


species <- unique(schadeData$wildsoort)


## THE MAP

### 1. Number of cases per region level

xtabs(~ wildsoort + afschotjaar, data = schadeData@data)

for (regionLevel in names(spatialData)[1:5]) {
    
#    for (iSpecies in species) {
    iSpecies <- "wild zwijn"
        
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
                timeRange = c(2014, 2018),
                species = iSpecies,
                regionLevel = regionLevel,
                unit = "absolute")
        
        # FIXME 
        if (regionLevel == "fbz_gemeentes") {
            which(!unique(spaceData$locatie) %in% spatialData$fbz_gemeentes@data$NAAM)
            tail(spaceData$locatie)
        } else {
            
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
                        timeRange = c(2014, 2018),
                        unit = "absolute") else 
                trendPlot <- trendYearRegion(
                        data = trendData,
                        timeRange = c(2014, 2018),
                        unit = "absolute",
                        locaties = trendData$locatie[1:3])
            
            print(trendPlot)
            
            
            
        }
        
#    }
    
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
