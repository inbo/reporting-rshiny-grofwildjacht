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

# TODO for faunabeheerzone
#for (regionLevel in names(spatialData)[1:5]) {
for (regionLevel in names(spatialData)[1:3]) {
    
#    for (iSpecies in species) {
    
    for(iUnit in c("absolute", "absoluteCases")) {
        
        spaceData <- createSpaceData(
                data = schadeData@data, 
                allSpatialData = spatialData,
                year = 2018,
                species = iSpecies,
                regionLevel = regionLevel,
                unit = iUnit
        )
        
        cat("*", regionLevel, "\n")
        cat("*", iUnit, "\n")
        print(sum(spaceData$freq))
        
    }
    
    myPlot <- mapFlanders(
            allSpatialData = spatialData, 
            regionLevel = regionLevel, 
            colorScheme = c("white", RColorBrewer::brewer.pal(
                            n = nlevels(spaceData$group) - 1, name = "YlOrBr")),
            summaryData = spaceData,
            legend = "topright",
            species = iSpecies
    )
    
    print(myPlot)
    
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
