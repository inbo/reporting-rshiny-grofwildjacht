# Tests for the Everzwijn Dashboard
# 
# Author: mvarewyck
###############################################################################


ecoData <- loadRawData(type = "eco")
ecoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]

geoData <- loadRawData(type = "geo")
geoData <- geoData[geoData$wildsoort == "Wild zwijn", ]

biotoopData <- loadHabitats(spatialData = spatialData)

load(file = file.path(dataDir, "spatialData.RData"))


# F05_1: Absoluut afschot
test_that("F05_1", {
    
    trendData <- createTrendData(
      data = ecoData,
      allSpatialData = spatialData,
      biotoopData = biotoopData,
      timeRange = c(2014, 2019),
      species = "Wild zwijn",
      regionLevel = "provinces",
      unit = "absolute"
    )
    
    myResult <- trendYearRegion(
      data = trendData, 
      locaties = "Limburg", 
      timeRange = c(2014, 2019), 
      unit = "absolute"
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })

# F05_2: Samenstelling afschot
test_that("F05_2", {
    
    myResult <- countYearAge(data = ecoData)
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })


# F16_1: Reproductie
test_that("F16_1", {
    
    plotData <- ecoData[ecoData$geslacht_comp == "Vrouwelijk", ]
    plotData$reproductiestatus <- ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig")
    
    # TODO currently onbekend excluded with disclaimer - #322
    countAgeGroup(data = plotData, groupVariable = "reproductiestatus")
    
  })
