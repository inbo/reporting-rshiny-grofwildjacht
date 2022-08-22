# Tests for the Everzwijn Dashboard
# 
# Author: mvarewyck
###############################################################################


ecoData <- loadRawData(type = "eco")
ecoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]

geoData <- loadRawData(type = "geo")
geoData <- geoData[geoData$wildsoort == "Wild zwijn", ]

schadeData <- loadRawData(type = "wildschade")
schadeData <- schadeData[schadeData$wildsoort == "Wild zwijn", ]

load(file = file.path(dataDir, "spatialData.RData"))

biotoopData <- loadHabitats(spatialData = spatialData)


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
      plotData$reproductiestatus <- ifelse(is.na(plotData$aantal_embryos), "Onbekend",
          ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig"))
      
      countAgeGroup(data = plotData, groupVariable = "reproductiestatus")
      
    })

# F17_1: Verspreidingsgebied afschot
test_that("F17_1", {
      
      regionLevel <- c("communes", "utm5")[2]
      
      spaceData <- createSpaceData(
          data = geoData, 
          allSpatialData = spatialData,
          year = 2016,
          species = "Wild zwijn",
          regionLevel = regionLevel,
          unit = c("absolute", "relative")[2]
      )
      
      myPlot <- mapFlanders(
          allSpatialData = spatialData, 
          regionLevel = regionLevel, 
          colorScheme = c("white", RColorBrewer::brewer.pal(
                  n = nlevels(spaceData$data$group) - 1, name = "YlOrBr")),
          summaryData = spaceData$data,
          legend = "topright",
          species = "Wild zwijn"
      )
      
      expect_s3_class(myPlot, "leaflet")
      # TODO include in the app (low priority)
      
    })


# F17_2: Verspreidingsgebied waarnemingen
test_that("F17_2", {
      
      regionLevel <- c("utm5", "communes")[1]
      
      df <- fread(file.path(dataDir, "waarnemingen_2018.csv"))
      df$wildsoort <- "Wild zwijn"
      
      spaceData <- createSpaceData(
          data = df, 
          allSpatialData = spatialData,
          year = 2018,
          species = "Wild zwijn",
          regionLevel = regionLevel,
          unit = c("absolute", "relative")[1],
          countVariable = "aantal"    
      )
      
      mapFlanders(
          regionLevel = regionLevel,
          species = "Wild zwijn",
          year = 2018,
          allSpatialData = spatialData,
          summaryData = spaceData$data,
          colorScheme = c("white", RColorBrewer::brewer.pal(
                  n = nlevels(spaceData$data$group) - 1, name = "YlOrBr")),
          legend = "topright"
      )
      # TODO include in the app (low priority)
      
    })


# F02_1: Samenstelling studiegebied

test_that("F02_1", {
      
      myResult <- barBiotoop(data = subset(biotoopData$provinces, regio %in% c("Limburg", "Antwerpen")))
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
    })

# F09_2: Kost landbouwschade

test_that("F09_2", {
      
      myResult <- barCost(
        data = schadeData@data, 
        summarizeBy = groupVariable <- c("SoortNaam", "season")[2]
      )
    
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
    })

# F03_1: Wegdensiteit

test_that("F03_1", {
    
    locaties <- c("Antwerpen", "Limburg", "Vlaams Brabant")
    
    tmpDf <- tableBackground(biotoopData, locaties)
    
    expect_s3_class(tmpDf, "data.frame")
    
    DT::datatable(tmpDf)
    
    # TODO include in the app (low priority)
    
  })


# F06_1,2,3: Verkeer
test_that("F06", {
      
      load(file = file.path(dataDir, "trafficData.RData"))
      
      myMap <- leaflet() %>%
          addTiles() %>%
          addPolylines(data = trafficData$ecorasters,
              opacity =  0.5) %>%
          addCircleMarkers(data = trafficData$oversteek,
              radius = 3,
              color = "black",
              stroke = F,
              fillOpacity = 1) 
      
      # TODO create new shiny module for the map
      ##   properties: download kaart, not data; add/remove layers using leafletproxy
      
      expect_s3_class(myMap, "leaflet")
      
    })

# F07_3: Inschatting schade
test_that("F07_3, F09_3, F11_3", {
    
    inschattingData <- fread(file.path(dataDir, "Data_inschatting.csv"))
    
    myResult <- barDraagkracht(data = inschattingData[Vraag != "populatie_evolutie", ], yVar = "Vraag")

    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })

# F12_1, F14_1, F14_2
test_that("F12_1, F14_1, F14_2", {
      
      library(ggplot2)
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
 
      # F12_1
      myResult <- barDraagkracht(data = fread(file.path(inputDir, "F12_1_data.csv")),
        xVar = "Jaar", yVar = "Aantal")
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
      # F14_1
      myResult <- barDraagkracht(data = fread(file.path(inputDir, "F14_1_data.csv")), 
        groupVariable = "Year", yVar = "Sector")
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
      
      # F14_2 (same code as above, different data)
      myResult <- barDraagkracht(data = fread(file.path(inputDir, "F14_2_data.csv")), 
        groupVariable = "Year", yVar = "Sector")         
 
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
    # TODO include in the app (low priority)
    
    })


test_that("F14_3, F14_4", {
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
      inputFile <- c("F14_3_data.csv", "F14_4_data.csv")[1]
      
      plotData <- fread(file.path(inputDir, inputFile))
      
      # Stakeholders
      subData <- subset(plotData, Sector %in% c('Jagers', 'Landbouwers', 'Natuurvereniging'))
      myResult <- barDraagkracht(data = subData, groupVariable = "Sector", yVar = "Question_label")
           
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
      # Plot Breed publiek
      subData <- subset(plotData, Sector %in% c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied'))
      myResult <- barDraagkracht(data = subData, groupVariable = "Sector", yVar = "Question_label")
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
    # TODO include in the app (low priority)
      
    })

  
test_that("F14_5", {
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"

      myResult <- barDraagkracht(data = fread(file.path(inputDir, "F14_5_data.csv")),
        groupVariable = "Sector", yVar = "Question_label")
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "plotly")
      expect_s3_class(myResult$data, "data.frame")
      
    # TODO include in the app (low priority)
      
    })


test_that("F18_1", {
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
      
      plotData <- fread(file.path(inputDir, "Data_inschatting.csv"))
      
      barDraagkracht(data = plotData[Vraag == "populatie_evolutie", ], yVar = "Vraag")
      
      # Remark:
      # There is a problem with hovertemplate when there is only 1 data point on the plot:
      # https://github.com/plotly/plotly.R/issues/1859
    
  # TODO include in the app (low priority)
    
    })



test_that("F17_4", {
    
    mapSpread(
      spatialDir = "~/git/reporting-rshiny-grofwildjacht/dashboard/input/spatial",
      spatialLevel = c("pixels", "municipalities")[2],
      unit = c("model_EP", "model_OH", "risk_EP", "risk_OH")[3],
      legend = "bottomright",
      addGlobe = TRUE
    )
    
  # TODO include in the app (low priority)
    
  })