# Tests for the Everzwijn Dashboard
# 
# Author: mvarewyck
###############################################################################



context("Test Everzwijn Dashboard")

ecoData <- loadRawData(type = "eco")
ecoData <- ecoData[ecoData$wildsoort == "Wild zwijn", ]

geoData <- loadRawData(type = "geo")
geoData <- geoData[geoData$wildsoort == "Wild zwijn", ]

schadeData <- suppressWarnings(loadRawData(type = "wildschade"))
schadeData <- schadeData[schadeData$wildsoort == "Wild zwijn", ]

readS3(file = "spatialData_sf.RData")

waarnemingenData <- loadRawData(type = "waarnemingen")
# Restrict all to same date
waarnemingenData <- waarnemingenData[waarnemingenData$afschotjaar <= 
    format(max(ecoData$afschot_datum, na.rm = TRUE), "%Y"), ]

everGeoAll <- rbind(
  # waarnemingen
  data.table::as.data.table(waarnemingenData),
  # afschot
  geoData,
  fill = TRUE)

biotoopData <- loadHabitats()


# F05_1: Absoluut afschot
test_that("F05_1", {
    
    trendData <- createTrendData(
      data = ecoData,
      allSpatialData = spatialData,
      biotoopData = biotoopData$provinces,
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
    
    myResult <- countAgeGroup(data = plotData, groupVariable = "reproductiestatus")
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })

# F17_1: Verspreidingsgebied afschot
test_that("F17_1", {
    
    regionLevel <- c("communes", "utm5")[2]
    
    spaceData <- createSpaceData(
      data = everGeoAll, 
      allSpatialData = spatialData,
      biotoopData = biotoopData[[regionLevel]],
      year = 2016,
      species = "Wild zwijn",
      regionLevel = regionLevel,
      sourceIndicator = "waarnemingen.be",
      unit = c("absolute", "relative")[2],
      countVariable = "aantal"
    )
    
    myPlot <- mapFlanders(
      allSpatialData = spatialData, 
      regionLevel = regionLevel, 
      colorScheme = c("white", RColorBrewer::brewer.pal(
          n = nlevels(spaceData$data$group) - 1, name = "YlOrBr")),
      summaryData = spaceData$data,
      legend = "topright",
      species = "Wild zwijn",
      borderRegion = "provinces",
      borderLocaties = c("Antwerpen", "Limburg")
    )
    
    expect_s3_class(myPlot, "leaflet")
    
  })


# F17_2: Verspreidingsgebied waarnemingen
test_that("F17_2", {
    
    skip("Currently not in the app")
    
    regionLevel <- c("utm5", "communes")[1]
    
    df <- readS3(FUN = data.table::fread, file = "waarnemingen_wild_zwijn_processed.csv")
    
    spaceData <- createSpaceData(
      data = df, 
      allSpatialData = spatialData,
      year = 2018,
      species = "Wild zwijn",
      regionLevel = regionLevel,
      unit = c("absolute", "relative")[1],
      countVariable = "aantal"    
    )
    
    myPlot <- mapFlanders(
      regionLevel = regionLevel,
      species = "Wild zwijn",
      year = 2018,
      allSpatialData = spatialData,
      summaryData = spaceData$data,
      colorScheme = c("white", RColorBrewer::brewer.pal(
          n = nlevels(spaceData$data$group) - 1, name = "YlOrBr")),
      legend = "topright"
    )
    
    expect_s3_class(myPlot, "leaflet")
    
  })


# F02_1: Samenstelling studiegebied

test_that("F02_1", {
    
    subData <- subset(biotoopData$provinces, regio %in% c("Limburg", "Antwerpen"))
    
    # Per province
    myResult <- barBiotoop(data = subData)
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    # Aggregated
    subData$regio <- "Totaal"
    subData <- merge(
      aggregate(subData[, grepl("Area", colnames(subData))], 
        by = list(regio = subData$regio), FUN = sum),
      aggregate(subData[, grepl("perc", colnames(subData))], 
        by = list(regio = subData$regio), FUN = mean)
    )
    barBiotoop(data = subData)$plot
    
  })


# F09_2: Kost landbouwschade

test_that("F09_2", {
    
    myResult <- barCost(
      data = sf::st_drop_geometry(schadeData), 
      unit = c("SoortNaam", "season")[2]
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })

# F03_1,2,3,4: Wegdensiteit

test_that("F03_1", {
    
    locaties <- c("Antwerpen", "Limburg", "Vlaams Brabant")
    regionLevel <- "provinces"
    
    # Subset data
    toReport <- subset(biotoopData[[regionLevel]], regio %in% locaties)
    # Add Vlaams Gewest
    toReport <- rbind(toReport, biotoopData$flanders)
    
    tmpDf <- tableBackground(data = toReport)
    
    expect_type(tmpDf, "list")
        
  })


# F06_1,2,3: Verkeer
test_that("F06", {
    
    skip("Currently not used")
    
    readS3(file = "trafficData.RData")
    
    
    myMap <- leaflet() %>%
      addTiles() %>%
      addPolylines(data = trafficData$ecorasters,
        opacity =  0.5) %>%
      addCircleMarkers(data = trafficData$oversteek,
        radius = 3,
        color = "black",
        stroke = F,
        fillOpacity = 1) 
    
    expect_s3_class(myMap, "leaflet")
    
  })


# F09_1: Meldingen schade

test_that("F07_1, F09_1, F11_1", {
    
    sources <- unique(schadeData$typeMelding)
    
    subData <- schadeData[schadeData$gemeente_afschot_locatie == "Bilzen", ]
    
    myResult <- barCost(
      data = subset(subData, typeMelding %in% sources[4]), 
      yVar = "count",
      unit = "season" # c("SoortNaam", "season")[1]
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })


# F07_3: Inschatting schade
test_that("F07_3, F09_3, F11_3", {
    
    skip("Currently not used")
    
    inschattingData <- readS3(FUN = data.table::fread, file = "Data_inschatting.csv")
    
    myResult <- barDraagkracht(
      data = inschattingData[inschattingData$Vraag != "populatie_evolutie", ], 
      yVar = "Vraag"
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })

draagvlakData <- loadDraagvlakData()

# F14_1, F14_3, F14_4, F14_5
test_that("F14_1, F14_3, F14_4, F14_5", {
    
    for (plotName in c("F14_1", "F14_3", "F14_4", "F14_5")) {
      
      subData <- switch(plotName, 
        "F14_1" = draagvlakData$aanwezigheid,
        "F14_2" = draagvlakData$aantrekkingskracht,
        "F14_3" = draagvlakData$impacts,
        "F14_4" = draagvlakData$maatregelen,
        "F14_5" = draagvlakData$beleid
      )
      
      subData <- subData[subData$Soort == "Wild zwijn", ]
      
      myResult <- pciDraagvlak(data = subData, 
        yVar = if (plotName == "F14_1") "Year" else "vraag_label") 
      
      expect_type(myResult, "list")
      expect_s3_class(myResult$plot, "ggplot")
      expect_s3_class(myResult$data, "data.frame")
      
    }

  })


test_that("F14_2", {
    
    myResult <- barDraagkracht(data = draagvlakData$aantrekkingskracht,
      yVar = "vraag_label")
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })


test_that("F18_1", {
    
    skip("Currently not used")
    
    plotData <- readS3(FUN = data.table::fread, file = "Data_inschatting.csv")
    
    myResult <- barDraagkracht(data = plotData[Vraag == "populatie_evolutie", ], yVar = "Vraag")
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    # Remark:
    # There is a problem with hovertemplate when there is only 1 data point on the plot:
    # https://github.com/plotly/plotly.R/issues/1859
    
  })


# Toekomstig verspreidingsgebied
test_that("F17_4", {
    
    readS3(file = "spreadData_sf.RData")
    
    myMap <- mapSpread(
      spreadShape = spreadData[[1]], 
      legend = "bottomright",
      addGlobe = TRUE
    )
    
    # Zoom in on specific region
    locaties <- c("Antwerpen", "Limburg")
    
    tmpSpatial <- filterSpatial(
      allSpatialData = spatialData, 
      species = "Wild zwijn", 
      regionLevel = "provinces", 
      year = NULL,
      locaties = locaties
    )
    selectedPolygons <- subset(tmpSpatial, 
      tmpSpatial$NAAM %in% locaties)
    
    centerView <- getCenterView(selectedPolygons)
    
    myMap <- myMap %>%
      leaflet::fitBounds(lng1 = centerView[1], lng2 = centerView[2],
        lat1 = centerView[3], lat2 = centerView[4]) %>%
      leaflet::clearGroup(group = "regionLines") %>%
      leaflet::addPolylines(data = selectedPolygons, color = "black", weight = 5,
        group = "regionLines")
    
    expect_s3_class(myMap, "leaflet")
    
  })


test_that("F04_3", {
    
    colsGeo <- c("afschotplan_nummer", "postcode_afschot_locatie", 
      "FaunabeheerZone", "gemeente_afschot_locatie"
    )
    # Data voorbereiding
    drukjachtData <- merge(
      ecoData[ecoData$jachtmethode_comp == "Drukjacht", c("ID", "afschot_datum", "afschotjaar", "provincie")], 
      geoData[, c("ID", colsGeo)], 
      by = "ID", all.x = TRUE)
    # Keep unique records per WBE & date
    drukjachtData <- drukjachtData[!duplicated(drukjachtData[, c("afschotplan_nummer", "afschot_datum")]), ]
    
    myResult <- countYearProvince(data = drukjachtData)
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")    
    
  })


test_that("F18_8 Kencijfers table", {
    
    summaryData <- summarizeKencijferData(
      geoData = everGeoAll,
      biotoopData = biotoopData$communes,
      unit = c("absolute", "relative", "relativeDekking")[1]
    )
    # Test case 1: Check if the function runs without errors
    expect_s3_class(everGeoAll, "data.table")
    result <- tableKencijfers(data = summaryData, jaar = 2020, thresholdAfschot = 1, thresholdWaarnemingen = 1)
    expect_is(result, "list")
    
    # Test case 2: Check if the result table has the correct structure
    expect_true("htmlTable" %in% names(result))
    expect_true("pdfTable" %in% names(result))
    expect_true("data" %in% names(result))
    expect_length(unique(result$data[,1]), 4)
    
    expect_equal(
      as.numeric(unique(result$data[result$data[,1] == "Dezelfde gemeentes",2])) +
        as.numeric(unique(result$data[result$data[,1] == "Nieuwe gemeentes",2])) ,
      as.numeric(result$data[1,2])
    )
    
  })
