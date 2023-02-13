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

draagkrachtDir <- system.file("extdata", "maatschappelijke_draagkracht", package = "reportingGrofwild")

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
    
    everWaarnemingen <- fread(file = file.path(dataDir, "waarnemingen_2022.csv"), drop = 1)[, 
      c("wildsoort", "dataSource") := list("Wild zwijn", "waarnemingen.be")]
    #rename variables to keep
    data.table::setnames(everWaarnemingen, 
      old = c("jaar", "gemeente", "TAG"),
      new = c("afschotjaar", "gemeente_afschot_locatie", "UTM5")
    )
    everGeoAll <- rbind(everWaarnemingen, cbind(geoData, data.frame(dataSource = "afschot")), fill = TRUE)
    everGeoAll$aantal[is.na(everGeoAll$aantal)] <- 1
    
    spaceData <- createSpaceData(
      data = everGeoAll, 
      allSpatialData = spatialData,
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
    
    regionLevel <- c("utm5", "communes")[1]
    
    df <- data.table::fread(file.path(dataDir, "waarnemingen_2022.csv"))
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
      data = schadeData@data, 
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
    
    expect_s3_class(tmpDf, "data.frame")
        
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
    
    expect_s3_class(myMap, "leaflet")
    
  })


# F09_1: Meldingen schade

test_that("F07_1, F09_1, F11_1", {
    
    sources <- unique(schadeData@data$typeMelding)
    
    subData <- schadeData@data[schadeData@data$gemeente_afschot_locatie == "Aartselaar", ]
    
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
    
    inschattingData <- data.table::fread(file.path(draagkrachtDir, "Data_inschatting.csv"))
    
    myResult <- barDraagkracht(
      data = inschattingData[inschattingData$Vraag != "populatie_evolutie", ], 
      yVar = "Vraag"
    )
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })

# F12_1, F14_1, F14_2
test_that("F12_1, F14_1, F14_2", {
    
    # F12_1
    myResult <- barDraagkracht(data = data.table::fread(file.path(draagkrachtDir, "F12_1_data.csv")),
      yVar = "Jaar", xVar = "Aantal")
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    # F14_1
    myResult <- barDraagkracht(data = data.table::fread(file.path(draagkrachtDir, "F14_1_data.csv")), 
      groupVariable = "Year", yVar = "Sector")
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    
    # F14_2
    myResult <- barDraagkracht(data = data.table::fread(file.path(draagkrachtDir, "F14_2_data.csv")), 
      groupVariable = "Year", yVar = "Sector")         
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })


test_that("F14_3, F14_4", {
    
    inputFiles <- c("F14_3_data.csv", "F14_4_data.csv")
    
    # F14_3
    plotData <- data.table::fread(file.path(draagkrachtDir, inputFiles[1]))
    
    # Stakeholders
    subData <- subset(plotData, Sector %in% c('Jagers', 'Landbouwers', 'Natuurvereniging'))
    myResult <- barDraagkracht(data = subData, groupVariable = "Question_label", 
      yVar = "Sector", verticalGroups = FALSE)
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    # Plot Breed publiek
    subData <- subset(plotData, Sector %in% c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied'))
    myResult <- barDraagkracht(data = subData, groupVariable = "Question_label", yVar = "Sector")
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    
    # F14_4
    plotData <- data.table::fread(file.path(draagkrachtDir, inputFiles[2]))
#    subData <- subset(plotData, Groep %in% c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied'))
    subData <- subset(plotData, Groep %in% c('Jagers', 'Landbouwers', 'Natuurvereniging'))
    subData$Antwoord_reclass <- ifelse(subData$Antwoord_reclass == "Belangrijk", "Aanvaardbaar",
      ifelse(subData$Antwoord_reclass == "Onbelangrijk", "Niet aanvaardbaar", subData$Antwoord_reclass))
    myResult <- barDraagkracht(data = subData, groupVariable = "Question_label", yVar = "Groep")
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
    
  })


test_that("F14_5", {
    
    myResult <- barDraagkracht(data = data.table::fread(file.path(draagkrachtDir, "F14_5_data.csv")),
      groupVariable = "Question_label", yVar = "Sector")
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")
    
  })



test_that("F18_1", {
    
    plotData <- data.table::fread(file.path(draagkrachtDir, "Data_inschatting.csv"))
    
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
    
    load(file.path(dataDir, "spreadData.RData"))
    
    myMap <- mapSpread(
      spreadShape = spreadData[["municipalities_2022"]], 
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
    
    coordData <- suppressMessages(ggplot2::fortify(selectedPolygons))
    centerView <- c(range(coordData$long), range(coordData$lat))
    
    myMap <- myMap %>%
      fitBounds(lng1 = centerView[1], lng2 = centerView[2],
        lat1 = centerView[3], lat2 = centerView[4]) %>%
      clearGroup(group = "regionLines") %>%
      addPolylines(data = selectedPolygons, color = "gray", weight = 5,
        group = "regionLines")
    
    expect_s3_class(myMap, "leaflet")
    
  })


test_that("F04_3", {
    
    # Data voorbereiding
    drukjachtData <- merge(
      ecoData[ecoData$jachtmethode_comp == "Drukjacht", c("ID", "afschot_datum", "afschotjaar", "provincie")], 
      geoData[, c("ID", "WBE_Naam_Toek")], 
      by = "ID", all.x = TRUE)[, c("afschot_datum", "afschotjaar", "WBE_Naam_Toek", "provincie")]
    # Keep unique records per WBE & date
    drukjachtData <- drukjachtData[!duplicated(drukjachtData), ]
    
    myResult <- countYearProvince(data = drukjachtData)
    
    expect_type(myResult, "list")
    expect_s3_class(myResult$plot, "plotly")
    expect_s3_class(myResult$data, "data.frame")    
    
  })