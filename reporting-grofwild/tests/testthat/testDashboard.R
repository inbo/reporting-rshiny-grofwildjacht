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
      
      groupVariable <- c("SoortNaam", "season")[1]
      # TODO bedrag column will change #325
      
      subData <- subset(schadeData@data, schadeBasisCode == "GEWAS",
          select = c("schadeBedrag", groupVariable, "afschotjaar"))
      
      summaryData <- count(df = subData, vars = names(subData))
      summaryData$schadeBedrag <- summaryData$schadeBedrag * summaryData$freq
      summaryData$freq <- NULL
      plotData <- aggregate(summaryData$schadeBedrag, by = summaryData[, c(groupVariable, "afschotjaar")], 
          FUN = sum, na.rm = TRUE)
      plotData <- plotData[plotData$x != 0, ]
      
      # TODO new plot function - barCost.R (to be checked)
      # plot per year (xaxis) and group (color): freq x minBedrag (yaxis)         
      costPlots <-  barCost(plotData)
      
      costPlots$barPlot
      costPlots$linePlot
      
      # Remarks:
      # - For some lines the hoverText isn't rendered
      # - Does the lineplot need to use relative values? (as in pdf file?)
      
    })

# F03_1: Wegdensiteit

test_that("F03_1", {
      
#    regionLevel <- "provinces"
      locaties <- c("Antwerpen", "Limburg", "Vlaams Brabant")
      
      # TODO create new function for table: tableBackground.R (see e.g. tableProvince.R) (to be checked)
      # Remarks:
      # - Value for Vlaams gewest is added as extra row rather than as extra column as in pdf file. 
      # since it would always hold the same value for each row. Can set it in bold?
      # - Round values?
      
      
      tmpDf <- tableBackground(biotoopData, locaties)
      
      DT::datatable(tmpDf)
      
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

# F07_3: Inschatting verkeer
test_that("F07_3, F09_3, F11_3", {
      
      inschattingData <- fread(file.path(dataDir, "Data_inschatting.csv"))
      
      verkeer_inschatting <- subset(inschattingData, Vraag != "populatie_evolutie")
      verkeer_inschatting$percentage <- as.numeric(verkeer_inschatting$percentage)
      
      verkeer_inschatting$Antwoord <- factor(verkeer_inschatting$Antwoord , 
          levels = c('Erg veel toegenomen','Veel toegenomen',
              'Beetje toegenomen', 'Hetzelfde gebleven',
              'Beetje afgenomen', 'Veel afgenomen',
              'Erg veel afgenomen', 'Geen mening'))
      
      ggplot2::ggplot(data = verkeer_inschatting,
              ggplot2::aes(x = percentage, y = Vraag, fill = Antwoord)) +
          ggplot2::geom_bar(stat = "identity", position = "stack")
      
# TODO create new plotly graph: barInschatting.R (to be checked)
      barInschatting(verkeer_inschatting)
      # Remarks:
      # - didn't add the percentages on the bars: can be seen when hovering over the area + 
      # if added they would still be there when deselecting a certain category
      
    })

# F12_1, F14_1, F14_2
test_that("F12_1, F14_1, F14_2", {
      
      library(ggplot2)
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"

      # TODO create new plotly graphs: barDraagkracht.R (to be checked)
      ## if possible merge with code from barInschatting.R -> then rename to barQuestionnaire.R: currently not implemented this way but can look into it
      
      # F12_1
      plotData <- fread(file.path(inputDir, "F12_1_data.csv"))
      ggplot2::ggplot(data = plotData,
              ggplot2::aes(x = Jaar, y = Aantal, fill = Type)) +
          ggplot2::geom_bar(stat = "identity", position = "stack") 
      
      # plotly
      barDraagkracht(plotData, ficheNumber = "F12_1")
      
      # F14_1
      plotData <- fread(file.path(inputDir, "F14_1_data.csv"))
      plotData$percentage <- as.numeric(plotData$percentage)
      plotData$Antwoord <- factor(plotData$Antwoord , 
          levels = c('Heel erg positief', 'Positief', 'Neutraal',
              'Negatief', 'Heel erg negatief', 'Geen mening'))
      
      ggplot(data = plotData,
              aes(x = percentage, y = Sector, fill = Antwoord)) +
          geom_bar(stat = "identity", position = "stack") +
          facet_wrap(~ Year)
      
      # plotly
      barDraagkracht(plotData, ficheNumber = "F14_1")

      # F14_2 (same code as above, different data)
      plotData <- fread(file.path(inputDir, "F14_2_data.csv"))
      plotData$percentage <- as.numeric(plotData$percentage)
      plotData$Antwoord <- factor(plotData$Antwoord , 
          levels = c('Ja, zeker wel', 'Ja, waarschijnlijk wel', 
              'Nee, waarschijnlijk niet', 'Nee, zeker niet'))
      
      ggplot(data = plotData, 
              aes(x = percentage, y = Sector, fill = Antwoord)) +
          geom_bar(stat = "identity", position = "stack") +
          facet_wrap(~ Year)
      
      # plotly
      barDraagkracht(plotData, ficheNumber = "F14_2")
          
 
    })


test_that("F14_3, F14_4", {
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
      inputFile <- c("F14_3_data.csv", "F14_4_data.csv")[1]
      
      # TODO create new plotly graphs + make data more uniform so same function applies for F14_3, F14_4, F14_5, F18_1
      plotData <- fread(file.path(inputDir, inputFile))
      plotData$percentage <- as.numeric(plotData$percentage)
      
      library(ggplot2)
      # Stakeholders
      stakeholders <- c('Jagers', 'Landbouwers', 'Natuurvereniging')
      
      subData <- subset(plotData, Sector %in% stakeholders)
      
      subData$Antwoord <- factor(subData$Antwoord , 
          levels = c('Geen idee', 'Zeer groot', 'Groot',
              'Klein', 'Zeer klein', 'Onbestaand'))
      
      ggplot(data = subData,
              aes(x = percentage, y = Question_label, fill = Antwoord)) +
          geom_bar(stat = "identity", position = "stack") +
          facet_grid( ~ Sector)
      
      
      # Plot Breed publiek
      breed_publiek <- c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied')
      
      subData <- subset(plotData, Sector %in% breed_publiek)
      
      subData$Antwoord <- factor(subData$Antwoord , 
          levels = c('Geen idee', 'Zeer groot', 'Groot',
              'Klein', 'Zeer klein', 'Onbestaand'))
      
      ggplot(data = subData,
              aes(x = percentage,
                  y = Question_label,
                  fill = Antwoord)) +
          geom_bar(stat = "identity", 
              position = "stack") +
          facet_grid( ~ Sector)
      
    })

test_that("F14_5", {
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
      
      # TODO create new plotly graphs + integrate with F14_3 and F14_4?
      plotData <- fread(file.path(inputDir, "F14_5_data.csv"))
      plotData$percentage <- as.numeric(plotData$percentage)
      library(ggplot2)
      
      plotData$Antwoord <- factor(plotData$Antwoord , 
          levels = c('Erg belangrijk', 'Belangrijk', 'Neutraal',
              'Niet belangrijk', 'Helemaal niet belangrijk',
              'Geen mening'))
      
      ggplot(data = plotData,
              aes(x = percentage, y = Question_label, fill = Antwoord)) +
          geom_bar(stat = "identity", position = "stack") +
          facet_grid( ~ Sector)
      
    })



test_that("F18_1", {
      
      # Maatschappelijke draagkracht
      inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
      
      # TODO create new plotly graphs -> integrate with code from F14_3 and F14_4
      plotData <- read.csv(file.path(inputDir, "Data_inschatting.csv"))
      plotData$percentage <- as.numeric(plotData$percentage)
      library(ggplot2)
      
      subData <- subset(plotData, Vraag == "populatie_evolutie")
      
      subData$Antwoord <- factor(subData$Antwoord , 
          levels = c('Erg veel toegenomen','Veel toegenomen',
              'Beetje toegenomen', 'Hetzelfde gebleven',
              'Beetje afgenomen', 'Veel afgenomen',
              'Erg veel afgenomen', 'Geen mening'))
      
      ggplot(data = subData[order(subData$Score), ],
              aes(x = percentage, y = Vraag, fill = Antwoord)) +
          geom_bar(stat = "identity", position = "stack")
      
    })



test_that("F17_4", {
    
    mapSpread(
      spatialDir = "~/git/reporting-rshiny-grofwildjacht/dashboard/input/spatial",
      spatialLevel = c("pixels", "municipalities")[2],
      unit = c("model_EP", "model_OH", "risk_EP", "risk_OH")[3],
      legend = "bottomright",
      addGlobe = TRUE
    )
    
  })