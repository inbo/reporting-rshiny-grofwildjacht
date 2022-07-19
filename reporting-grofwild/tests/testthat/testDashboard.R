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
    plotData$reproductiestatus <- ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig")
    
    # TODO currently onbekend excluded with disclaimer - #322
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
    
  })


# F17_2: Verspreidingsgebied waarnemingen
test_that("F17_2", {
    
    regionLevel <- "utm1"
    
    # TODO how to filter on region? Attach to waarnemingen province, fbz and gemeente
    
    df <- fread(file.path(dataDir, "waarnemingen_2018.csv"))
    nOccurred <- as.data.frame(table(df[[regionLevel]]))
    
    spaceData <- createSpaceData(
      data = df, 
      allSpatialData = spatialData,
      year = NULL,
      species = "Wild zwijn",
      regionLevel = "utm1",
      unit = c("absolute", "relative")[1]
    )
    
    mapFlanders(
      regionLevel = regionLevel,
      species = "Wild zwijn",
      year = NULL,
      allSpatialData = spatialData,
      summaryData = spaceData$data,
      colorScheme = "YlOrBr" 
    )
    
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
    
    # TODO plot function
    # plot per year (xaxis) and group (color): freq x minBedrag (yaxis)
    
    subData <- subset(schadeData@data, schadeBasisCode == "GEWAS",
      select = c("schadeBedrag", groupVariable, "afschotjaar"))
    
    summaryData <- count(df = subData, vars = names(subData))
    summaryData$schadeBedrag <- summaryData$schadeBedrag * summaryData$freq
    summaryData$freq <- NULL
    plotData <- aggregate(summaryData$schadeBedrag, by = summaryData[, c(groupVariable, "afschotjaar")], 
      FUN = sum, na.rm = TRUE)
    plotData <- plotData[plotData$x != 0, ]
    
  })

# F03_1: Wegdensiteit

test_that("F03_1", {
    
    regionLevel <- "provinces"
    locaties <- c("Antwerpen", "Limburg")
    
    # TODO create function for table
    toReport <- subset(habitatData$provinces, regio %in% locaties, select = c("regio", "weg_dens_km"))
    toReport <- rbind(toReport, habitatData$flanders[, c("regio", "weg_dens_km")])
    toReport[,2] <- toReport[,2]*100
    
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

# F07_3: Inschatting verkeer
test_that("F07_3, F09_3, F11_3", {
    
    inschattingData <- fread(file.path(dataDir, "Data_inschatting.csv"))
    
    # TODO create plotly graph
    library(tidyverse)
    library(ggplot2)
    verkeer_inschatting <- inschattingData %>%
      filter(Vraag != "populatie_evolutie")
    
    verkeer_inschatting$Antwoord <- factor(verkeer_inschatting$Antwoord , 
      levels = c('Erg veel toegenomen','Veel toegenomen',
        'Beetje toegenomen', 'Hetzelfde gebleven',
        'Beetje afgenomen', 'Veel afgenomen',
        'Erg veel afgenomen', 'Geen mening'))
    
    plot_inschatting_verkeer <- ggplot(data = verkeer_inschatting,
        aes(x = percentage,
          y = Vraag,
          fill = Antwoord)) +
      geom_bar(stat = "identity", 
        position = "stack")
    
    plot_inschatting_verkeer
    
  })

# F12_1, F14_1, F14_2
test_that("F12_1, F14_1, F14_2", {
    
    # Maatschappelijke draagkracht
    inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
    
    # TODO create plotly graphs
    
    # F12_1
    plotData <- fread(file.path(inputDir, "F12_1_data.csv"))
    library(tidyverse)
    library(ggplot2)
    myPlot <- ggplot(data = plotData,
        aes(x = Jaar, y = Aantal, fill = Type)) +
      geom_bar(stat = "identity", position = "stack") 
    
    # F14_1
    plotData <- fread(file.path(inputDir, "F14_1_data.csv"))
    plotData$Antwoord <- factor(plotData$Antwoord , 
      levels = c('Heel erg positief', 'Positief', 'Neutraal',
        'Negatief', 'Heel erg negatief', 'Geen mening'))
    
    myPlot <- ggplot(data = plotData,
        aes(x = percentage, y = Sector, fill = Antwoord)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ Year)
    
    # F14_2 (same code as above, different data
    plotData <- fread(file.path(inputDir, "F14_2_data.csv"))
    plotData$Antwoord <- factor(plotData$Antwoord , 
      levels = c('Ja, zeker wel', 'Ja, waarschijnlijk wel', 
        'Nee, waarschijnlijk niet', 'Nee, zeker niet'))
    
    myPlot <- ggplot(data = plotData, 
        aes(x = percentage, y = Sector, fill = Antwoord)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ Year)
    
  })


test_that("F14_3, F14_4", {
    
    # Maatschappelijke draagkracht
    inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
    inputFile <- c("F14_3_data.csv", "F14_4_data.csv")[1]
    
    # TODO create plotly graphs + make data more uniform so same function applies for F14_3, F14_4, F14_5, F18_1
    plotData <- fread(file.path(inputDir, inputFile))
    library(tidyverse)
    library(ggplot2)
    # Stakeholders
    stakeholders <- c('Jagers', 'Landbouwers', 'Natuurvereniging')
    
    subData <- subData %>% filter(Sector %in% stakeholders)
    
    subData$Antwoord <- factor(subData$Antwoord , 
      levels = c('Geen idee', 'Zeer groot', 'Groot',
        'Klein', 'Zeer klein', 'Onbestaand'))
    
    myPlot <- ggplot(data = subData,
        aes(x = percentage, y = Question_label, fill = Antwoord)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid( ~ Sector)
    
    
    # Plot Breed publiek
    breed_publiek <- c('Publiek buiten everzwijngebied', 'Publiek in everzwijngebied')
    
    subData <- plotData %>% filter(Sector %in% breed_publiek)
    
    subData$Antwoord <- factor(subData$Antwoord , 
      levels = c('Geen idee', 'Zeer groot', 'Groot',
        'Klein', 'Zeer klein', 'Onbestaand'))
    
    myPlot <- ggplot(data = subData,
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
    
    # TODO create plotly graphs + integrate with F14_3 and F14_4
    plotData <- fread(file.path(inputDir, "F14_5_data.csv"))
    library(tidyverse)
    library(ggplot2)
    
    plotData$Antwoord <- factor(plotData$Antwoord , 
      levels = c('Erg belangrijk', 'Belangrijk', 'Neutraal',
        'Niet belangrijk', 'Helemaal niet belangrijk',
        'Geen mening'))
    
    myPlot <- ggplot(data = plotData,
        aes(x = percentage, y = Question_label, fill = Antwoord)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid( ~ Sector)
    
  })

test_that("F17_4", {
    
    spatialDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/spatial"
    spatialFile <- c(
      # pixels
      "Pixels_ModelOutput_toekomst_verspr_2022.shp",
      # gemeente
      "Municipalities_ModelOutput_toekomst_verspr_2022.shp"
    )[2]
    unitVar <- if (grepl("Pixels", spatialFile))
        # Model output exact pixels / Model output optimal habitat / AVP risk exact pixels / AVP risk optimal habitat
        c("Mdl_EP_", "Mdl_OH_", "Rsc_ExP", "Rsc_OpH")[1] else   
        c("M_EP_A_", "M_OH_A_", "M_EP__G_", "M_OH__G_")[4] 
    
    baseMap <- rgdal::readOGR(file.path(spatialDir, spatialFile)) %>%
      sp::spTransform(CRS("+proj=longlat +datum=WGS84"))
    
    # Modify data
    ## Risico
    riskLevels <- c("Hoog risico", "Gemiddeld risico", "Laag risico", "Verwaarloosbaar risico") 
    if (grepl("Pixels", spatialFile)) {
      baseMap$Rsc_ExP <- factor(baseMap$Rsc_ExP, levels = riskLevels)
      baseMap$Rsc_OpH <- factor(baseMap$Rsc_ExP, levels = riskLevels)
    } else {
      baseMap$M_EP__G_ <- factor (baseMap$M_EP__G_, levels = riskLevels)
      baseMap$M_OH__G_ <- factor (baseMap$M_OH__G_, levels = riskLevels)
    }
    
    
    # TODO create function: similar code structure in mapFlanders.R: mapFlanders(), mapFlandersServer(), mapFlandersUI()
    # baseMap should be function argument: load both when the app starts and pass to the function
    # other function arguments: 
    #   spatialLevel = c("pixels", "communes"); replace in code below where spatialFile is used
    #   unitVar: one of choices above (include check whether unitVar can be chosen for spatialLevel)
    
    library(leaflet)
    
    # Create map
    finalMap <- leaflet(baseMap) %>%
      setView(lng = 4.403268, lat  = 51.094453, zoom = 8) %>%
      # TODO ask Anneleen: differ background map & zoom for gemeente and pixel?
      addProviderTiles(providers$CartoDB.Positron)
#    addTiles() # gemeente
    
    modelShape <- subset(baseMap, !is.na(baseMap@data[, unitVar]))
    modelShape[[unitVar]] <- as.factor(modelShape[[unitVar]])
    pal_model <- colorFactor(
      palette = if (grepl("Mdl", unitVar))
          c("deepskyblue1", "deepskyblue3", "deepskyblue4") else if (grepl("Rsc", unitVar))
          c('red', 'orange', 'green', 'white') else
          c('green', 'yellow', 'orange', 'red', 'darkgrey'), 
      domain = modelShape[[unitVar]],
      na.color = NA) 
    
    # TODO some layers same across unitVar -> leafletproxy in the app (see mapFlandersServer(), need 'group =' in addPolygons())
    # create helper function for each palette, which can then be called in leafletProxy
    if (unitVar %in% c("Mdl_EP_", "Mdl_OH_")) {
    
      ## Habitat classes - Model output
      baseMap$Hbtt_ct <- factor(baseMap$Hbtt_ct, 
        levels = c("Hoge Geschiktheid","Geschikt", "Lage geschiktheid", "Niet geschikt"))
      
      pal_hab <- colorFactor(
        palette = c("darkgreen", "green", "yellow", "grey"), 
        domain = baseMap$Hbtt_ct)
      
      finalMap <- finalMap %>%  
        addPolygons(stroke = FALSE,
          smoothFactor = 1,
          fillOpacity = 0.5,
          fillColor =  ~pal_hab(Hbtt_ct)) %>%
        addLegend("bottomleft", pal = pal_hab, values = ~Hbtt_ct,
          title = "Habitatsgeschiktheid",
          opacity = 1)
      
    }
    
    finalMap <- finalMap %>%
      addPolygons(data = modelShape,
        stroke = grepl("Municipalities", spatialFile),
        smoothFactor = 1,
        fillOpacity = if (grepl("Pixels", spatialFile)) 1 else 0.5,
        fillColor =  ~pal_model(modelShape[[unitVar]]),
        weight = if (grepl("Pixels", spatialFile)) 0 else 0.75,
        color = "black") %>%
      addLegend("bottomright", pal = pal_model, values = ~get(unitVar),
        title = if (grepl("Mdl", unitVar))
            "Waarschijnlijkheid verspreiding" else 
            "Risico klasse",
        opacity = 1,
        na.label = "")
    
    startVar <- switch(unitVar,
      Mdl_EP_ = "Strt_EP",
      Mdl_OH_ = "Strt_OH",
      NULL
    )
    
    if (!is.null(startVar) && startVar %in% colnames(baseMap@data)) {
      
      startShape <- subset(baseMap, baseMap@data[[startVar]] == 2019)
      pal_start <- colorFactor(
        palette = c("black"), 
        domain = startShape[[startVar]],
        na.color = NA)  
      
      finalMap <- finalMap %>%
        addPolygons(data = startShape,
          stroke = FALSE,
          smoothFactor = 1,
          fillOpacity = 1,
          fillColor =  ~pal_start(get(startVar))) %>%
        addLegend("bottomright", pal = pal_start, values = ~get(startVar),
          title = "Startlocatie",
          opacity = 1)
    }
    
    finalMap
    
  })


test_that("F18_1", {
    
    # Maatschappelijke draagkracht
    inputDir <- "~/git/reporting-rshiny-grofwildjacht/dashboard/input/maatschappelijke_draagkracht"
    
    # TODO create plotly graphs + integrate with code from F14_3 and F14_4
    plotData <- read.csv(file.path(inputDir, "Data_inschatting.csv"))
    
    library(tidyverse)
    library(ggplot2)
    
    subData <- plotData %>% filter(Vraag == "populatie_evolutie")
    
    subData$Antwoord <- factor(subData$Antwoord , 
      levels = c('Erg veel toegenomen','Veel toegenomen',
        'Beetje toegenomen', 'Hetzelfde gebleven',
        'Beetje afgenomen', 'Veel afgenomen',
        'Erg veel afgenomen', 'Geen mening'))
    
    myPlot <- ggplot(data = subData[order(subData$Score), ],
        aes(x = percentage, y = Vraag, fill = Antwoord)) +
      geom_bar(stat = "identity", position = "stack")
    
  })

