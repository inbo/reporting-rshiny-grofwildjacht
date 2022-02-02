# Server file for WBE page
# 
# Author: mvarewyck
###############################################################################




## Filter Data ##

results$wbe_geoData <- reactive({
    
    subset(geoData, wildsoort == req(input$wbe_species))
    
  })

results$wbe_combinedData <- reactive({
    
    ecoData <- subset(ecoData, wildsoort == req(input$wbe_species))
    
    # Combine data
    commonNames <- names(ecoData)[names(ecoData) %in% names(results$wbe_geoData())]
    combinedData <- merge(results$wbe_geoData(), ecoData, 
      by = commonNames, all.x = TRUE)
    
  })

results$wbe_spatialData <- reactive({
    
    spatialData <- filterSpatial(
      allSpatialData = spatialData, 
      species = req(input$wbe_species), 
      regionLevel = "WBE_binnengrenzen", 
      year = req(input$wbe_year)
    )
    
    req(spatialData)
    validate(need(any(currentWbe %in% spatialData@data$NAAM), "Geen data beschikbaar"))
    
    spatialData
    
  })


results$wbe_timeRange <- reactive({
    
    req(nrow(results$wbe_combinedData()) > 0)
    
    range(results$wbe_combinedData()$afschotjaar)
        
  })  

results$types <- reactive({
    
    types <- switch(req(input$wbe_species), 
      "Wild zwijn" = "", 
      "Ree" = c("kits", "geit", "bok"), 
      "Damhert" = "", 
      "Edelhert" = ""
    )
    
    if (length(types) == 1 && types == "")
      return(c("alle" = "all")) else 
      return(types)
    
  })




### The MAP
### -------------

# Data-dependent input fields
output$wbe_year <- renderUI({
    
    div(class = "sliderBlank", 
      sliderInput(inputId = "wbe_year", label = "Geselecteerd Jaar (kaart)",
        min = min(results$wbe_geoData()$afschotjaar),
        max = max(results$wbe_geoData()$afschotjaar),
        value = defaultYear,
        sep = "", step = 1))
    
  })


## Map for Flanders ##

# Create data for map, summary of ecological data, given year, species and regionLevel
results$wbe_summarySpaceData <- reactive({
    
    validate(need(results$wbe_geoData(), "Geen data beschikbaar"),
      need(input$wbe_year, "Gelieve jaar te selecteren"))
    
    
    createSpaceData(
      data = geoData, 
      allSpatialData = spatialData,
      year = input$wbe_year,
      species = input$wbe_species,
      regionLevel = "WBE_binnengrenzen",
      unit = "relative"
    )
    
  })


# Define text to be shown in the pop-ups
results$wbe_textPopup <- reactive({
    
    validate(need(results$wbe_summarySpaceData()$data, "Geen data beschikbaar"))
    
    regionNames <- results$wbe_summarySpaceData()$data$locatie
    titleText <- paste("Gerapporteerd aantal/100ha in", input$wbe_year)
    
    wbeInfo <- subset(wbeData, year == input$wbe_year & WBE_NR %in% regionNames)
    
    textPopup <- paste0("<h4>", regionNames, "</h4>",  
      "<strong>", titleText, "</strong>: ", 
      round(results$wbe_summarySpaceData()$data$freq, 2),
      "</br><strong> Totale oppervlakte (km2) </strong>: ", 
      round(wbeInfo$Area_km2, 2),
      "</br><strong> Percentages (%) </strong></br>",
      paste(sapply(grep("perc", colnames(wbeInfo), value = TRUE), function(x)
          paste(gsub("perc_", "", x), ":", round(wbeInfo[, x]*100, 2))), collapse = "</br>")
    )
    
    
    return(textPopup)
    
  })


# Define colors for the polygons
results$wbe_colorScheme <- reactive({
    
    # Might give warnings if n < 3
    suppressWarnings(c("white", RColorBrewer::brewer.pal(
          n = nlevels(results$wbe_summarySpaceData()$data$group) - 1, name = "YlOrBr")))
    
  })			


# Send map to the UI
output$wbe_spacePlot <- renderLeaflet({
    
    validate(need(results$wbe_spatialData(), "Geen data beschikbaar"),
      need(nrow(results$wbe_summarySpaceData()$data) > 0, "Geen data beschikbaar"))
    
    mapFlanders(
      regionLevel = "WBE_binnengrenzen",
      species = input$wbe_species, 
      year = input$wbe_year,
      allSpatialData = spatialData,
      summaryData = results$wbe_summarySpaceData()$data,
      colorScheme = results$wbe_colorScheme()
    )
    
    
  })


# Plot thick border for selected regions
observe({
    
      validate(need(results$wbe_spatialData(), "Geen data beschikbaar"))
      
      selectedPolygons <- subset(results$wbe_spatialData(), 
        results$wbe_spatialData()$NAAM %in% currentWbe)
      
      coordData <- ggplot2::fortify(selectedPolygons)
      centerView <- c(range(coordData$long), range(coordData$lat))
      
      leafletProxy("wbe_spacePlot", data = results$wbe_spatialData()) %>%
        
        clearGroup(group = "regionLines") %>%
        
        addPolylines(data = selectedPolygons, color = "gray", weight = 5,
          group = "regionLines") %>%
        
        fitBounds(lng1 = centerView[1], lng2 = centerView[2],
          lat1 = centerView[3], lat2 = centerView[4])
        
  })


# Add world map
observe({
    
    validate(need(results$wbe_spatialData(), "Geen data beschikbaar"))
    
    proxy <- leafletProxy("wbe_spacePlot", data = results$wbe_spatialData())
    
    if (!is.null(input$wbe_globe) & !is.null(proxy)){
      
      if (input$wbe_globe %% 2 == 1){
        
        updateActionLink(session, 
          inputId = "wbe_globe", 
          label = "Verberg landkaart")
        
        proxy %>% addProviderTiles("OpenStreetMap.HOT")
        
      } else {
        
        updateActionLink(session, 
          inputId = "wbe_globe", 
          label = "Voeg landkaart toe")
        
        proxy %>% clearTiles()
        
      }
      
    }
    
  })


# Add legend
observe({
    
    validate(need(nrow(results$wbe_summarySpaceData()$data) > 0, "Geen data beschikbaar"))
    
    req(input$wbe_legend)
    
    proxy <- leafletProxy("wbe_spacePlot", data = results$wbe_spatialData())
    proxy %>% removeControl(layerId = "legend")
    
    if (input$wbe_legend != "none") {
      
      palette <- colorFactor(palette = results$wbe_colorScheme(), 
        levels = levels(results$wbe_summarySpaceData()$data$group))
      
      valuesPalette <- results$wbe_summarySpaceData()$data[
        match(results$wbe_spatialData()$NAAM, results$wbe_summarySpaceData()$data$locatie),
        "group"]
      
      
      proxy %>% addLegend(
        position = input$wbe_legend,
        pal = palette, 
        values = valuesPalette,
        opacity = 0.8,
        title = "Legende",
        layerId = "legend"
      )                      
      
    }
    
  })


# Add popups
observe({
    
    validate(need(results$wbe_spatialData(), "Geen data beschikbaar"),
      need(results$wbe_textPopup(), "Geen data beschikbaar"))
    
    currentMap <- leafletProxy("wbe_spacePlot", data = results$wbe_spatialData()) 
    currentMap %>% clearPopups()
    
    event <- input$wbe_spacePlot_shape_click
    
    if (!is.null(event)) {
      
      if (!is.null(event$id)) {
        
        if (event$id %in% results$wbe_summarySpaceData()$data$locatie) {
          
          textSelected <- results$wbe_textPopup()[
            results$wbe_summarySpaceData()$data$locatie == event$id]
          
          isolate({
              
              currentMap %>% 
                addPopups(event$lng, event$lat, popup = textSelected)
              
            }) 
          
        }
        
      }
      
    }
    
  })


# Title for the map
output$wbe_title <- renderUI({
    
    
    h3(paste("Gerapporteerd afschot/100ha voor", tolower(input$wbe_species),
        "in", input$wbe_year))
    
    
  })

# Statistics with map
output$wbe_stats <- renderUI({
    
    h5(paste0("Info beschikbaar en weergegeven voor ", results$wbe_summarySpaceData()$stats$percentage, 
          "% van de totale gegevens (", results$wbe_summarySpaceData()$stats$nAvailable, "/", 
          results$wbe_summarySpaceData()$stats$nTotal, ")" ))
   
  })

# Create final map (for download)
results$wbe_finalMap <- reactive({
    
    validate(need(results$wbe_summarySpaceData()$data, "Geen data beschikbaar"))
    
    
    newMap <- mapFlanders(
      regionLevel = "WBE_binnengrenzen", 
      species = input$wbe_species,
      allSpatialData = spatialData,
      summaryData = results$wbe_summarySpaceData()$data,
      colorScheme = results$wbe_colorScheme(),
      legend = input$wbe_legend,
      addGlobe = input$wbe_globe %% 2 == 1
    )
    
    # save the zoom level and centering to the map object
    newMap %<>% setView(
      lng = input$wbe_spacePlot_center$lng,
      lat = input$wbe_spacePlot_center$lat,
      zoom = input$wbe_spacePlot_zoom
    )
    
    # write map to temp .html file
    htmlwidgets::saveWidget(newMap, file = outTempFileName, selfcontained = FALSE)
    
    # output is path to temp .html file containing map
    outTempFileName
    
    
  }) 

# Download the map
output$wbe_download <- downloadHandler(
  filename = function()
    nameFile(species = input$wbe_species,
      year = input$wbe_year, 
      content = "kaart", fileExt = "png"),
  content = function(file) {
    
    # convert temp .html file into .png for download
    webshot::webshot(url = results$wbe_finalMap(), file = file,
      vwidth = 1000, vheight = 500, cliprect = "viewport")
    
  }
)

output$wbe_downloadData <- downloadHandler(
  filename = function()
    nameFile(species = input$wbe_species,
      year = input$wbe_year, 
      content = "kaartData", fileExt = "csv"),
  content = function(file) {
    
    myData <- results$wbe_summarySpaceData()$data
    # change variable names
    names(myData)[names(myData) == "freq"] <- "aantal/100ha"
    names(myData)[names(myData) == "group"] <- "groep"
    
    ## write data to exported file
    write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
      sep = ";", dec = ",")
    
  })



### Extra Graphs/Tables
### -------------



# Plot1: Line Plot

output$wbe_period <- renderUI({
    
    minYear <- min(results$wbe_geoData()$afschotjaar)
    
    sliderInput(inputId = "wbe_period", label = "Periode (grafiek)", 
      min = minYear,
      max = max(results$wbe_geoData()$afschotjaar),
      value = c(minYear, defaultYear),
      step = 1,
      sep = "")
    
  })


# Create data for map, time plot
results$wbe_timeData <- reactive({
    
    validate(need(results$wbe_geoData(), "Geen data beschikbaar"),
      need(input$wbe_period, "Gelieve periode te selecteren"))
    
    createTrendData(
      data = results$wbe_geoData(),
      allSpatialData = spatialData,
      timeRange = input$wbe_period,
      species = input$wbe_species,
      regionLevel = "WBE_binnengrenzen",
      unit = input$wbe_unit
    )
        
  })

callModule(module = optionsModuleServer, id = "wbe_plot1", 
  data = results$wbe_timeData)
callModule(module = plotModuleServer, id = "wbe_plot1",
  plotFunction = "trendYearRegion", 
  data = results$wbe_timeData,
  locaties = reactive(currentWbe),
  timeRange = reactive(input$wbe_period),
  unit = reactive(input$wbe_unit),
  combinatie = reactive(input$wbe_combinatie)
)



# Table1: Species Table

## User input for controlling the plots and create plotly
# Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
callModule(module = optionsModuleServer, id = "wbe_table1", 
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange
)
callModule(module = plotModuleServer, id = "wbe_table1",
  plotFunction = "tableSpecies", 
  data = results$wbe_combinedData)


# Plot2: Verdeling afschot over de jaren
callModule(module = optionsModuleServer, id = "wbe_plot2", 
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange,
  intervals = c("Per maand", "Per seizoen", "Per twee weken"),
  types = results$types,
  multipleTypes = FALSE)
callModule(module = plotModuleServer, id = "wbe_plot2",
  plotFunction = "countYearShotAnimals", 
  data = results$wbe_combinedData)


# Plot3: Afschot per jachtmethode
callModule(module = optionsModuleServer, id = "wbe_plot3", 
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange)
callModule(module = plotModuleServer, id = "wbe_plot3",
  plotFunction = "countHuntingMethod", 
  data = results$wbe_combinedData)


# Plot4: Schademeldingen
mapSchadeServer(id = "wbe",
  schadeData = schadeData, 
  allSpatialData = spatialData, 
  timeRange = reactive(c(max(2018, results$wbe_timeRange()[1]), results$wbe_timeRange()[2])), 
  defaultYear = defaultYear, 
  species = reactive(input$wbe_species))


# Plot 5: Geslachtsverdeling binnen het afschot per leeftijdscategorie
countAgeGenderServer(id = "wbe",
  data = results$wbe_combinedData,
  timeRange = results$wbe_timeRange)

