# Server file for wildschade summary statistics
# 
# Author: mvarewyck
###############################################################################



### Filter Data
### ---------------

output$schade_subcode <- renderUI({
      
      gewasChoices <- fullNames(
          unique(schadeData@data$schadeCode[schadeData@data$schadeBasisCode == "GEWAS"]))
      voertuigChoices <- fullNames(
          unique(schadeData@data$schadeCode[schadeData@data$schadeBasisCode == "VRTG"]))
      
      
      tagList(
          if ("GEWAS" %in% input$schade_code)
            selectInput(inputId = "schade_gewas", label = "Filter Gewas Schade",
                choices = gewasChoices,
                selected = gewasChoices,
                multiple = TRUE,
                width = "100%"
            ),
          if ("VRTG" %in% input$schade_code)
            selectInput(inputId = "schade_voertuig", label = "Filter Voertuig Schade",
                choices = voertuigChoices,
                selected = voertuigChoices,
                multiple = TRUE,
                width = "100%"
            )
      )
      
    })





# Filter data upon user choices
results$schade_data <- reactive({
      
      # Select species & code & exclude data before 2018
      # TODO: keep 2018 hardcoded?
      toRetain <- schadeData@data$wildsoort %in% req(input$schade_species) &
          schadeData@data$schadeBasisCode %in% req(input$schade_code) &
          schadeData@data$afschotjaar >= 2018
      
      # Filter gewas
      if ("GEWAS" %in% input$schade_code) {
        otherCodes <- input$schade_code[input$schade_code != "GEWAS"]
        toRetain <- toRetain &
            (schadeData@data$schadeBasisCode %in% otherCodes |
              schadeData@data$schadeCode %in% input$schade_gewas)
      }
      
      # Filter voertuig
      if ("VRTG" %in% input$schade_code) {
        otherCodes <- input$schade_code[input$schade_code != "VRTG"]
        toRetain <- toRetain &
            (schadeData@data$schadeBasisCode %in% otherCodes |
              schadeData@data$schadeCode %in% input$schade_voertuig)
      }
      
      schadeData[toRetain, ]
      
    })


# Create frequency tables for filtered data
## wildsoort
callModule(dataModuleServer, id = "wildsoort",
    data = results$schade_data,
    variable = "wildsoort")
## schade
callModule(dataModuleServer, id = "schade",
    data = results$schade_data,
    variable = "schadeBasisCode")
## subschade
callModule(dataModuleServer, id = "subschade",
    data = results$schade_data,
    variable = "schadeCode")


# Show frequency tables for filtered data
output$schade_summary <- renderUI({
      
      fixedRow(
          column(4, tableModuleUI(id = "wildsoort", includeTotal = TRUE)),
          column(4, tableModuleUI(id = "schade", includeTotal = TRUE)),
          if (any(c("GEWAS", "VRTG") %in% input$schade_code)) 
            column(4, tableModuleUI(id = "subschade",
                    includeTotal = TRUE))
      
      )
      
    })


results$schade_timeRange <- reactive({
      
      range(results$schade_data()$afschotjaar)
      
    })  





### Summary map
### ---------------


results$schade_spatialData <- reactive({
      
      req(spatialData)
      
      spatialData[[req(input$schade_regionLevel)]]
      
      
    })

# For which region to show the summary map
output$schade_region <- renderUI({
      
      selectInput(inputId = "schade_region", label = "Regio('s)",
          choices = sort(unique(results$schade_spatialData()$NAAM)),
          selected = NULL, multiple = TRUE)
      
    })



# Data-dependent input fields
output$schade_year <- renderUI({
      
      req(results$schade_data()$afschotjaar)
      
      div(class = "sliderBlank", 
          sliderInput(inputId = "schade_year", label = "Geselecteerd Jaar (kaart)",
              min = min(results$schade_data()$afschotjaar),
              max = max(results$schade_data()$afschotjaar),
              value = defaultYear,
              sep = "", step = 1))
      
    })


output$schade_time <- renderUI({
      
      req(results$schade_data()$afschotjaar)
      
      minYear <- min(results$schade_data()$afschotjaar)
      
      sliderInput(inputId = "schade_time", label = "Periode (grafiek)", 
          value = c(minYear, 
              defaultYear),
          min = minYear,
          max = max(results$schade_data()$afschotjaar),
          step = 1,
          sep = "")
      
    })


# Create data for map, summary of ecological data, given year, species and regionLevel
results$schade_summarySpaceData <- reactive({
      
      validate(need(results$schade_data(), "Geen data beschikbaar"),
          need(input$schade_year, "Gelieve jaar te selecteren"))
      
      
      createSpaceData(
          data = results$schade_data()@data, 
          allSpatialData = spatialData,
          year = input$schade_year,
          species = input$schade_species,
          regionLevel = input$schade_regionLevel,
          unit = input$schade_unit,
          sourceIndicator = input$schade_bron1
      )
      
    })


# Which region(s) are selected?
observe({
      
      event <- input$schade_spacePlot_shape_click
      
      if (!is.null(event)) {
        
        if (!is.null(event$id)) {
          
          currentSelected <- isolate(input$schade_region)
          
          # Remove from list
          if (event$id %in% currentSelected) {
            
            updateSelectInput(session, "schade_region", 
                selected = currentSelected[ - which(currentSelected == event$id)])
            
            # Add to list
          } else {
            
            updateSelectInput(session, "schade_region", 
                selected = c(currentSelected, event$id))
            
          }
          
        }
        
      }
      
    })


# Define text to be shown in the pop-ups
results$schade_textPopup <- reactive({
      
      validate(need(results$schade_summarySpaceData()$data, "Geen data beschikbaar"))
      
      regionNames <- results$schade_summarySpaceData()$data$locatie
      titleText <- paste("Gerapporteerd aantal schadegevallen", 
          "in", input$schade_year)
      
      textPopup <- paste0("<h4>", regionNames, "</h4>",  
          "<strong>", titleText, "</strong>: ", 
          round(results$schade_summarySpaceData()$data$freq, 2)
      )
      
      
      return(textPopup)
      
    })


# Define colors for the polygons
results$schade_colorScheme <- reactive({
      
      # Might give warnings if n < 3
      suppressWarnings(c("white", RColorBrewer::brewer.pal(
                  n = nlevels(results$schade_summarySpaceData()$data$group) - 1, name = "YlOrBr")))
      
    })			


# Send map to the UI
output$schade_spacePlot <- renderLeaflet({
      
      req(spatialData)
      
      validate(need(results$schade_spatialData(), "Geen data beschikbaar"),
          need(nrow(results$schade_summarySpaceData()$data) > 0, "Geen data beschikbaar"))
      
      mapFlanders(
          regionLevel = input$schade_regionLevel,
          species = input$schade_species, 
          allSpatialData = spatialData,
          summaryData = results$schade_summarySpaceData()$data,
          colorScheme = results$schade_colorScheme()
      )
      
    })


# Plot thick border for selected regions
observe({
      
      if (!is.null(input$schade_region)) {
        
        req(results$schade_spatialData())
        
        selectedPolygons <- subset(results$schade_spatialData(), 
            results$schade_spatialData()$NAAM %in% input$schade_region)
        
        leafletProxy("schade_spacePlot", data = results$schade_spatialData()) %>%
            
            clearGroup(group = "regionLines") %>%
            
            addPolylines(data = selectedPolygons, color = "gray", weight = 5,
                group = "regionLines")
        
      }
      
    })


# Add world map
observe({
      
      req(results$schade_spatialData())
      
      proxy <- leafletProxy("schade_spacePlot", data = results$schade_spatialData())
      
      if (!is.null(input$schade_globe) & !is.null(proxy)){
        
        if (input$schade_globe %% 2 == 1){
          
          updateActionLink(session, 
              inputId = "schade_globe", 
              label = "Verberg landkaart")
          
          proxy %>% addProviderTiles("OpenStreetMap.HOT")
          
        } else {
          
          updateActionLink(session, 
              inputId = "schade_globe", 
              label = "Voeg landkaart toe")
          
          proxy %>% clearTiles()
          
        }
        
      }
      
    })


# Add legend
observe({
      
      req(nrow(results$schade_summarySpaceData()$data) > 0)
      
      req(input$schade_legend)
      
      proxy <- leafletProxy("schade_spacePlot", data = results$schade_spatialData())
      proxy %>% removeControl(layerId = "legend")
      
      if (input$schade_legend != "none") {
        
        palette <- colorFactor(palette = results$schade_colorScheme(), 
            levels = levels(results$schade_summarySpaceData()$data$group))
        
        valuesPalette <- results$schade_summarySpaceData()$data[
            match(results$schade_spatialData()$NAAM, results$schade_summarySpaceData()$data$locatie),
            "group"]
        
        
        proxy %>% addLegend(
            position = input$schade_legend,
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
      
      validate(need(results$schade_spatialData(), "Geen data beschikbaar"),
          need(results$schade_textPopup(), "Geen data beschikbaar"))
      
      currentMap <- leafletProxy("schade_spacePlot", data = results$schade_spatialData()) 
      currentMap %>% clearPopups()
      
      event <- input$schade_spacePlot_shape_click
      
      if (!is.null(event)) {
        
        if (!is.null(event$id)) {
          
          if (event$id %in% results$schade_summarySpaceData()$data$locatie) {
            
            textSelected <- results$schade_textPopup()[
                results$schade_summarySpaceData()$data$locatie == event$id]
            
            isolate({
                  
                  currentMap %>% 
                      addPopups(event$lng, event$lat, popup = textSelected)
                  
                }) 
            
          }
          
        }
        
      }
      
    })



# Title for the map
output$schade_title <- renderUI({
      
      n_lk1 <- length(input$schade_species) 
      h3(paste("Schademeldingen", 
              "voor", if (n_lk1 > 1) paste(paste(tolower(input$schade_species)[1:n_lk1-1], collapse = ", "), "en", tolower(input$schade_species[n_lk1])) else tolower(input$schade_species),
              "in", input$schade_year))
      
      
    })

# Create final map (for download)
results$schade_finalMap <- reactive({
      
      validate(need(results$schade_summarySpaceData()$data, "Geen data beschikbaar"))
      
      
      newMap <- mapFlanders(
          regionLevel = input$schade_regionLevel, 
          species = input$schade_species,
          allSpatialData = spatialData,
          summaryData = results$schade_summarySpaceData()$data,
          colorScheme = results$schade_colorScheme(),
          legend = input$schade_legend,
          addGlobe = input$schade_globe %% 2 == 1
      )
      
      # save the zoom level and centering
      newMap %>%  setView(
          lng = input$schade_spacePlot_center$lng,
          lat = input$schade_spacePlot_center$lat,
          zoom = input$schade_spacePlot_zoom
      )
      
      
    }) 


# Download the map
output$schade_downloadMap <- downloadHandler(
    filename = function()
      nameFile(species = input$schade_species,
          year = input$schade_year, 
          content = "kaartSchade", fileExt = "png"),
    content = function(file) {
      
      mapview::mapshot(x = results$schade_finalMap(), file = file,
          vwidth = 1000, vheight = 500, cliprect = "viewport")
      
    }
)

output$schade_downloadData <- downloadHandler(
    filename = function()
      nameFile(species = input$schade_species,
          year = input$schade_year, 
          content = "kaartData", fileExt = "csv"),
    content = function(file) {
      
      myData <- results$schade_summarySpaceData()$data
      # change variable names
      names(myData)[names(myData) == "freq"] <- "aantal schadegevallen"
      names(myData)[names(myData) == "group"] <- "groep"
      
      ## write data to exported file
      write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
          sep = ";", dec = ",")
      
    })




## Time line plots per region
## -----------------


## Time plot for Flanders (reference) ##

results$schade_timeDataFlanders <- reactive({
      
      validate(need(input$schade_time, "Gelieve periode te selecteren"))
      
      ## Get data for Flanders
      createTrendData(
          data = results$schade_data()@data,
          allSpatialData = spatialData,
          timeRange = input$schade_time,
          species = input$schade_species,
          regionLevel = "flanders",
          unit = input$schade_unit,
          sourceIndicator = input$schade_bron1
      )
      
    })

callModule(module = optionsModuleServer, 
    id = "schade_timePlotFlanders", 
    data = results$schade_timeDataFlanders)
callModule(module = plotModuleServer, id = "schade_timePlotFlanders",
    plotFunction = "trendYearFlanders", 
    data = results$schade_timeDataFlanders,
    timeRange = reactive(input$schade_time),
    unit = reactive(input$schade_unit),
    schadeTitles = TRUE)




## Time plot for selected region ##

# Create data for map, time plot
results$schade_timeData <- reactive({
      
      validate(need(results$schade_data(), "Geen data beschikbaar"),
          need(input$schade_time, "Gelieve periode te selecteren"))
      
      
      createTrendData(
          data = results$schade_data()@data,
          allSpatialData = spatialData,
          timeRange = input$schade_time,
          species = input$schade_species,
          regionLevel = input$schade_regionLevel,
          unit = input$schade_unit,
          sourceIndicator = input$schade_bron1
      )
      
    })

# Title for selected region level
output$schade_timeTitle <- renderUI({
      
      regionLevel <- switch(input$schade_regionLevel,
          "flanders" = "Vlaanderen",
          "provinces" = "Provincie",
          "faunabeheerzones" = "Faunabeheerzones",
          "communes" = "Gemeente (binnen provincie)",
          "fbz_gemeentes" = "Gemeente (binnen faunabeheerzone)",
          "utm5" = "5x5 UTM")
      
      
      h3("Evolutie schademeldingen", regionLevel)
      
    })


callModule(module = optionsModuleServer, id = "schade_timePlot", 
    data = results$schade_timeData)
callModule(module = plotModuleServer, id = "schade_timePlot",
    plotFunction = "trendYearRegion", 
    data = results$schade_timeData,
    locaties = reactive(input$schade_region),
    timeRange = reactive(input$schade_time),
    unit = reactive(input$schade_unit),
    schadeTitles = TRUE,
    combinatie = reactive(FALSE))



## Perceel map
## -----------------


# Data-dependent input fields
output$schade_time2 <- renderUI({
      
      sliderInput(inputId = "schade_time2", label = "Periode", 
          value = c(results$schade_timeRange()[1], defaultYear),
          min = results$schade_timeRange()[1],
          max = results$schade_timeRange()[2],
          step = 1,
          sep = "")
      
    })


output$schade_titlePerceel <- renderUI({
      
      n_lk2 <- length(input$schade_species)      
      
      h3(paste("Schademeldingen", 
              "voor", if (n_lk2 > 1) paste(paste(tolower(input$schade_species)[1:n_lk2-1], collapse = ", "), "en", tolower(input$schade_species[n_lk2])) else tolower(input$schade_species),
              "per", switch(input$schade_variable2, 
                  season = "seizoen",
                  schadeCode = "schadetype"),
              #jaartallen
              ifelse(input$schade_time2[1] != input$schade_time2[2],
                  paste0("(", input$schade_time2[1], " tot ", input$schade_time2[2], ")"),
                  paste0("(", input$schade_time2[1], ")")
              )#,
          
#                            paste0("(", 
#                                    input$schade_time2[1], 
#                                    " tot ", 
#                                    input$schade_time2[2],
#                                    ")"
#                            )
          ))
      
    })

# Create data for map, summary of schade data, given year
results$schade_summaryPerceelData <- reactive({
      
      validate(need(results$schade_data(), "Geen data beschikbaar"),
          need(input$schade_time2, "Gelieve periode te selecteren"))
      
      createSchadeSummaryData(
          schadeData = results$schade_data(),
          timeRange = input$schade_time2,
          sourceIndicator = input$schade_bron2)
    })

# Map for UI
output$schade_perceelPlot <- renderLeaflet({
      
      validate(need(results$schade_spatialData(), "Geen data beschikbaar"),
          need(nrow(results$schade_summaryPerceelData()@data) > 0, "Geen data beschikbaar"),
          need(input$schade_time2, "Gelieve periode te selecteren"))
      
      
      
      mapSchade(
          schadeData = results$schade_summaryPerceelData(),
          regionLevel = "provinces",
          variable = input$schade_variable2,
          allSpatialData = spatialData,
          addGlobe = input$schade_globe2 %% 2 == 0, 
          legend = input$schade_legend2)
      
    })

# Create final perceelplot map (for download)
results$schade_perceelMap <- reactive({
      
      validate(need(results$schade_summaryPerceelData(), "Geen data beschikbaar"))
      
      
      newPerceelMap <- mapSchade(
          schadeData = results$schade_summaryPerceelData(),
          regionLevel = "provinces", 
          variable = input$schade_variable2,
          allSpatialData = spatialData,
          legend = input$schade_legend2,
          addGlobe = input$schade_globe2 %% 2 == 0
      )
      
      # save the zoom level and centering
      newPerceelMap %>%  setView(
          lng = input$schade_perceelPlot_center$lng,
          lat = input$schade_perceelPlot_center$lat,
          zoom = input$schade_perceelPlot_zoom
      )
      
      
    })

observeEvent(input$schade_globe2, {
      
      if(input$schade_globe2 %% 2 == 0) {
        
        updateActionLink(session, 
            inputId = "schade_globe2",
            label = "Verberg landkaart")
        
      } else {
        
        updateActionLink(session, 
            inputId = "schade_globe2",
            label = "Voeg landkaart toe")
        
      }
      
    })

# Generating image outside of downloadHandler
map <- reactiveVal()
observeEvent(input$schade_genereerMap, {
      map(NULL)
      idNote <- showNotification("Aanvraag wordt verwerkt... Even geduld.", type = "message", duration = NULL)
      
      file <- tempfile(fileext = ".png")
      map(file)
      
      mapview::mapshot(x = results$schade_perceelMap(), file = file,
          vwidth = 1000, vheight = 500, cliprect = "viewport")
      
      removeNotification(id = idNote)
      
      session$sendCustomMessage(type = "imageReady", 
          message = list(id = "schade_downloadPerceelMap"))
    })

# Download the perceeplot map
output$schade_downloadPerceelMap <- downloadHandler(
    filename = function()
      nameFile(species = input$schade_species,
          year = unique(input$schade_time2), 
          content = switch(input$schade_variable2, 
              season = "kaartSchadeSeizoen", 
              schadeCode = "kaartSchadeTypeSchade"), 
          fileExt = "png"),
    content = function(file) {
      
      file.copy(map(), file, overwrite = TRUE)
    }
)

# Download the minimal perceeplot map data
output$schade_downloadPerceelmapData <- downloadHandler(
    filename = function()
      nameFile(species = input$schade_species,
          year = unique(input$schade_time2), 
          content = "kaartDataPerVariabele", 
          fileExt = "csv"),
    content = function(file) {
      
      myPerceelplotData <- formatSchadeSummaryData(results$schade_summaryPerceelData())
      
      ## write data to exported file
      write.table(x = myPerceelplotData, file = file, quote = FALSE, row.names = FALSE,
          sep = ";", dec = ",")
      
    })

### Descriptive Plots
### -----------------

# Plot 1: Gerapporteerd aantal schadegevallen per jaar en per regio
callModule(module = optionsModuleServer, id = "schade_plot1", 
    data = reactive(results$schade_data()@data),
    types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
        )), 
    labelTypes = "Regio", 
    typesDefault = reactive("provinces"), 
    timeRange = results$schade_timeRange)

callModule(module = plotModuleServer, id = "schade_plot1",
    plotFunction = "countYearProvince", 
    data = reactive(results$schade_data()@data))


# Plot 2: Gerapporteerd aantal schadegevallen per jaar en variabele
callModule(module = optionsModuleServer, id = "schade_plot2", 
    data = reactive(results$schade_data()@data),
    types = reactive(c(
            "Wildsoort" = "wildsoort",
            "Gewas" = "SoortNaam", 
            "Type Schade" = "schadeCode"
        )), 
    labelTypes = "Variabele", 
    typesDefault = reactive("wildsoort"), 
    timeRange = results$schade_timeRange)

callModule(module = plotModuleServer, id = "schade_plot2",
    plotFunction = "countYearSchade", 
    data = reactive(results$schade_data()@data))


# Table Frequency table schadeCode
callModule(module = optionsModuleServer, id = "schade_table2", 
    data = reactive(results$schade_data()@data),
    types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
        )), 
    labelTypes = "Regio", 
    typesDefault = reactive("provinces"), 
    timeRange = results$schade_timeRange)
callModule(module = plotModuleServer, id = "schade_table2",
    plotFunction = "tableSchadeCode", 
    data = reactive(results$schade_data()@data),
    schadeChoices = reactive(input$schade_code),
    schadeChoicesVrtg = reactive(input$schade_voertuig),
    schadeChoicesGewas = reactive(input$schade_gewas),
    schade = TRUE,
    datatable = TRUE)

# Table Frequency table gewas
callModule(module = optionsModuleServer, id = "schade_gewas",
    data = reactive(results$schade_data()@data),
    types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
        )), 
    labelTypes = "Regio", 
    typesDefault = reactive("provinces"),
    timeRange = results$schade_timeRange)

#callModule(dataModuleServer, id = "gewas",
#    data = results$schade_data,
#    variable = "SoortNaam")

callModule(plotModuleServer, id = "schade_gewas",
    plotFunction = "tableGewas",
    data = reactive(results$schade_data()@data),
    variable = "SoortNaam")


