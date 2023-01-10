# Server file for wildschade summary statistics
# 
# Author: mvarewyck
###############################################################################



### Filter Data
### ---------------

output$schade_subcode <- renderUI({
      
      gewasChoices <- metaSchade$codes[["GEWAS"]]
      voertuigChoices <- metaSchade$codes[["VRTG"]]
      
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
  
  output$schade_warning <- renderUI({
      
      validate(need(input$schade_species, "Gelieve wildsoort(en) te selecteren"),
          need(input$schade_code, "Gelieve type(s) schade te selecteren"))
      
    })

  observe({
      
      shinyjs::toggle(id = "schade_results", 
        condition = !is.null(input$schade_species) && !is.null(input$schade_code)) 
      
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
    variable = "schadeBasisCode",
    fullNames = schadeTypes)
## subschade
callModule(dataModuleServer, id = "subschade",
    data = results$schade_data,
    variable = "schadeCode",
    fullNames = schadeCodes)


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

mapFlandersServer(id = "schade",
  defaultYear = defaultYear,
  type = "wildschade",
  species = reactive(input$schade_species),
  geoData = reactive(results$schade_data()@data),
  allSpatialData = spatialData)





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
          sourceIndicator = input$schade_bron2,
          fullNames = fullNames
        )
    })

# Map for UI
output$schade_perceelPlot <- renderLeaflet({
      
      validate(need(spatialData, "Geen data beschikbaar"),
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
    data = reactive(results$schade_data()@data),
    fullNames = fullNames)


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
    datatable = TRUE,
    fullNames = fullNames)

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


