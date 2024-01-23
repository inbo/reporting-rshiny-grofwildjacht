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
      toRetain <- schadeData$wildsoort %in% req(input$schade_species) &
          schadeData$schadeBasisCode %in% req(input$schade_code) &
          schadeData$afschotjaar >= 2018
      
      # Filter gewas
      if ("GEWAS" %in% input$schade_code) {
        otherCodes <- input$schade_code[input$schade_code != "GEWAS"]
        toRetain <- toRetain &
            (schadeData$schadeBasisCode %in% otherCodes |
              schadeData$schadeCode %in% input$schade_gewas)
      }
      
      # Filter voertuig
      if ("VRTG" %in% input$schade_code) {
        otherCodes <- input$schade_code[input$schade_code != "VRTG"]
        toRetain <- toRetain &
            (schadeData$schadeBasisCode %in% otherCodes |
              schadeData$schadeCode %in% input$schade_voertuig)
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
    
    req(input$schade_species)
    
    tagList(
      h2("Schadegevallen"),
      fixedRow(
        column(4, tableModuleUI(id = "wildsoort", includeTotal = TRUE)),
        column(4, tableModuleUI(id = "schade", includeTotal = TRUE)),
        if (any(c("GEWAS", "VRTG") %in% input$schade_code)) 
          column(4, tableModuleUI(id = "subschade",
              includeTotal = TRUE))
      
      )
    )
    
  })


results$schade_timeRange <- reactive({
      
      range(results$schade_data()$afschotjaar)
      
    }) 
  
  
observe({
      
    if (is.null(input$schade_species) || is.null(input$schade_code))
      shinyjs::hide(id = "schade_results") else
      shinyjs::toggle(id = "schade_results", 
        condition = nrow(results$schade_data()) > 0) 
      
  })  





### Summary map
### ---------------

mapFlandersServer(id = "schade",
  uiText = uiText,
  defaultYear = defaultYear,
  type = "wildschade",
  species = reactive(req(input$schade_species)),
  geoData = reactive({
      tmpData <- sf::st_drop_geometry(results$schade_data())
      tmpData$dataSource <- tmpData$indieningType
      tmpData$indieningType <- NULL
      tmpData
    }),
  allSpatialData = spatialData)





## Perceel map
## -----------------

mapSchadeServer(id = "schade", 
  schadeData = results$schade_data,
  allSpatialData = reactive(spatialData),
  timeRange = results$schade_timeRange,
  defaultYear = defaultYear,
  species = reactive(input$schade_species),
  borderRegion = "provinces"
)



### Descriptive Plots
### -----------------

# Plot 1: Gerapporteerd aantal schadegevallen per jaar en per regio
countYearProvinceServer(id = "schade",
    data = reactive(sf::st_drop_geometry(results$schade_data())),
    types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
        )), 
    labelTypes = "Regio", 
    typesDefault = reactive("provinces"), 
    timeRange = results$schade_timeRange,
    title = reactive({
        input$tabs   # ensure title is updated
        title <- uiText$title[uiText$plotFunction == "countYearProvinceUI"]
        gsub("Gerapporteerd aantal", "Aantal schadegevallen", title)
      })
  )


# Plot 2: Gerapporteerd aantal schadegevallen per jaar en variabele
countYearSchadeServer(
    id = "schade",
    data = reactive(sf::st_drop_geometry(results$schade_data())),
    types = reactive(c(
            "Wildsoort" = "wildsoort",
            "Gewas" = "SoortNaam", 
            "Type Schade" = "schadeCode"
        )), 
    labelTypes = "Variabele", 
    typesDefault = reactive("wildsoort"), 
    timeRange = results$schade_timeRange,
    fullNames = fullNames)


# Table Frequency table schadeCode
tableSchadeServer(
    id = "schade",  
    data = reactive(sf::st_drop_geometry(results$schade_data())),
    types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
        )), 
    labelTypes = "Regio", 
    typesDefault = reactive("provinces"), 
    timeRange = results$schade_timeRange,
    schadeChoices = reactive(input$schade_code),
    schadeChoicesVrtg = reactive(input$schade_voertuig),
    schadeChoicesGewas = reactive(input$schade_gewas),
    datatable = TRUE,
    fullNames = fullNames)

# Table Frequency table gewas
tableGewasServer(
    id = "schade",
    data = reactive(sf::st_drop_geometry(results$schade_data())),
    types = reactive(c(
            "Vlaanderen" = "flanders",
            "Provincie" = "provinces", 
            "Faunabeheerzones" = "faunabeheerzones"
        )), 
    labelTypes = "Regio", 
    typesDefault = reactive("provinces"),
    timeRange = results$schade_timeRange,
    variable = "SoortNaam")


