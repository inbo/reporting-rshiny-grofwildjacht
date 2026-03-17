#' Create map for disperser wolves
#' @param data data.frame main data
#' @param popup_vars variables to be shown in circle popup
#' @param spatialData data.frame spatial data
#' @inheritParams mapFlanders
#' @return leaflet map
#' @author sjunius
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles addPolygons setView addLegend
#' @importFrom RColorBrewer brewer.pal
#' @importFrom INBOtheme inbo_palette
#' @export
mapDispersersWolves <- function(
  data,
  popup_vars,
  spatialData,
  addGlobe = FALSE,
  showTerritoria = TRUE,
  legend = "topright"
) {
  # Color palette for Years
  nColors <- length(unique(data$year))
  colors <- if (nColors < 10) {
    inbo_palette(n = nColors)
  } else {
    paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
    unlist(
      sapply(
        paletteNames,
        function(x) {
          suppressWarnings(brewer.pal(n = 12, name = x))
        }
      )
    )[1:nColors]
  }
  year_colors <- colorFactor(colors, sort(unique(data$year)))

  # Color palette for territories
  territory_names <- sort(unique(spatialData$Territory))
  territory_palette <- colorFactor(
    palette = brewer.pal(length(territory_names), "Set2"),
    domain = territory_names
  )

  myMap <- leaflet(
    data,
    options = leafletOptions(maxZoom = 12)
  ) |>
    addMapPane("polylines", zIndex = 200)

  if (showTerritoria) {
    myMap <- myMap |>
      addPolygons(
        data = spatialData,
        fillColor = ~ territory_palette(Territory),
        color = "grey40",
        weight = 0.8,
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        label = ~Territory,
        highlightOptions = highlightOptions(
          weight = 1.5,
          color = "black"
        ),
        group = "Territoria"
      )
  }

  myMap <- myMap |>
    addCircleMarkers(
      radius = 6,
      fillColor = ~ year_colors(year),
      stroke = TRUE,
      color = "black",
      weight = 0.5,
      fillOpacity = 1,
      popup = construct_popup(data, popup_vars),
      group = "Zwervers"
    ) |>
    setView(lng = 4, lat = 51, zoom = 8)

  # Add world map
  if (addGlobe) {
    myMap <- myMap |>
      addProviderTiles("OpenStreetMap.HOT")
  }

  # Add legend
  if (legend != "none") {
    myMap <- myMap |>
      addLegend(
        legend,
        pal = year_colors,
        values = ~year,
        title = "Zwervers",
        opacity = 1,
        labFormat = labelFormat(),
        group = "Zwervers",
        layerId = "legend1"
      )

    if (showTerritoria) {
      myMap <- myMap |>
        addLegend(
          legend,
          pal = territory_palette,
          values = spatialData$Territory,
          title = "Territoria",
          opacity = 1,
          labFormat = labelFormat(),
          group = "Territoria",
          layerId = "legend2"
        )
    }
  }

  myMap
}


#' Shiny module for creating map on disperser wolves - server side
#' 
#' @param data data.frame main data
#' @param variable character with column name of interest
#' @inheritParams getOutputTitle
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @author sjunius
#' @import shiny
#' @importFrom leaflet renderLeaflet setView leafletProxy clearTiles leafletOutput
#' @export
mapDispersersWolvesServer <- function(
  id,
  data,
  variable = "Lot",
  popup_variables = NULL,
  allSpatialData = NULL,
  preSelected = reactive(NULL)
) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      results <- reactiveValues()
      
      output$time_wolf <- renderUI({
          req(data())
          
          timeRange <- range(data()$year)
          sliderInput(inputId = ns("time"), label = "Periode", 
            value = c(timeRange[1], timeRange[2]),
            min = timeRange[1],
            max = timeRange[2],
            step = 1,
            sep = "")
          
        })
      
      # Selected regions of interest
      spatialData <- reactive({
          
          req(allSpatialData)
          req(preSelected())
          req(preSelected()$regionLevel())
          
          filterSpatial(
            allSpatialData = allSpatialData, 
            species = "Wolf", 
            regionLevel = preSelected()$regionLevel(), 
            year = NULL
          )
          
        })
      
      selectedPolygons <- reactive({
          
          validate(need(spatialData(), "Geen data beschikbaar"))
#          validate(need(preSelected()$region(), "Gelieve regio('s) te selecteren"))
          
          subset(spatialData(), spatialData()$NAAM %in% preSelected()$region())
          
        })
      
      subData <- reactive({
          
          req(data())
          req(input$time)
          
          years <- input$time[1]:input$time[2]
          
          data <- data() %>%
            filter(Status == "Zwerver") %>%
            filter(!(Last_known_location %in% c("Namen", "Hoge Venen"))) %>%
            filter(!is.na(Y_Coord)) %>%
            filter(year %in% years)
          
          data
          
        })
      
      spatialSpecificData <- reactive({
          loadWolfShapeData(type = "territory")
        })
      
      spacePlot <- reactive({
          req(subData())
          validate(need(nrow(subData()) > 0, "Er is geen data aanwezig voor de geselecteerde filters. Gelieve een andere selectie te maken."))

          if(is.null(popup_variables)){
            popup_variables <- c(variable)
          }

          myMap <- mapDispersersWolves(data = subData(), popup_vars = popup_variables, spatialData = spatialSpecificData(), addGlobe = TRUE,
            legend = isolate(input$legend), showTerritoria = input$showTerritoria)
          
          if (!is.null(spatialData()))
            myMap <- myMap %>%
              addPolygons(data = spatialData(), color = "gray", weight = 2,
                group = "regionLinesAll", fillOpacity = 0, layerId = spatialData()$NAAM,
                options = pathOptions(pane = "polylines")) %>%
              addPolylines(data = selectedPolygons(), color = "black", weight = 2,
                group = "regionLines",
                options = pathOptions(pane = "polylines"))
          
          myMap
          
        })
      
      
      output$spacePlot <- renderLeaflet({
          
          spacePlot() %>%
            leaflet.extras2::addEasyprint(   # use leaflets personal functionality to download maps
              options = leaflet.extras2::easyprintOptions(
                exportOnly = TRUE,
                hideControlContainer = FALSE,  # Keep controls visible
              )
            )
          
        })
      
      output$spacePlotUI <- renderUI({
          
          tryCatch({
              leafletOutput(ns("spacePlot"))
            },
            error = function(e) {
              return(NULL) 
            })
          
        })
      
      
      # Center view
      observe({
          
          # Update after plot
          req(spacePlot())
          
          leafletProxy("spacePlot", data = subData()) %>%
            setView(lng = 4, lat = 51, zoom = 8)
          
        })
      
      # Add world map
      observe({
          req(subData())
          proxy <- leafletProxy("spacePlot", data = subData())
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == 0){
              
              updateActionLink(session, inputId = "globe", 
                label = "Verberg landkaart")
              
              proxy %>% addProviderTiles("OpenStreetMap.HOT")
              
            } else {
              
              updateActionLink(session, inputId = "globe", 
                label = "Voeg landkaart toe")
              
              proxy %>% clearTiles()
              
            }
            
          }
          
        })
      
      # Add legend
      observe({
          
          req(input$legend)
          req(subData())
          validate(need(nrow(subData()) > 0, "Er is geen data aanwezig voor de geselecteerde filters. Gelieve een andere selectie te maken."))
          
          req(spatialSpecificData())
          
          proxy <- leafletProxy("spacePlot", data = subData())
          req(!is.null(proxy))
          
          proxy %>% removeControl(layerId = "legend1") %>% removeControl(layerId = "legend2")
          
          if (input$legend != "none") {
            nColors <- length(unique(subData()$year))
            colors <- if (nColors < 10) {
                inbo_palette(n = nColors) 
              } else {
                paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
                unlist(sapply(paletteNames, function(x)
                      suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
              }
            year_colors <- colorFactor(colors, sort(unique(subData()$year)))
            
            # Color palette for territories
            territory_names <- sort(unique(spatialSpecificData()$Territory))
            territory_palette <- colorFactor(palette = brewer.pal(length(territory_names), "Set2"), domain = territory_names)
            
            proxy %>% addLegend(
                position = input$legend,
                pal = year_colors,
                values = ~year,
                title = "Zwervers",
                opacity = 1,
                labFormat = labelFormat(),
                group = "Zwervers",
                layerId = "legend1") 
              
              if (isolate(input$showTerritoria))
                proxy %>% addLegend(
                  position = input$legend,
                  pal = territory_palette,
                  values = spatialSpecificData()$Territory,
                  title = "Territoria",
                  opacity = 1,
                  labFormat = labelFormat(),
                  group = "Territoria",
                  layerId = "legend2")
              
            }
          
        })
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapDispersersWolves(data = subData(), spatialData = spatialSpecificData(), 
            addGlobe = input$globe %% 2 == 0, legend = input$legend, showTerritoria = input$showTerritoria)
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spacePlot_center$lng,
            lat = input$spacePlot_center$lat,
            zoom = input$spacePlot_zoom
          )
          
          if (!is.null(spatialData()))
            newMap <- newMap %>%
              addPolygons(data = spatialData(), color = "gray", weight = 2,
                group = "regionLinesAll", fillOpacity = 0, layerId = spatialData()$NAAM,
                options = pathOptions(pane = "polylines")) %>%
              addPolylines(data = selectedPolygons(), color = "black", weight = 2,
                group = "regionLines",
                options = pathOptions(pane = "polylines"))
          
          return(newMap)
          
        }) 
      
      
      # Download the map
      observeEvent(input$download, {
          
          idNote <- showNotification("Aanvraag wordt verwerkt... Even geduld.", type = "message", duration = NULL)
          
          leafletProxy("spacePlot") %>% leaflet.extras2::easyprintMap(
            sizeModes = "CurrentSize",
            filename = nameFile(
              species = "Wolf_dispersers",
              content = "kaart"
            )
          )
          
          removeNotification(id = idNote)
          
        })
      
      # Download data
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = "Wolf",
            content = "kaartData", fileExt = "csv"),
        content = function(file) {

          # Coordinate information may not be downloaded
          columns_to_remove <- intersect(
            c("geometry", "X_Coord", "Y_Coord"),
            colnames(subData())
          )
          data_to_download <- subData() |>
            as.data.frame() |>
            dplyr::select(!columns_to_remove)
          
          ## write data to exported file
          write.table(x = data_to_download, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ".")
          
        })
      
      # Which region(s) are selected?
      observe({
          
          event <- input$spacePlot_shape_click
          
          if (!is.null(event) && !is.null(event$id)) {
            currentSelected <- isolate(preSelected()$region())
            
            if (event$id %in% currentSelected) {
              # Remove from list
              updateSelectInput(session, inputId = "region", 
                selected = currentSelected[- which(currentSelected == event$id)])
              
              results$selectedRegions <- currentSelected[- which(currentSelected == event$id)]
              
            } else {
              # Add to list
              updateSelectInput(session, inputId = "region", 
                selected = c(currentSelected, event$id))
              
              results$selectedRegions <- c(currentSelected, event$id)
              
            }
            
          }
          
        })
      
      return(reactive({
            # Update when any of these change
            results$selectedRegions
            # Return the static values
            c(
              selectedRegions = reactive(na.omit(results$selectedRegions))
            )
          }))
      
    })
  
}



#' Shiny module for creating map on disperser wolves - UI side
#' 
#' @inherit welcomeSectionUI
#' @inheritParams reportingGrofwild-common-args
#' @author sjunius
#' @import shiny
#' @export
mapDispersersWolvesUI <- function(
  id, uiText, plotFunction = "mapDispersersWolvesUI", context = "description",
  doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(  
    
    actionLink(inputId = ns("linkMapDispersersWolves"),
      label = h3(HTML(title))),
    conditionalPanel(paste("input.linkMapDispersersWolves % 2  ==", as.numeric(doHide)), ns = ns,
      
      wellPanel(
        fixedRow(
          column(4, uiOutput(ns("time_wolf"))),
          column(4, selectInput(inputId = ns("legend"), label = "Legende",
              choices = c(
                "Bovenaan rechts" = "topright",
                "Onderaan rechts" = "bottomright",
                "Bovenaan links" = "topleft",
                "Onderaan links" = "bottomleft",
                "<geen>" = "none"))           
          ),
          column(4, checkboxInput(inputId = ns("showTerritoria"), label = "Toon huidige territoria", value = TRUE))
        ),
        actionLink(inputId = ns("globe"), label = "Verberg landkaart",
          icon = icon("globe"))
      ),
      
      fixedRow(
        column(12,
          withSpinner(uiOutput(ns("spacePlotUI"))),
          tags$br(),
          actionButton(ns("download"), label = "Download figuur", class = "downloadButton"),
          downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton")
        )
      ),
      
      tags$br(),
      tags$div(class = "larger-description", HTML(description)),
      tags$hr()
    
    )
  )
  
  
}
