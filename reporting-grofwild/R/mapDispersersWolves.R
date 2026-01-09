#' Create map for raffic accidents involving wolves
#' @inheritParams mapFlanders 
#' @return leaflet map
#' @author sjui
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles fitBounds
#' @importFrom RColorBrewer brewer.pal
#' @importFrom INBOtheme inbo_palette
#' @export
mapDispersersWolves <- function(
  data, 
  spatialData,
  addGlobe = FALSE,
  legend = "topright"
) {
  
  # Color palette for Years
  nColors <- length(unique(data$year))
  colors <- if (nColors < 10) {
      inbo_palette(n = nColors) 
    } else {
      paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
      unlist(sapply(paletteNames, function(x)
            suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
    }
  year_colors <- colorFactor(colors, sort(unique(data$year)))
  
  # Color palette for territories
  territory_names <- sort(unique(spatialData$Territory))
  territory_palette <- colorFactor(palette = brewer.pal(length(territory_names), "Set2"), domain = territory_names)
  
  
  myMap <- leaflet(data) %>%
    
    addPolygons(data = spatialData,
      fillColor = ~territory_palette(Territory),
      color = "grey40", weight = 0.8,
      fillOpacity = 0.8,
      smoothFactor = 0.5,
      label = ~Territory,
      highlightOptions = highlightOptions(
        weight = 1.5, 
        color = "black"),
      group = "Territoria") %>%
    addCircleMarkers(
      radius = 6,
      fillColor = ~year_colors(year),
      stroke = TRUE, color = "black", weight = 0.5, 
      fillOpacity = 1,
      popup = paste0("<li><strong> Jaar </strong>: ", data$year),
      group = "Zwervers"
  ) %>%
  setView(lng = 4, lat = 51, zoom = 8)
  
  # Add world map
  if (addGlobe) {
    
    myMap <- myMap %>%
      addProviderTiles("OpenStreetMap.HOT")
    
  }
  
  # Add legend
  if (legend != "none") {
    myMap <- myMap %>% addLegend(legend,
        pal = year_colors,
        values = ~year,
        title = "Zwervers",
        opacity = 1,
        labFormat = labelFormat(),
        group = "Zwervers",
        layerId = "legend1") %>%
      addLegend(legend,
        pal = territory_palette,
        values = spatialData$Territory,
        title = "Territoria",
        opacity = 1,
        labFormat = labelFormat(),
        group = "Territoria",
        layerId = "legend2")
  }
  
  
  myMap
  
  
}


#' Shiny module for creating map on traffic accidents involving wolves - server side
#' @param schadeData reacive object as returned by \code{loadRawData(type = "wildschade")}
#' @param allSpatialData reactive with sf objects with spatial data 
#' for selected region (and year for WBE)
#' @inheritParams getOutputTitle
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @importFrom webshot2 webshot
#' @importFrom leaflet renderLeaflet setView leafletProxy clearTiles
#' @importFrom dplyr coalesce
#' @export
mapDispersersWolvesServer <- function(
  id, data, variable = "Lot", preSelected = reactive(NULL)) {
  
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
      
      spatialData <- reactive({
          loadWolfShapeData(type = "territory")
        })
      
      spacePlot <- reactive({
          req(subData())
          validate(need(nrow(subData()) > 0, "Er is geen data aanwezig voor de geselecteerde filters. Gelieve een andere selectie te maken."))
          
          mapDispersersWolves(data = subData(), spatialData = spatialData(), addGlobe = TRUE)
          
        })
      
      
      output$spacePlot <- renderLeaflet({
          
          spacePlot() %>%
            leaflet.extras2::addEasyprint(   # use leaflets personal functionality to download maps
              options = leaflet.extras2::easyprintOptions(
                exportOnly = TRUE,
                hideControlContainer = FALSE,  # Keep controls visible
                hideClasses = c("leaflet-control-zoom", "leaflet-control-easyPrint")
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
          
          req(spatialData())
          
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
            territory_names <- sort(unique(spatialData()$Territory))
            territory_palette <- colorFactor(palette = brewer.pal(length(territory_names), "Set2"), domain = territory_names)
            
            proxy %>% addLegend(
                position = input$legend,
                pal = year_colors,
                values = ~year,
                title = "Zwervers",
                opacity = 1,
                labFormat = labelFormat(),
                group = "Zwervers",
                layerId = "legend1") %>%
              addLegend(
                position = input$legend,
                pal = territory_palette,
                values = spatialData()$Territory,
                title = "Territoria",
                opacity = 1,
                labFormat = labelFormat(),
                group = "Territoria",
                layerId = "legend2")
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapDispersersWolves(data = subData(), spatialData = spatialData(), 
            addGlobe = input$globe %% 2 == 0, legend = input$legend)
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spacePlot_center$lng,
            lat = input$spacePlot_center$lat,
            zoom = input$spacePlot_zoom
          )
          
          return(newMap)
          
        }) 
      
      
      # Download the map
      observeEvent(input$download, {
          
          idNote <- showNotification("Aanvraag wordt verwerkt... Even geduld.", type = "message", duration = NULL)
          
          leafletProxy("spacePlot") %>% leaflet.extras2::easyprintMap(
            sizeModes = "CurrentSize",
            filename = nameFile(species = "Wolf",
              content = "kaart", fileExt = "png")
          )
          
          removeNotification(id = idNote)
          
        })
      
      # Download data
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = "Wolf",
            content = "kaartData", fileExt = "csv"),
        content = function(file) {
          
          ## write data to exported file
          write.table(x = subData(), file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ".")
          
        })
      
      
      
    })
  
}



#' Shiny module for creating map on traffic accidents involving wolves - UI side
#' @param filterCode boolean whether to include the option to filter on schade code;
#' default value is FALSE
#' @param filterSubcode boolean, whether to include the option to filter on schade subcode;
#' default value is FALSE
#' @param filterSource boolean, whether to show filter option for source
#' @param filterAccuracy boolean, whether to show filter option for accuracy
#' @param variableChoices named character vector, choices for coloring 
#' @inherit welcomeSectionUI
#' @inheritParams reportingGrofwild-common-args
#' @author mvarewyck
#' @importFrom leaflet leafletOutput
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
          )
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
