# Map(s) for traffic accidents involving wolves
# 
# Author: sjunius
###############################################################################

#' Create map for UTM hokken involving wolves
#' 
#' @param data data.frame main data
#' @inheritParams mapFlanders 
#' @return leaflet map
#' @author sjunius
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles fitBounds setView
#' @importFrom RColorBrewer brewer.pal
#' @importFrom INBOtheme inbo_palette
#' @export
mapUTMWolves <- function(
  data, 
  addGlobe = FALSE,
  legend = "none"
) {
  
#  # Color palette
#  nColors <- length(levels(data$variable))
#  colors <- if (nColors < 10) {
#      inbo_palette(n = nColors) 
#    } else {
#      paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
#      unlist(sapply(paletteNames, function(x)
#            suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
#    }
#  
#  palette <- colorFactor(colors, levels(data$variable))
  
    myMap <- leaflet(data) %>%
            
      addPolygons(fillColor = "#fee391",
        stroke = TRUE,
        smoothFactor = 1,
        color = "black", weight = 0.5,
        fillOpacity = 0.7) %>%
      setView(lng = 4, lat = 51, zoom = 8)
    
    # Add world map
    if (addGlobe) {
        
        myMap <- myMap %>%
                    addProviderTiles("OpenStreetMap.HOT")
        
    }
    
#    # Add legend
#    if (legend != "none") {
#        
#        myMap <- addLegend(
#                map = myMap,
#                position = legend,
#                pal = palette, 
#                values = ~Soort,
#                opacity = 1,
#                na.label = "onbekend",
#                title = "Legende",
#                layerId = "legend"
#        )
#        
#        
#    }
    
    
    myMap
    
    
}


#' Shiny module for creating map for UTM-hokken for wolves - server side
#' 
#' @param data data.frame main data
#' @param definedYear numeric, single numeric value specifying the year value 
#' @inheritParams getOutputTitle
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @author sjunius
#' @import shiny
#' @importFrom leaflet renderLeaflet setView leafletProxy clearTiles leafletOutput
#' @importFrom dplyr right_join
#' @export
mapUTMWolvesServer <- function(
  id, data, preSelected = reactive(NULL),
  definedYear = config::get("defaultYear", file = system.file("config.yml", package = "reportingGrofwild"))) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      results <- reactiveValues()
      
      output$year_wolf <- renderUI({
          req(data())
          
          timeRange <- range(data()$year)
          
          div(class = "sliderBlank", 
            sliderInput(inputId = ns("year"), label = "Jaar", 
              value = definedYear,
              min = timeRange[1],
              max = timeRange[2],
              step = 1,
              sep = "")
          )
        })
      
      subData <- reactive({
          
          req(data())
          req(input$year)
          
          spatialData <- loadWolfShapeData(type = "utm")
          req(spatialData)
          
          right_join(spatialData, data(), by = "TAG", copy = TRUE) %>% filter(year == input$year)
          
        })
      
      spacePlot <- reactive({
          req(subData())
          validate(need(nrow(subData()) > 0, "Er is geen data aanwezig voor de geselecteerde filters. Gelieve een andere selectie te maken."))
          
          mapUTMWolves(data = subData(), addGlobe = TRUE)
          
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
          
          leafletProxy("spacePlot", data = subData())  %>%
            setView(lng = 4, lat = 51, zoom = 8)
          
        })
      
      # Add world map
      observe({
          req(subData())
          proxy <- leafletProxy("spacePlot")
          
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
          
          proxy <- leafletProxy("spacePlot")
          req(!is.null(proxy))
          
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            # Color palette
            nColors <- length(levels(subData()$variable))
            
            colors <- if (nColors < 10) {
                inbo_palette(n = nColors) 
              } else {
                paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
                unlist(sapply(paletteNames, function(x)
                      suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
              }
            
            palette <- colorFactor(colors, levels(subData()$variable))
            
            proxy %>% addLegend(
                position = input$legend,
                pal = palette, 
                values = ~variable,
                opacity = 0.8,
                na.label = "onbekend",
                title = "Legende",
                layerId = "legend"
              )
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapUTMWolves(data = subData(),
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
          write.table(x = sf::st_drop_geometry(subData()), file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ".")
          
        })
      
    
    
    })
    
}



#' Shiny module for creating map for UTM-hokken for wolves - UI side
#' 
#' @inherit welcomeSectionUI
#' @inheritParams reportingGrofwild-common-args
#' @author sjunius
#' @importFrom leaflet leafletOutput
#' @importFrom shinyjs hide
#' @export
mapUTMWolvesUI <- function(
  id, uiText, plotFunction = "mapUTMWolvesUI", context = "description", showYear = FALSE,
  showLegend = FALSE, doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(  
    
    actionLink(inputId = ns("linkMapUTMWolves"),
      label = h3(HTML(title))),
    conditionalPanel(paste("input.linkMapUTMWolves % 2  ==", as.numeric(doHide)), ns = ns,
      
      wellPanel(
        fixedRow(
          if (showYear) {
            column(4, uiOutput(ns("year_wolf")))
          },
          if (showLegend) {
              column(4, selectInput(inputId = ns("legend"), label = "Legende",
                  choices = c(
                    "Bovenaan rechts" = "topright",
                    "Onderaan rechts" = "bottomright",
                    "Bovenaan links" = "topleft",
                    "Onderaan links" = "bottomleft",
                    "<geen>" = "none"), selected = "none")
              ) 
            } else {
              column(4, hide(selectInput(inputId = ns("legend"), label = "Legende",
                    choices = c(
                      "Bovenaan rechts" = "topright",
                      "Onderaan rechts" = "bottomright",
                      "Bovenaan links" = "topleft",
                      "Onderaan links" = "bottomleft",
                      "<geen>" = "none"), selected = "none")
                ) )
            }
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
