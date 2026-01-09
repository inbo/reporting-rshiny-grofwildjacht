#' Create map for schade involving wolves
#' 
#' @param data data.frame main data
#' @inheritParams mapFlanders 
#' @return leaflet map
#' @author sjunius
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles fitBounds setView
#' @export
mapSchadeWolves <- function(
  data, 
  addGlobe = FALSE,
  legend = "topright"
) {
  
  colors <- colorNumeric(
    palette = "YlOrBr",
    domain = data$aantal_schade,
    na.color = "transparent"
  )
  
  myMap <- leaflet(data) %>%
    addPolygons(
      fillColor = ~colors(aantal_schade),
      color = "grey40",
      weight = 0.8, stroke = TRUE,
      fillOpacity = 0.8,
      smoothFactor = 1,
      label = ~paste0(NAAM, ": ", aantal_schade, " schadegevallen"),
      highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
    ) %>%
    setView(lng = 4, lat = 51, zoom = 8)
  
  # Add world map
  if (addGlobe) {
    
    myMap <- myMap %>%
      addProviderTiles("OpenStreetMap.HOT")
    
  }
  
  # Add legend
  if (legend != "none") {
    myMap <- myMap %>% addLegend(
      position = legend,
      pal = colors,
      values = ~aantal_schade,
      title = "Aantal schadegevallen Wolf",
      opacity = 1,
      layerId = "legend")
  }
  
  
  myMap
  
  
}


#' Shiny module for creating map on traffic accidents involving wolves - server side
#' 
#' @param data data.frame main data
#' @inheritParams getOutputTitle
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @author sjunius
#' @import shiny
#' @importFrom webshot2 webshot
#' @importFrom leaflet renderLeaflet setView leafletProxy clearTiles leafletOutput
#' @importFrom dplyr filter group_by summarise left_join
#' @export
mapSchadeWolvesServer <- function(
  id, data, preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      results <- reactiveValues()
      
      subData <- reactive({
          
          req(data())
          
          spatialData <- loadWolfShapeData(type = "gemeenten")
          req(spatialData)
          
          req(!is.null(preSelected()))
          req(!is.null(preSelected()$time()))
          
          years <- preSelected()$time()[1]:preSelected()$time()[2]
          
          data <- data() %>%
            filter(Schade == "Wolf") %>%
            filter(Gemeente != "")  %>%
            filter(year %in% years) %>%            
            group_by(Gemeente) %>%
            summarise(aantal_schade = n(), .groups = "drop")
          
          data <- spatialData %>%
            left_join(data, by = c("NAAM" = "Gemeente"))
          
          data
          
        })
      
      spacePlot <- reactive({
          req(subData())
          validate(need(nrow(subData()) > 0, "Er is geen data aanwezig voor de geselecteerde filters. Gelieve een andere selectie te maken."))
          
          mapSchadeWolves(data = subData(), addGlobe = TRUE)
          
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
          
          proxy <- leafletProxy("spacePlot", data = subData())
          req(!is.null(proxy))
          
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            colors <- colorNumeric(
              palette = "YlOrBr",
              domain = subData()$aantal_schade,
              na.color = "transparent"
            )
            
            proxy %>% addLegend(
                position = input$legend,
                pal = colors,
                values = ~aantal_schade,
                title = "Aantal schadegevallen Wolf",
                opacity = 1,
                layerId = "legend")
            
          }
          
        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapSchadeWolves(data = subData(), 
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



#' Shiny module for creating map on traffic accidents involving wolves - UI side
#'
#' @inherit welcomeSectionUI
#' @inheritParams reportingGrofwild-common-args
#' @author sjunius
#' @importFrom leaflet leafletOutput
#' @export
mapSchadeWolvesUI <- function(
  id, uiText, plotFunction = "mapSchadeWolvesUI", context = "description",
  doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(  
    
    actionLink(inputId = ns("linkMapSchadeWolves"),
      label = h3(HTML(title))),
    conditionalPanel(paste("input.linkMapSchadeWolves % 2  ==", as.numeric(doHide)), ns = ns,
      
      wellPanel(
        fixedRow(
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
