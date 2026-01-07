# Map(s) for traffic accidents involving wolves
# 
# Author: sjunius
###############################################################################

#' Create map for raffic accidents involving wolves
#' @inheritParams mapFlanders 
#' @return leaflet map
#' @author sjui
#' @importFrom leaflet leaflet addCircleMarkers addProviderTiles fitBounds
#' @importFrom RColorBrewer brewer.pal
#' @importFrom INBOtheme inbo_palette
#' @export
mapAccidentsWolves <- function(
        data, 
        variable,
        addGlobe = FALSE,
        legend = "topright"
) {
    
  data$variable <- data[[variable]]
  data$variable <- as.factor(data$variable)
  
  # Color palette
  nColors <- length(levels(data$variable))
  colors <- if (nColors < 10) {
      inbo_palette(n = nColors) 
    } else {
      paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
      unlist(sapply(paletteNames, function(x)
            suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
    }
  
  palette <- colorFactor(colors, levels(data$variable))
  
  
    myMap <- leaflet(data) %>%
            
      addCircleMarkers(~X_Coord, ~Y_Coord,
        radius = 6,
        fillColor = ~palette(variable),
        stroke = TRUE, color = "black", weight = 1, 
        fillOpacity = 1,
        popup = paste0("<h4>Info</h4>",  
          "<ul>", 
          "<li><strong> Jaar </strong>: ", data$Year,
          "<li><strong> Lot </strong>: ", data$Lot,
          "</ul>"
        )) %>%
      setView(
        lng = mean(data$X_Coord),
        lat = mean(data$Y_Coord),
        zoom = 9)
    
    # Add world map
    if (addGlobe) {
        
        myMap <- myMap %>%
                    addProviderTiles("OpenStreetMap.HOT")
        
    }
    
    # Add legend
    if (legend != "none") {
        
        myMap <- addLegend(
                map = myMap,
                position = legend,
                pal = palette, 
                values = ~variable,
                opacity = 1,
                na.label = "onbekend",
                title = "Legende",
                layerId = "legend"
        )
        
        
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
mapAccidentsWolvesServer <- function(
  id, data, variable = "Lot", preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      results <- reactiveValues()
      
      subData <- reactive({
          
          req(data())
          req(!is.null(preSelected()))
          req(!is.null(preSelected()$time()))
          
          years <- preSelected()$time()[1]:preSelected()$time()[2]
          
          data <- data() %>%
            filter(Levend == 0) %>%
            filter(!(Lot %in% c("Niet geweten", "Verdronken"))) %>%
            filter(Year %in% years)
          
          data$X_Coord <- as.numeric(gsub(",", ".", data$X_Coord))
          data$Y_Coord <- as.numeric(gsub(",", ".", data$Y_Coord))
          
          data
          
        })
      
      spacePlot <- reactive({
          req(subData())
          
          mapAccidentsWolves(data = subData(), variable = variable, addGlobe = TRUE, legend = input$legend)
          
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
            setView(
              lng = mean(subData()$X_Coord),
              lat = mean(subData()$Y_Coord),
              zoom = 9)
          
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
      
#      # Add legend
#      observe({
#          
#          req(input$legend)
#          req(subData())
#          
#          proxy <- leafletProxy("spacePlot")
#          req(!is.null(proxy))
#          
#          proxy %>% removeControl(layerId = "legend")
#          
#          if (input$legend != "none") {
#            browser()
#            # Color palette
#            nColors <- length(levels(subData()[[variable]]))
#            colors <- if (nColors < 10) {
#                inbo_palette(n = nColors) 
#              } else {
#                paletteNames <- c("Set3", "Paired", "Dark2", "Pastel2")
#                unlist(sapply(paletteNames, function(x)
#                      suppressWarnings(brewer.pal(n = 12, name = x))))[1:nColors]
#              }
#            
#            palette <- colorFactor(colors, levels(subData()[[variable]]))
#            
#            proxy %>% addLegend(
#                map = myMap,
#                position = input$legend,
#                pal = palette, 
#                values = ~variable,
#                opacity = 0.8,
#                na.label = "onbekend",
#                title = "Legende",
#                layerId = "legend"
#              )
#            
#          }
#          
#        })
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- mapAccidentsWolves(data = subData(), variable = variable, 
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
mapAccidentsWolvesUI <- function(
  id, uiText, plotFunction = "mapAccidentsWolvesUI", context = "description",
  doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(  
    
    actionLink(inputId = ns("linkMapAccidentsWolves"),
      label = h3(HTML(title))),
    conditionalPanel(paste("input.linkMapAccidentsWolves % 2  ==", as.numeric(doHide)), ns = ns,
    
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
