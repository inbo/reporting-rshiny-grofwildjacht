# Functions to plot the interactive map for verspreiding everzwijn dashboard
# 
# Author: mvarewyck
###############################################################################



#' Necessary info for the color palette of \code{\link{mapSpread}}
#' @param variable character, data variable for which the \code{groupNames} are defined
#' and that will be used for coloring
#' @param groupNames character, factor levels that will be used as labels for colors
#' @return list with colors, character vector and levels, character vector. 
#' Each item has same length as \code{units}
#' 
#' @author mvarewyck
#' @importFrom grDevices palette
#' @importFrom RColorBrewer brewer.pal
#' @export
paletteMap <- function(variable, groupNames) {
  
  myColors <- if (grepl("model", variable) && "Al aanwezig" %in% groupNames)
      c(RColorBrewer::brewer.pal(n = length(groupNames) - 1, "YlOrBr"), "gray") else if (grepl("model", variable))
      RColorBrewer::brewer.pal(n = length(groupNames), "YlOrBr") else if (grepl("risk", variable))
      c('red', 'orange', 'green', 'white') else if (grepl("start", variable))
      "gray"
  
  list(
    colors = myColors,
    levels = groupNames
  )
  
}




#' Create leaflet map for the spread of a species
#' 
#' @param spreadShape SpatialPolygonsDataFrame as created by \code{createSpreadData}
#' @param startYear integer, starting year; only relevant for pixels & model unit 
#' @inheritParams mapFlanders
#' @return leaflet map
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapSpread <- function(spreadShape, startYear = 2019, legend = "none", addGlobe = FALSE) {
  
  
  unit <- attr(spreadShape, "unit")
  spatialLevel <- attr(spreadShape, "spatialLevel")
  year <- attr(spreadShape, "year")
  
  modelColors <- paletteMap(variable = unit, groupNames = levels(spreadShape$outcome))
  pal_model <- colorFactor(palette = modelColors$colors, levels = modelColors$levels, ordered = FALSE)
  
  finalMap <- leaflet(spreadShape)
  
  finalMap <- finalMap %>%
    
    addPolygons(
      data = spreadShape,
      stroke = spatialLevel != "pixels",
      smoothFactor = 1,
      fillOpacity = if (spatialLevel == "pixels") 1 else 0.8,
      fillColor =  ~pal_model(outcome),
      weight = if (spatialLevel == "pixels") 0 else 0.75,
      color = "gray",
      group = "modelPolygons")
  
  
  if ("start" %in% colnames(spreadShape@data)) {
    
    startShape <- subset(spreadShape, spreadShape@data$start == startYear)
    
    startColors <- paletteMap(variable = "start", groupNames = unique(startShape$start))
    pal_start <- colorFactor(palette = startColors$colors, levels = startColors$levels, na.color = NA)
    
    finalMap <- finalMap %>%
      addPolygons(
        data = startShape,
        stroke = FALSE,
        smoothFactor = 1,
        fillOpacity = 1,
        fillColor =  ~pal_start(start),
        group = "startPolygons")
    
  }
  
  
  # Add legend
  if (legend != "none") { 
    
    finalMap <- addLegend(
      map = finalMap, 
      position = legend,
      pal = pal_model, 
      values = modelColors$levels,
      title = paste(if (grepl("model", unit))
          "Waarschijnlijkheid verspreiding" else 
          "Risico klasse", "in", year),
      opacity = if (spatialLevel == "pixels") 1 else 0.8,
      na.label = "",
      layerId = "legend")
    
    if ("start" %in% colnames(spreadShape@data))
      finalMap <- addLegend(
        map = finalMap,
        position = legend,
        pal = pal_start, 
        values = startColors$levels,
        title = "Bevestigde aanwezigheid in",
        opacity = 1,
        layerId = "startLegend")
    
  }
  
  # Add background map
  if (addGlobe) {
    
    finalMap <- finalMap %>% addProviderTiles("OpenStreetMap.HOT")
    
  }
  
  attr(finalMap, "modelColors") <- modelColors
  
  finalMap
  
}


#' Create leaflet map for verkeer of a species
#' 
#' @param trafficData list, with sf data.frame for each name specified in \code{layers}
#' @param layers character vector,
#' @inheritParams mapSpread 
#' @return leaflet map
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapVerkeer <- function(trafficData, layers = c("oversteek", "ecorasters"), 
  addGlobe = FALSE) {
  
  myMap <- leaflet() 
  
  if ("oversteek" %in% layers)
    myMap <- myMap %>%
      addPolylines(data = trafficData$ecorasters,
        opacity =  0.5,
        group = "ecorasters") 
  
  if ("ecorasters" %in% layers)
    myMap <- myMap %>%
      addCircleMarkers(data = trafficData$oversteek,
        radius = 3,
        color = "black",
        stroke = FALSE,
        fillOpacity = 1,
        group = "oversteek")
  
  # Add background map
  if (addGlobe)
    myMap <- myMap %>% addProviderTiles("OpenStreetMap.HOT")
  
  
  # For compliance with mapSpread()
  attr(myMap, "modelColors") <- NULL
  
  
  myMap
  
}


#' Shiny module for creating the plot \code{\link{mapFlanders}} - server side
#' @inheritParams mapFlandersServer
#' @param title reactive object, title with asterisk to show in the \code{actionLink}
#' 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom ggplot2 fortify
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @export
mapSpreadServer <- function(id, regionLevel, locaties, allSpatialData, 
  type = c("F06", "F17_4"), title = reactive(NULL)) {
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      ## User Input ##
      ## ---------- ##
      
      
      # Selected regions of interest
      spatialData <- reactive({
          
          req(allSpatialData)
          
          filterSpatial(
            allSpatialData = allSpatialData, 
            species = "Wild zwijn", 
            regionLevel = regionLevel(), 
            year = NULL,
            locaties = locaties()
          )
          
        })
      
      
      selectedPolygons <- reactive({
          
          validate(need(spatialData(), "Geen data beschikbaar"))
          
          subset(spatialData(), spatialData()$NAAM %in% locaties())
          
        })
      
      
      ## Map for spread ##
      ## -------------- ##
      
      shapeData <- reactive({
          
          if (type == "F17_4") {
            
            if (!exists("spreadData"))
              readS3(file = "spreadData.RData")
            spreadData[grep(req(input$regionLevel), names(spreadData), value = TRUE)]
            
          } else if (type == "F06") {
            
            if (!exists("trafficData"))
              readS3(file = "trafficData.RData")            
            trafficData
            
          }
          
        })
      
      output$year <- renderUI({
          
          choices <- sapply(names(shapeData()), function(x) strsplit(x, split = "_")[[1]][2])
          names(choices) <- choices
          
          selectInput(inputId = ns("year"), label = "Jaar", choices = choices)
          
        })
      
      spreadPlot <- reactive({
          
          baseMap <- if (type == "F17_4") {
              
              req(input$year)
              req(shapeData())
              req(grepl(input$year, names(shapeData())))
              
              mapSpread(
                spreadShape = shapeData()[[grep(input$year, names(shapeData()), value = TRUE)]],
                legend = "topright",
                addGlobe = TRUE
              ) 
              
            } else if (type == "F06") {
              
              mapVerkeer(trafficData = shapeData(), addGlobe = TRUE)
              
            }
          
          
          baseMap %>%
            addPolylines(data = selectedPolygons(), color = "black", weight = 3,
              group = "regionLines")
          
        })
      
      
      output$spreadPlot <- renderLeaflet({
          
          spreadPlot()
          
        })
      
      
      # Center view
      observe({
          
          # Update after plot
          req(spreadPlot())
          
          coordData <- suppressMessages(ggplot2::fortify(selectedPolygons()))
          centerView <- c(range(coordData$long), range(coordData$lat))
          
          leafletProxy("spreadPlot", data = spatialData()) %>%
            
            fitBounds(lng1 = centerView[1], lng2 = centerView[2],
              lat1 = centerView[3], lat2 = centerView[4])
          
        })
      
      
      # Title
      observe({
          
          req(title())
          updateActionLink(session = session, inputId = "linkSpread",
            label = paste("FIGUUR:", title()))
          
        })
      
      output$disclaimerMapSpread <- renderUI({
          
          req(title())
          
          if (grepl("\\*", title()))
            getDisclaimerLimited()
          
        })
      
      # Add world map
      observe({
          
          proxy <- leafletProxy("spreadPlot", data = spatialData())
          
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
          
          proxy <- leafletProxy("spreadPlot")
          req(!is.null(proxy))
          
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            modelColors <- attr(spreadPlot(), "modelColors")
            pal_model <- colorFactor(palette = modelColors$colors, levels = modelColors$levels, na.color = NA)
            
            proxy %>% addLegend(
              position = input$legend,
              pal = pal_model, 
              values = modelColors$levels,
              title = paste(if (grepl("model", "model_EP"))
                  "Waarschijnlijkheid verspreiding" else 
                  "Risico klasse", "in", input$year),
              opacity = if (!is.null(input$regionLevel) && input$regionLevel != "pixels") 0.8 else 1,
              na.label = "",
              layerId = "legend")
            
          }
          
        })
      
      
      # Traffic data layers
      observe({
          
          proxy <- leafletProxy("spreadPlot")
          
          if (type == "F06" & !is.null(proxy)) {
            
            if ("oversteek" %in% input$layers) {
              
              proxy %>% addCircleMarkers(data = shapeData()$oversteek,
                radius = 3,
                color = "black",
                stroke = FALSE,
                fillOpacity = 1,
                group = "oversteek")
              
            } else {
              
              proxy %>% clearGroup("oversteek")
              
            }
            
            if ("ecorasters" %in% input$layers) {
              
              proxy %>% addPolylines(data = shapeData()$ecorasters,
                opacity =  0.5,
                group = "ecorasters")
              
            } else {
              
              proxy %>% clearGroup("ecorasters")
              
            }
            
          }
          
        })
     
      
      # Create final map (for download)
      finalMap <- reactive({
          
          newMap <- if (type == "F17_4") {
              
              mapSpread(
                spreadShape = shapeData(),
                legend = input$legend,
                addGlobe = input$globe %% 2 == 0
              ) 
              
            } else if (type == "F06") {
              
              mapVerkeer(trafficData = shapeData(), 
                layers = input$layers,
                addGlobe = input$globe %% 2 == 0
                )
              
            }
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spreadPlot_center$lng,
            lat = input$spreadPlot_center$lat,
            zoom = input$spreadPlot_zoom
          ) %>%
          # black borders for preselected regions
          addPolylines(data = selectedPolygons(), color = "black", weight = 3,
            group = "regionLines")
        
        newMap
          
        }) 
      
      
      # Download the map
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = "Wild zwijn",
            content = "kaart", fileExt = "png"),
        content = function(file) {
          
          tmpFile <- tempfile(fileext = ".html")
          
          # write map to temp .html file
          htmlwidgets::saveWidget(finalMap(), file = tmpFile, selfcontained = FALSE)
          
          # convert temp .html file into .png for download
          webshot::webshot(url = tmpFile, file = file,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
        }
      )
      
      # Download data
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = paste("Wild zwijn", collapse = "-"),
            content = "kaartData", fileExt = "csv"),
        content = function(file) {
          
          myData <- shapeData()@data
          
          ## write data to exported file
          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
        
    return(reactive({
          # Update when any of these change
          finalMap()
          input
          # Return the static values
          c(
            list(plot = isolate(finalMap())),
            isolate(reactiveValuesToList(input))
          )
        }))
      
    })
  
}



#' Shiny module for creating the plot \code{\link{mapSpread}} - UI side
#' @template moduleUI 
#' @param showLayer boolean, whether to display option to choose layer
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
mapSpreadUI <- function(id, uiText, showLayer = FALSE) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == paste(strsplit(id, "_")[[1]][-1], collapse = "_"), ]
  
  # Map spread
  
  tagList(
    
    actionLink(inputId = ns("linkSpread"),
      label = paste("FIGUUR:", uiText$title), class = "action-h3"),
    conditionalPanel("input.linkSpread % 2 == 0", ns = ns,
      
      uiOutput(ns("disclaimerMapSpread")),
      
      tags$p(HTML(uiText[, strsplit(id, split = "_")[[1]][1]])),
  
      wellPanel(
        
        if (showLayer) {
            checkboxGroupInput(inputId = ns("layers"), label = "Toon",
              choices = c(
                "Preventieve rasters" = "ecorasters",
                "Preventieve signalisatie/snelheidsbeperkingen" = "oversteek"),
              selected = c("ecorasters", "oversteek"),
              inline = TRUE)
            
          } else {
            
            fixedRow(
              column(4, selectInput(inputId = ns("regionLevel"), label = "Regio-schaal",
                  choices = c(
                    "Gemeente" = "municipalities",
                    "2x2 UTM" = "pixels"
                  ))
              ),
              column(4, uiOutput(ns("year"))),
              column(4, selectInput(inputId = ns("legend"), label = "Legende",
                  choices = c(
                    "Bovenaan rechts" = "topright",
                    "Onderaan rechts" = "bottomright",
                    "Bovenaan links" = "topleft",
                    "Onderaan links" = "bottomleft",
                    "<geen>" = "none")) 
#              ),
#              column(4, selectInput(inputId = ns("unit"), label = "Startpopulatie",
#                  choices = c("Exacte pixels" = "model_EP") 
##                    "Optimaal habitat" = "model_OH")
#                )
              )
            )
            
          }
        
        , actionLink(inputId = ns("globe"), label = "Voeg landkaart toe",
          icon = icon("globe"))
      
      ),
      
      fixedRow(
        column(12,
          withSpinner(leafletOutput(ns("spreadPlot"))),
          tags$br(),
          downloadButton(ns("download"), label = "Download figuur", class = "downloadButton"),
          if (!showLayer)
            downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton")
        )
      ),
      
      tags$hr()
    
    )
  )
  
  
}
