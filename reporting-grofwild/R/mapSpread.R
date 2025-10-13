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
  
  myColors <- if (grepl("model", variable) && "Al aanwezig" %in% groupNames && "0 %" %in% groupNames)
      c("#FCFCED", suppressWarnings(RColorBrewer::brewer.pal(n = length(groupNames) - 2, "YlOrBr")), "gray") else if (grepl("model", variable) && "Al aanwezig" %in% groupNames)
      c(suppressWarnings(RColorBrewer::brewer.pal(n = length(groupNames) - 1, "YlOrBr")), "gray") else if (grepl("model", variable) && "0 %" %in% groupNames)
      c("#FCFCED", suppressWarnings(RColorBrewer::brewer.pal(n = length(groupNames) - 1, "YlOrBr"))) else if (grepl("model", variable))
      suppressWarnings(RColorBrewer::brewer.pal(n = length(groupNames), "YlOrBr")) else if (grepl("risk", variable))
      c('red', 'orange', 'green', 'white') else if (grepl("start", variable))
      "gray"
  
  list(
    colors = myColors,
    levels = groupNames
  )
  
}




#' Create leaflet map for the spread of a species
#' 
#' @param spreadShape sf as created by \code{createSpreadData}
#' @inheritParams mapFlanders
#' @return leaflet map
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapSpread <- function(spreadShape, legend = "none", addGlobe = FALSE) {
  
  
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
      fillOpacity = 0.8,
      fillColor =  ~pal_model(outcome),
      weight = if (spatialLevel == "pixels") 0 else 0.75,
      color = "gray",
      group = "modelPolygons")
  
  
  if ("start" %in% colnames(spreadShape)) {
    
    startShape <- subset(spreadShape, !is.na(spreadShape$start))
    
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
    
    if ("start" %in% colnames(spreadShape))
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


#' Create leaflet map for toekomstig verspreidingsgebied of Bevers
#' 
#' @param beverData list, with sf data.frame 
#' @inheritParams mapSpread 
#' @return leaflet map
#' 
#' @author mvarewyck
#' @import leaflet
#' @export
mapBevers <- function(beverData, 
  addGlobe = FALSE, legend = "none") {
  
  myMap <- leaflet() 
  
  factpal <- colorFactor(palette =c('#99ccff', '#6699ff','#3366ff'), beverData$Vestigingskans)

  myMap <- myMap %>%
    clearShapes() %>%
    addPolygons(data = beverData,
      color = ~factpal(beverData$Vestigingskans),
      opacity=0,
      fillOpacity=0.75,
      popup = ~label)
  
  if (legend != "none") { 
    
    myMap <- addLegend(
      map = myMap, 
      position = legend,
      pal = factpal, 
      values = beverData$Vestigingskans,
      title = "Kans op vestiging",
      opacity = 0.75,
      layerId = "legend")
  }
  
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
#' @importFrom webshot2 webshot
#' @importFrom htmlwidgets saveWidget
#' @export
mapSpreadServer <- function(id, 
  allSpatialData, species,
  type = c("F06", "F17_4"), title = reactive(NULL), preSelected = reactive(NULL)) {
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
            species = species, 
            regionLevel = input$regionLevel, 
            year = NULL
          )
          
        })
      
      output$region <- renderUI({
          
          selectInput(
            inputId = ns("region"), label = "Regio('s)",
            choices = sort(unique(spatialData()$NAAM)),
            selected = if (req(input$regionLevel) == "flanders")
              spatialData()$NAAM[1] else if (req(input$regionLevel) %in% c("provinces", "faunabeheerzones")) 
              unique(spatialData()$NAAM) else "",
            multiple = TRUE
          )
          
        })
      
      selectedPolygons <- reactive({
          
          validate(need(spatialData(), "Geen data beschikbaar"))
          validate(need(input$region, "Gelieve regio('s) te selecteren"))

          subset(spatialData(), spatialData()$NAAM %in% input$region)
          
        })
      
      
      ## Map for spread ##
      ## -------------- ##
      
      shapeData <- reactive({
          
          if (type == "F17_4") {
            
            if (!exists("spreadData"))
              spreadData <- loadSpreadData()
            
            spreadData[grep(req(input$mapScale), names(spreadData), value = TRUE)]
            
          } else if (type == "F06") {
            
            if (!exists("trafficData"))
              trafficData <- loadTrafficData()
            trafficData
            
          }
          
        })
      
      output$year <- renderUI({
          
          choices <- sapply(names(shapeData()), function(x) strsplit(x, split = "_")[[1]][2])
          names(choices) <- choices
          
          selectInput(inputId = ns("year"), label = "Jaar", choices = choices)
          
        })
      
      selectedShape <- reactive({
          
          req(shapeData())
          
          shapeData <- if (type == "F06") 
              shapeData() else
              shapeData()[[grep(req(input$year), names(shapeData()), value = TRUE)]]
          
          if ("wildsrt" %in% colnames(shapeData)) 
            shapeData <- shapeData[!is.na(shapeData$wildsrt) & shapeData$wildsrt == species, ]
          
          shapeData
        })
      
      spreadPlot <- reactive({
          
          baseMap <- if (type == "F17_4") {
              
              validate(need(nrow(selectedShape()) > 0, "Geen data beschikbaar"))
              
              mapSpread(
                spreadShape = selectedShape(),
                legend = "topright",
                addGlobe = TRUE
              ) 
              
            } else if (type == "F06") {
              
              mapVerkeer(trafficData = selectedShape(), addGlobe = TRUE)
              
            }
          
          
          baseMap %>%
            addPolylines(data = selectedPolygons(), color = "black", weight = 3,
              group = "regionLines")
          
        })
      
      
      output$spreadPlot <- renderLeaflet({
          
          spreadPlot() %>%
            leaflet.extras2::addEasyprint(   # use leaflets personal functionality to download maps
              options = leaflet.extras2::easyprintOptions(
                exportOnly = TRUE,
                hideControlContainer = FALSE,  # Keep controls visible
                hideClasses = c("leaflet-control-zoom", "leaflet-control-easyPrint")
              )
            )
          
        })
      
      output$spreadPlotUI <- renderUI({
          
          tryCatch({
              leafletOutput(ns("spreadPlot"))
            },
            error = function(e) {
              return(NULL) 
            })
          
        })
      
      
      # Center view
      observe({
          
          # Update after plot
          req(spreadPlot())
          
          centerValues <- getCenterView(sf_object = selectedPolygons())
          
          leafletProxy("spreadPlot", data = spatialData()) %>%
            
            fitBounds(lng1 = centerValues[1], lng2 = centerValues[2],
              lat1 = centerValues[3], lat2 = centerValues[4])
          
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
              
              proxy %>% addCircleMarkers(data = selectedShape()$oversteek,
                radius = 3,
                color = "black",
                stroke = FALSE,
                fillOpacity = 1,
                group = "oversteek")
              
            } else {
              
              proxy %>% clearGroup("oversteek")
              
            }
            
            if ("ecorasters" %in% input$layers) {
              
              proxy %>% addPolylines(data = selectedShape()$ecorasters,
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
                spreadShape = selectedShape(),
                legend = input$legend,
                addGlobe = input$globe %% 2 == 0
              ) 
              
            } else if (type == "F06") {
              
              mapVerkeer(trafficData = selectedShape(), 
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
        
          return(newMap)
          
        }) 
      
      
      # Download the map
      observeEvent(input$download, {
          
          idNote <- showNotification("Aanvraag wordt verwerkt... Even geduld.", type = "message", duration = NULL)
          
          leafletProxy("spreadPlot") %>% leaflet.extras2::easyprintMap(
            sizeModes = "CurrentSize",
            filename = nameFile(species = species,
              content = "kaart", fileExt = "png")
          )
          
          removeNotification(id = idNote)
          
        })
      
      # Download data
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = paste(species, collapse = "-"),
            content = "kaartData", fileExt = "csv"),
        content = function(file) {
          
          myData <- sf::st_drop_geometry(selectedShape())
          
          ## write data to exported file
          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
      
      
      ## Application for Bever
      
      beverData <- reactive({
          req(species == "Bever")
          loadBeverData()
          
        })
      
      output$beverFiltersUI <- renderUI({
          req(beverData())
          
          tagList(
            checkboxGroupInput(ns("rb"), "Kans op vestiging in 2026:",
              levels(beverData()$Vestigingskans),
              selected = character(0)),
            fluidRow(
              column(4, sliderInput(ns("overst"), "Percentage overstromingsgevoelig gebied:",
                  min(beverData()$Overstromingsrisico), max(beverData()$Overstromingsrisico),
                  value = range(beverData()$Ovrstrm), step = 5)),
              column(4, sliderInput(ns("rust"), "Percentage potentieel rustgebied:",
                  min(beverData()$Rustzones), max(ceiling(beverData()$Rustzones)),
                  value = range(beverData()$Rustzones), step = 1)),
              column(4, sliderInput(ns("habitat"), "Percentage kwestbaar habitat:",
                  min(beverData()$Habitats), max(ceiling(beverData()$Habitats)),
                  value = range(beverData()$Habitats), step = 5))
            ),
            fluidRow(
              column(4, sliderInput(ns("soorten"), "Aantal soorten met mogelijk positief effect:",
                  min(beverData()$HRLsoorten), max(beverData()$HRLsoorten),
                  value = range(beverData()$HRLsoorten), step = 1)),
              column(4, sliderInput(ns("vissen"), "Aantal vissen met mogelijk negatief effect:",
                  min(beverData()$HRLvissen), max(beverData()$HRLvissen),
                  value = range(beverData()$HRLvissen), step = 1))
            )
          )
        })
      
      filteredBeverData <- reactive({
          req(beverData())
          req(input$rb)
          req(input$overst)
          req(input$rust)
          req(input$habitat)
          req(input$soorten)
          req(input$vissen)
          
          beverData() %>% filter(
            !is.na(Vestigingskans),
            Vestigingskans %in% input$rb,
            Overstromingsrisico >= input$overst[1], Overstromingsrisico <= input$overst[2],
            Rustzones >= input$rust[1], Rustzones <= input$rust[2],
            Habitats >= input$habitat[1], Habitats <= input$habitat[2],
            HRLsoorten >= input$soorten[1], HRLsoorten <= input$soorten[2],
            HRLvissen >= input$vissen[1], HRLvissen <= input$vissen[2]
          )
        })
      
        spreadPlotBever <- reactive({
            validate(need(length(input$rb) > 0, "Gelieve een kans op vestiging te selecteren"))
            mapBevers(
              filteredBeverData(), 
              addGlobe = isolate(input$globeBever) %% 2 == 0, 
              legend = isolate(input$legendBever))
          })
        
        
        output$spreadPlotBever <- renderLeaflet({
            spreadPlotBever() %>%
              leaflet.extras2::addEasyprint(   # use leaflets personal functionality to download maps
                options = leaflet.extras2::easyprintOptions(
                  exportOnly = TRUE,
                  hideControlContainer = FALSE,  # Keep controls visible
                  hideClasses = c("leaflet-control-zoom", "leaflet-control-easyPrint")
                )
              )
          })
      
      output$spreadPlotBeverUI <- renderUI({
          tryCatch({
              leafletOutput(ns("spreadPlotBever"))
            },
            error = function(e) {
              return(NULL) 
            })
          
        })
      
      # Add world map
      observe({
          
          proxy <- leafletProxy("spreadPlotBever", data = spatialData())
          
          if (!is.null(input$globeBever) & !is.null(proxy)){
            
            if (input$globeBever %% 2 == 0){
              
              updateActionLink(session, inputId = "globeBever", 
                label = "Verberg landkaart")
              
              proxy %>% addProviderTiles("OpenStreetMap.HOT")
              
            } else {
              
              updateActionLink(session, inputId = "globeBever", 
                label = "Voeg landkaart toe")
              
              proxy %>% clearTiles()
              
            }
            
          }
          
        })
      
      
      # Add legend
      observe({
          
          req(input$legendBever)
          
          proxy <- leafletProxy("spreadPlotBever")
          req(!is.null(proxy))
          
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legendBever != "none") {
            
            factpal <- colorFactor(palette =c('#99ccff', '#6699ff','#3366ff'), filteredBeverData()$Vestigingskans)
            
            proxy %>% addLegend(
              position = input$legendBever,
              pal = factpal, 
              values = filteredBeverData()$Vestigingskans,
              title = "Kans op vestiging",
              opacity = 0.75,
              layerId = "legend")
            
          }
          
        })
      
      finalMapBever <- reactive({
          
          newMap <- mapBevers(
            filteredBeverData(), 
            addGlobe = input$globeBever %% 2 == 0, 
            legend = input$legendBever)
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spreadPlotBever_center$lng,
            lat = input$spreadPlotBever_center$lat,
            zoom = input$spreadPlotBever_zoom
          )
          
          return(newMap)
          
        }) 
      
      
      # Download the map
      observeEvent(input$downloadBever, {
          
          idNote <- showNotification("Aanvraag wordt verwerkt... Even geduld.", type = "message", duration = NULL)
          
          leafletProxy("spreadPlotBever") %>% leaflet.extras2::easyprintMap(
            sizeModes = "CurrentSize",
            filename = nameFile(species = species,
              content = "kaart", fileExt = "png")
          )
          
          removeNotification(id = idNote)
          
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
#' @inherit welcomeSectionUI
#' @param showLayer boolean, whether to display option to choose layer
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @return UI object
#' @export
mapSpreadUI <- function(id, 
  uiText, context = id, specie = NULL,
  doHide = TRUE, showLayer = FALSE,
  regionChoices = c(
      "Vlaanderen" = "flanders",
      "Provincie" = "provinces", 
      "Faunabeheerzones" = "faunabeheerzones",
      "Gemeente" = "communes")
  ) {
  
  ns <- NS(id)
  
  
  if (specie == "Bever") {
    title <- getOutputTitle(output = "mapSpreadUI", specie = specie, 
      uiText = uiText)
    description <- getOutputDescription(output = paste0("mapSpreadUI_", specie), 
      specie = specie, uiText = uiText, context = context)
  } else {
    title <- getOutputTitle(output = "mapSpreadUI", specie = specie, 
      uiText = uiText)
    description <- getOutputDescription(output = "mapSpreadUI", 
      specie = specie, uiText = uiText, context = context)
  }
  
  # Map spread
  
  tagList(
    
    actionLink(inputId = ns("linkSpread"),
      label = tags$h3(title)),
    conditionalPanel(
      condition = paste("input.linkSpread % 2 ==", as.numeric(doHide)),
      ns = ns,
      
      uiOutput(ns("disclaimerMapSpread")),
  
      if (specie == "Bever") {   # Custom app for Toekomstig verspreidingsgebied Bever
          tagList(
            wellPanel(
              uiOutput(ns("beverFiltersUI")),
              fluidRow(
                column(4, selectInput(inputId = ns("legendBever"), label = "Legende",
                    choices = c(
                      "Bovenaan rechts" = "topright",
                      "Onderaan rechts" = "bottomright",
                      "Bovenaan links" = "topleft",
                      "Onderaan links" = "bottomleft",
                      "<geen>" = "none")) 
                )),
              fluidRow(column(12, actionLink(inputId = ns("globeBever"), label = "Voeg landkaart toe",
                  icon = icon("globe"))))
            ),
            fixedRow(
              column(12,
                withSpinner(uiOutput(ns("spreadPlotBeverUI"))),
                tags$br(),
                actionButton(ns("downloadBever"), label = "Download figuur", class = "downloadButton")
              )
            ),
            tags$p(HTML(description)),
            
            tags$hr()
          )
        
      } else {
        tagList(
          wellPanel(
            
            if(!is.null(regionChoices)){
              fixedRow(
                column(8, uiOutput(ns("region"))),
                column(4, 
                  selectInput(
                    inputId = ns("regionLevel"), label = "Regio-schaal",
                    choices = regionChoices, selected = "flanders"
                  )
                )
              )
            },
            
            if (showLayer) {
                checkboxGroupInput(inputId = ns("layers"), label = "Toon",
                  choices = c(
                    "Preventieve rasters" = "ecorasters",
                    "Preventieve signalisatie/snelheidsbeperkingen" = "oversteek"),
                  selected = c("ecorasters", "oversteek"),
                  inline = TRUE)
                
              } else {
                
                fixedRow(
                  column(4, 
                    selectInput(
                      inputId = ns("mapScale"), 
                      label = "Kaartweergave",
                      choices = c(
                        "Gemeente" = "municipalities",
                        "2x2 UTM" = "pixels"
                      )
                    )
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
              withSpinner(uiOutput(ns("spreadPlotUI"))),
              tags$br(),
              actionButton(ns("download"), label = "Download figuur", class = "downloadButton"),
              if (!showLayer)
                downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton")
            )
          ),
          
          tags$p(HTML(description)),
          
          tags$hr()
        )
      }
    )
    
  )
  
}
