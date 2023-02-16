# Functions to plot the interactive map for flanders
# 
# Author: mvarewyck
###############################################################################



#' Define province based on commune NIS code
#' @param NISCODE character vector, NIS codes from communes, for which to define province
#' @inheritParams createSpaceData
#' @return character vector, same length and order as \code{NISCODE}
#' with corresponding province for each communeS
#' @author mvarewyck
#' @export
getProvince <- function(NISCODE, allSpatialData) {
  
  communeCode <- substr(NISCODE, start = 1, stop = 1)
  
  provinceData <- allSpatialData$provinces@data[, c("NISCODE", "NAAM")]
  provinceData$NISCODE <- substr(provinceData$NISCODE, start = 1, stop = 1)
  
  sapply(communeCode, function(iCode) {
      
      if (is.na(iCode))
        NA else
        as.character(provinceData[provinceData$NISCODE == iCode, ]$NAAM)
      
    })
  
}



#' Create summary data of geographical data for selected year, species and region level
#' @param data data.frame, geographical data
#' @param allSpatialData list of sp, spatial data for all spatial levels
#' @param biotoopData data.frame, background data for the WBE, as read from \code{loadHabitats}
#' @param year integer, year of interest
#' @param species character, species of interest
#' @param regionLevel character, regional level of interest should be one of 
#' \code{c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes", "utm5" )}
#' @param unit character, whether absolute frequencies, relative frequencies (aantal/100ha),
#' absolute cases, or relative bos freq (aantal/100ha bos & natuur); if 'region'
#' the legend shows different types of regions for WBE
#' should be reported,
#' @inheritParams filterSchade
#' @inheritParams readShapeData
#' @return a list with two items: data - a data.frame with the summary data; stats - a data.frame with the summary statistics
#' @author mvarewyck
#' @export
createSpaceData <- function(data, allSpatialData, biotoopData, 
  year, species, regionLevel,
  unit = c("absolute", "relative", "absoluteCases", "relativeDekking", "region"), 
  sourceIndicator = NULL, 
  dataDir = system.file("extdata", package = "reportingGrofwild")) {
  
  
  # To prevent warnings with R CMD check
  afschotjaar <- NULL
  wildsoort <- NULL
  
  unit <- match.arg(unit)
  
  # Select correct spatial data
  spatialData <- filterSpatial(allSpatialData = allSpatialData, 
    species = species, regionLevel = regionLevel, year = year)
  
  # Framework for summary data
  fullData <- if (regionLevel %in% c("communes", "fbz_gemeentes")) {
      
      spatialData@data[, c("NAAM", "AREA", "NISCODE")]
      
    } else if (regionLevel == "WBE_buitengrenzen") {
      
      tmpData <- spatialData@data[, c("NAAM", "AREA")]        
      tmpData[tmpData$NAAM %in% unique(data$PartijNummer), , drop = FALSE]
      
    } else {
      
      spatialData@data[, c("NAAM", "AREA")]
      
    }
  
  # Bind area bos&natuur
  if (unit == "relativeDekking") {
    if (regionLevel == "WBE_buitengrenzen")
      tmpData <- biotoopData[biotoopData$year %in% year, c("regio", "Area_hab_km2_bos")] else 
      tmpData <- biotoopData[, c("regio", "Area_hab_km2_bos")]
    
    colnames(tmpData) <- c("NAAM", "AREA")
    fullData$AREA <- NULL
    fullData <- merge(fullData, tmpData)
  }
  
  
  if (nrow(fullData) == 0)
    return(NULL)
  
  colnames(fullData)[1] <- "locatie"
  
  
  # For communes -> provide corresponding province in downloaded data
  if (regionLevel %in% c("communes", "fbz_gemeentes")) {
    
    fullData$provincie <- getProvince(
      NISCODE = fullData$NISCODE, 
      allSpatialData = allSpatialData)
    
    # keep NIS for communes data
    if (regionLevel == "fbz_gemeentes") {
      fullData$NISCODE <- NULL
      
    }
    
  }
  
  
  
  # Select subset for time
  if (length(species) > 1 || species != "")
    plotData <- subset(data, subset = afschotjaar %in% year & wildsoort %in% species) else
    plotData <- subset(data, subset = afschotjaar %in% year)
  
  
  plotData <- filterSchade(plotData = plotData, sourceIndicator = sourceIndicator,
    returnStop = "data")
  
  #compute total number of cases to output in stats
  statsDf <- data.frame(nTotal = as.integer(NA), 
    nAvailable = as.integer(NA), 
    percentage = as.numeric(NA)) 
  statsDf[, "nTotal"] <- nrow(plotData)
  
  if (nrow(plotData) == 0) {
    
    allData <- fullData 
    allData$freq <- 0
    
  } else {
    
    # Create general plot data names
    plotData$locatie <- switch(regionLevel,
      flanders = "Vlaams Gewest",
      provinces = plotData$provincie, 
      communes = plotData$gemeente_afschot_locatie,
      faunabeheerzones = plotData$FaunabeheerZone,
      fbz_gemeentes = plotData$fbz_gemeente,
      utm5 = plotData$UTM5,
      WBE_buitengrenzen = plotData$PartijNummer
    )
    
    # Exclude data with missing time or space
    plotData <- subset(plotData, !is.na(plotData$afschotjaar) & 
        !is.na(plotData$locatie) & !plotData$locatie %in% c("", "Onbekend"),
      c("afschotjaar", "locatie")
    )
    
    # Summarize data over years
    summaryData <- plyr::count(df = plotData, vars = names(plotData))
    
    # Add names & times with 0 observations
    allData <- merge(summaryData, fullData, all = TRUE)
    allData$freq[is.na(allData$freq)] <- 0
    
  }
  
  ## stats
  # compute all cases with full info available
  statsDf$nAvailable <- sum(allData$freq)
  statsDf$percentage <- statsDf$nAvailable / statsDf$nTotal * 100

  # Remove redundant variables
  allData$afschotjaar <- NULL
  
  
  summaryData2 <- plyr::count(df = allData, 
    vars = names(allData)[!names(allData) %in% "freq"], 
    wt_var = "freq")
  
  
  # unit taken into account
  if (grepl("relative", unit))
    summaryData2$freq <- summaryData2$freq/summaryData2$AREA 
  
  
  # Create group variable
  if (unit == "region") {
    
    jachtData <- filterSpatial(allSpatialData = allSpatialData, 
      species = species, regionLevel = "WBE", year = year)
    
    if (is.null(jachtData)) {
      
      regionLevels <- NA
      
    } else {

#      regionLevels <- c("Niet-bejaagd", paste0("Jachtterrein (", jachtData@data$WBELID, ")"))
      # Keep only 'aangesloten' #327
      regionLevels <- "Jachtterrein (aangesloten)"
      
    }
    
    summaryData2 <- cbind(summaryData2, data.frame(group = factor(regionLevels)))
    
  } else {
    
    if (regionLevel %in% c("flanders", "provinces")) {
      
      if (unit == "absolute")
        otherBreaks <- unique(sort(summaryData2$freq)) else
        otherBreaks <- unique(sort(ceiling(summaryData2$freq*100)/100))
      
      summaryData2$group <- cut(x = summaryData2$freq, 
        breaks = c(-Inf, otherBreaks),
        labels = otherBreaks) 
      
    } else if (regionLevel == "faunabeheerzones" & "Ree" %in% species) {
      
      if (unit == "absolute")
        summaryData2$group <- cut(x = summaryData2$freq, 
          breaks = c(-Inf, 0, 100, 200, 500, 1000, Inf),
          labels = c("0", "1-100", "100-200", "200-500", "500-1000", ">1000")) else
        summaryData2$group <- cut(x = summaryData2$freq, 
          breaks = c(-Inf, 0, 0.25, 0.5, 1, 2, 3, Inf),
          labels = c("0", "0-0.25", "0.25-0.5", "0.5-1", "1-2", "2-3", ">3"))
      
    } else {
      
      if (unit == "absolute") {
        
        if (any(c("Wild zwijn", "Ree") %in% species))
          summaryData2$group <- cut(x = summaryData2$freq, 
            breaks = c(-Inf, 0, 10, 20, 40, 80, Inf),
            labels = c("0", "1-10", "11-20", "21-40", "41-80", ">80")) else
          summaryData2$group <- cut(x = summaryData2$freq, 
            breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),
            labels = c("0", "1-5", "6-10", "11-15", "16-20", ">20"))
        
      } else {
        
        summaryData2$group <- cut(x = summaryData2$freq, 
          breaks = c(-Inf, 0, 0.25, 0.5, 1, 2, 3, Inf),
          labels = c("0", "0-0.25", "0.25-0.5", "0.5-1", "1-2", "2-3", ">3"))
        
      }
      
    }
  }
  
  # remove redundant variables
  summaryData2$AREA <- NULL
  summaryData2$wildsoort <- paste(species, collapse = ", ")
  
  # Add (hoofd)postcode
  if (regionLevel == "communes") {
    
    gemeenteData <- loadGemeentes()
    
    summaryData2$postcode <- gemeenteData$Postcode[match(summaryData2$NISCODE, gemeenteData$NIS.code)]
    names(summaryData2)[names(summaryData2) == "NISCODE"] <- "niscode"
    
    # re-arrange columns 
    summaryData2 <- summaryData2[c(c("locatie", "niscode", "postcode"), 
        setdiff(names(summaryData2), c("locatie", "niscode", "postcode")))]
    
    
  }
  
#    return(summaryData2)
  return(list(data = summaryData2, stats = statsDf))
  
  
}




#' Create map for Flanders - color by incidence
#' @inheritParams createSpaceData
#' @param summaryData data.frame, as returned by \code{\link{createSpaceData}}
#' @param colorScheme character vector, specifies the color palette for the different groups
#' in the summary data
#' @param legend character, legend placement; default is "none", no legend
#' @param addGlobe boolean, whether to add world map to background; default is FALSE 
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addPolygons addPolylines colorFactor addLegend addProviderTiles
#' @export
mapFlanders <- function(
  regionLevel = c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes", "utm5", "WBE_buitengrenzen"),  
  species, year = NA,
  allSpatialData, summaryData, colorScheme,
  legend = "none", addGlobe = FALSE) {
  
  
  spatialData <- filterSpatial(allSpatialData = allSpatialData, 
    species = species, regionLevel = regionLevel, year = year, 
    locaties = summaryData$locatie)
  
  if (any(!summaryData$locatie %in% spatialData$NAAM))
    stop("De geo-data kan niet gematcht worden aan de shape data.")
  
  if (regionLevel == "WBE_buitengrenzen") {
    
    jachtData <- filterSpatial(allSpatialData = allSpatialData, species = species,
      regionLevel = "WBE", year = year, locaties = summaryData$locatie)
    
    spatialData@data$NAAM <- NA
    
    if (!is.null(jachtData)) {
      # Retain only 'aangesloten' #327
      jachtData <- subset(jachtData, WBELID == "aangesloten")
      jachtData@data$NAAM <- paste0("Jachtterrein (", jachtData@data$WBELID, ")")
      jachtData@data$WBE_NR <- jachtData@data$WBE_NR_wbe
      jachtData@data <- jachtData@data[, c("WBE_NR", "NAAM", "AREA")]
      
      spatialData <- rbind(spatialData, jachtData)
    }
    
    valuesPalette <- unique(spatialData@data$NAAM)
    
  } else {
    
    valuesPalette <- summaryData[match(spatialData$NAAM, summaryData$locatie), "group"]
    
  }
  
  
  palette <- colorFactor(palette = colorScheme, levels = levels(summaryData$group))
  
  myMap <- leaflet(spatialData) %>%
    
    addPolygons(
      weight = 1, 
      color = "gray",
      fillColor = ~ palette(valuesPalette),
      fillOpacity = 0.8,
      layerId = spatialData$NAAM,
      group = "region"
    ) 
  
  # Add legend
  if (legend != "none") { 
    
    myMap <- addLegend(
      map = myMap,
      position = legend,
      pal = palette, 
      values = if (regionLevel == "WBE_buitengrenzen")
        valuesPalette[!is.na(valuesPalette)] else 
        valuesPalette,
      opacity = 0.8,
      title = "Legende",
      layerId = "legend"
    )
    
  }
  
  
  # Add black borders
  if (regionLevel %in% c("communes", "fbz_gemeentes", "utm5")) {
    
    borderRegion <- switch(regionLevel,
      "communes" = "provinces",
      "fbz_gemeentes" = "faunabeheerzones",
      "utm5" = "provinces"
    )  
    
    myMap <- addPolylines(map = myMap,
      data = filterSpatial(allSpatialData = allSpatialData, species = species,
        regionLevel = borderRegion, year = year, locaties = summaryData$locatie), 
      color = "black", 
      weight = 3,
      opacity = 0.8,
      group = "borderRegion"
    )
    
  }
  
  if (addGlobe) {
    
    myMap <- addProviderTiles(myMap, "OpenStreetMap.HOT")
    
  }
  
  
  myMap
  
}



#' Shiny module for creating the plot \code{\link{mapFlanders}} - server side
#' @param id character, unique identifier for the module
#' @param defaultYear numeric, default year
#' @param species character, species for which to show the graphs
#' @param currentWbe numeric, KBO number; default value is NULL
#' @param hideGlobeDefault boolean, whether the globe is shown by default 
#' when the map is first created; default value is TRUE
#' @param type character, defines the layout depending on which page it is shown;
#' should be one of \code{c("grofwild", "wildschade", "wbe")}
#' @param geoData SpatialPolygonsDataFrame with geographical data
#' @param biotoopData data.frame, with background biotoop data for selected region level;
#' default value is NULL
#' @param allSpatialData list with SpatialPolygonsDataFrame 
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom ggplot2 fortify
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @export
mapFlandersServer <- function(id, uiText, defaultYear, species, currentWbe = reactive(NULL),
  hideGlobeDefault = TRUE, type = c("grofwild", "wildschade", "wbe"),
  geoData, biotoopData = NULL, allSpatialData) {
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      regio <- NULL
      
      ns <- session$ns
      
      results <- reactiveValues(
        year_value = defaultYear
      )
      
      
      # Minimum year
      results$minYear <- reactive({
          
          req(nrow(geoData()) > 0)
          
          if ((type == "grofwild" && input$regionLevel %in% c("faunabeheerzones", "fbz_gemeentes", "utm5")) | type == "wbe")
            2014 else
            min(geoData()$afschotjaar)          
          
        })
      
      
      # Data dependent input #
      # -------------------- #
      
      ## Region level
      results$regionLevel <- reactive({
          
          if (!is.null(currentWbe()))
            "WBE_buitengrenzen" else
            req(input$regionLevel)
          
        })
      
      
      results$regionLevelName <- reactive({
          
          req(results$regionLevel())
          
          switch(results$regionLevel(),
            "flanders" = "Vlaanderen",
            "provinces" = "Provincie",
            "faunabeheerzones" = "Faunabeheerzones",
            "communes" = "Gemeente",
            "fbz_gemeentes" = "Gemeente per faunabeheerzone",
            "utm5" = "5x5 UTM",
            "WBE_buitengrenzen" = unique(geoData()$WBE_Naam_Toek[match(geoData()$PartijNummer, currentWbe())]))
          
        })
      
      observeEvent(input$regionLevel, {
          
          updateCheckboxInput(session = session, inputId = "combinatie",
            label = paste0("Combineer alle geselecteerde regio's (grafiek: Evolutie gerapporteerd afschot ", 
              results$regionLevelName(), ")"))
          
        })  
      
      
      ## Geselecteerd Jaar (kaart)
      # freeze value - when input$regionLevel changes
      observeEvent(input$regionLevel, {
          
          req(input$year)
          
          if (results$year_value != input$year)
            results$year_value <- input$year
          
        })
      
      output$year <- renderUI({
          
          req(geoData())
          
          div(class = "sliderBlank", 
            sliderInput(inputId = ns("year"), 
              label = if (type == "wbe") "Geselecteerd Jaar" else "Geselecteerd Jaar (kaart)",
              min = results$minYear(),
              max = max(geoData()$afschotjaar),
              value = results$year_value,
              sep = "", step = 1))
          
        })
      
      
      ## Periode (grafiek)
      # freeze value - when input$regionLevel changes
      observeEvent(input$regionLevel, {
          
          if (is.null(input$period)) {
            results$period_value <- c(results$minYear(), defaultYear)
          } else {
            results$period_value <- input$period
          }
          
        })
      # Update period if species changes
      observeEvent(species(), {
          updateSliderInput(session = session, inputId = "period", 
            value = c(results$minYear(), defaultYear))
        })
      
      output$period <- renderUI({
          
          req(nrow(geoData()) > 0)
          
          # initialize
          if (is.null(results$period_value))
            results$period_value <- c(results$minYear(), defaultYear)
          
          sliderInput(inputId = ns("period"), 
            label = if (type == "wbe") "Periode" else "Periode (grafiek)", 
            value = results$period_value,
            min = results$minYear(),
            max = max(geoData()$afschotjaar),
            step = 1,
            sep = "")
          
        })
      
      
      ## Regio's
      # freeze value - when species() changes
      observe({
          
          results$region_value <- if (is.null(input$region)) {
              if (req(input$regionLevel) == "flanders")
                spatialData()$NAAM[1] else
                NULL
            } else {
              input$region
            }
          
        })
      
      spatialData <- reactive({
          
          req(allSpatialData)
          
          filterSpatial(
            allSpatialData = allSpatialData, 
            species = req(!is.null(species())), 
            regionLevel = results$regionLevel(), 
            year = req(input$year),
            locaties = if (!is.null(currentWbe())) currentWbe()
          )
          
        })
      
      output$region <- renderUI({
          
          selectInput(inputId = ns("region"), label = "Regio('s)",
            choices = sort(unique(spatialData()$NAAM)),
            selected = results$region_value, multiple = TRUE)
          
        })
      
      
      
      
      ## Map for Flanders ##
      ## ---------------- ##
      
      # Unit text
      results$unitText <- reactive({
          
          paste0(
            if (type == "wildschade") "aantal schademeldingen" else "afschot",
            if (type != "wbe") switch(req(input$unit),
                absolute = "",
                relative = "/100ha",
                relativeDekking = "/100ha bos & natuur"
              )  
          )
          
        })
      
      # Title for the map
      output$mapFlandersTitle <- renderUI({
          
          nSpecies <- length(species())
          
          if (type == "wbe") {
            
            myTitle <- paste("WBE grenzen en jachtterreinen in", input$year)
            
            tagList(
              h3(myTitle),
              tags$p(HTML(uiText[uiText$plotFunction == "mapFlandersUI", strsplit(id, "_")[[1]][1]]))
            )
            
          } else {
            
            myTitle <- paste("Gerapporteerd", results$unitText(), 
              "voor", if (nSpecies > 1)
                  paste(paste(tolower(species())[1:nSpecies-1], collapse = ", "), "en", tolower(species()[nSpecies])) else 
                  tolower(species()),
              "in", input$year)
          
            h3(myTitle)
            
          }
          
        })  
      
      output$mapFlandersDescription <- renderUI({
          
          tags$p(HTML(uiText[uiText$plotFunction == "mapFlandersUI", strsplit(id, "_")[[1]][1]]))
          
        })
      
      
      # Create data for map, summary of ecological data, given year, species and regionLevel
      results$summarySpaceData <- reactive({
          
          validate(need(input$year, "Gelieve jaar te selecteren"))
          
          createSpaceData(
            data = geoData(), 
            allSpatialData = allSpatialData,
            biotoopData = if (is.list(biotoopData))
              biotoopData[[results$regionLevel()]] else
              biotoopData,
            year = input$year,
            species = species(),
            regionLevel = results$regionLevel(),
            unit = if (type == "wbe") "region" else input$unit,
            sourceIndicator = input$bronMap
          )
          
        })
      
      
      # Define text to be shown in the pop-ups
      results$textPopup <- reactive({
          
          if (results$regionLevel() == "WBE_buitengrenzen")
            return(NULL)
          
          validate(need(results$summarySpaceData()$data, "Geen data beschikbaar"))
          
          regionNames <- results$summarySpaceData()$data$locatie
          titleText <- paste("Gerapporteerd", results$unitText(), "in", input$year[1])
          
          textPopup <- paste0("<h4>", regionNames, "</h4>",  
            "<strong>", titleText, "</strong>: ", 
            round(results$summarySpaceData()$data$freq, 2)
          )
          
          
          return(textPopup)
          
        })
      
      
      # Define colors for the polygons
      results$colorScheme <- reactive({
          
          # Might give warnings if n < 3
          if (type == "wbe") {
            
            suppressWarnings(RColorBrewer::brewer.pal(
                n = nlevels(results$summarySpaceData()$data$group), name = "YlOrBr"))
            
          } else {
            
            suppressWarnings(c("white", RColorBrewer::brewer.pal(
                  n = nlevels(results$summarySpaceData()$data$group) - 1, name = "YlOrBr")))
            
          }
          
        })			
      
      
      # Send map to the UI
      output$spacePlot <- renderLeaflet({
          
          req(allSpatialData)
          
          validate(need(spatialData(), "Geen data beschikbaar"),
            need(nrow(results$summarySpaceData()$data) > 0, "Geen data beschikbaar"))
          
          mapFlanders(
            regionLevel = results$regionLevel(),
            species = species(), 
            year = input$year,
            allSpatialData = allSpatialData,
            summaryData = results$summarySpaceData()$data,
            colorScheme = results$colorScheme()
          )          
          
        })
      
      
      # Statistics with map
      output$stats <- renderUI({
          
          if (req(input$regionLevel) != "flanders") {
          
            percentage <- round(with(results$summarySpaceData()$stats, nAvailable / nTotal) * 100, 1) 
            
            h5(paste0("Info beschikbaar en weergegeven voor ", percentage, 
                "% van de totale gegevens (", results$summarySpaceData()$stats$nAvailable, "/", 
                results$summarySpaceData()$stats$nTotal, ")" ))
          }
          
        })
      
      
      # Which region(s) are selected?
      observe({
          
          event <- input$spacePlot_shape_click
          
          if (!is.null(event)) {
            
            if (!is.null(event$id)) {
              
              currentSelected <- isolate(input$region)
              
              if (event$id %in% currentSelected) {
                # Remove from list
                
                updateSelectInput(session, inputId = "region", 
                  selected = currentSelected[- which(currentSelected == event$id)])
                
                
              } else {
                # Add to list
                
                updateSelectInput(session, inputId = "region", 
                  selected = c(currentSelected, event$id))
                
              }
              
            }
            
          }
          
        })
      
      
      # Center view for WBE
      observe({
          
          req(currentWbe())
          
          selectedPolygons <- subset(spatialData(), 
            spatialData()$NAAM %in% currentWbe())
          
          coordData <- ggplot2::fortify(selectedPolygons)
          centerView <- c(range(coordData$long), range(coordData$lat))
          
          leafletProxy("spacePlot", data = spatialData()) %>%
            
            fitBounds(lng1 = centerView[1], lng2 = centerView[2],
              lat1 = centerView[3], lat2 = centerView[4])
          
        })
      
      
      # Plot thick border for selected regions
      observe({
          
          if (!is.null(input$region)) {
            
            validate(need(spatialData(), "Geen data beschikbaar"))
            
            selectedPolygons <- subset(spatialData(), 
              spatialData()$NAAM %in% input$region)
            
            leafletProxy("spacePlot", data = spatialData()) %>%
              
              clearGroup(group = "regionLines") %>%
              
              addPolylines(data = selectedPolygons, color = "gray", weight = 5,
                group = "regionLines")
            
          }
          
        })
      
      
      # Add world map
      observe({
          
          validate(need(spatialData(), "Geen data beschikbaar"),
            # update when new plot proxy data
            need(nrow(results$summarySpaceData()$data) > 0, "Geen data beschikbaar"))
          
          proxy <- leafletProxy("spacePlot", data = spatialData())
          
          if (!is.null(input$globe) & !is.null(proxy)){
            
            if (input$globe %% 2 == as.numeric(hideGlobeDefault)){
              
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
          
          validate(need(nrow(results$summarySpaceData()$data) > 0, "Geen data beschikbaar"))
          
          req(input$legend)
          
          proxy <- leafletProxy("spacePlot", data = spatialData())
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            palette <- colorFactor(palette = results$colorScheme(), 
              levels = levels(results$summarySpaceData()$data$group))
            
            if (type == "wbe")
              valuesPalette <- results$summarySpaceData()$data$group else 
              valuesPalette <- results$summarySpaceData()$data[
                match(spatialData()$NAAM, results$summarySpaceData()$data$locatie),
                "group"]
            
            if (!all(is.na(valuesPalette)))
              proxy %>% addLegend(
                position = input$legend,
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
          
          validate(need(spatialData(), "Geen data beschikbaar"),
            need(results$textPopup(), "Geen data beschikbaar"))
          
          currentMap <- leafletProxy("spacePlot", data = spatialData()) 
          currentMap %>% clearPopups()
          
          event <- input$spacePlot_shape_click
          
          if (!is.null(event)) {
            
            if (!is.null(event$id)) {
              
              if (event$id %in% results$summarySpaceData()$data$locatie) {
                
                textSelected <- results$textPopup()[
                  results$summarySpaceData()$data$locatie == event$id]
                
                isolate({
                    
                    currentMap %>% 
                      addPopups(event$lng, event$lat, popup = textSelected)
                    
                  }) 
                
              }
              
            }
            
          }
          
        })
      
      
      
      
      # Create final map (for download)
      results$finalMap <- reactive({
          
          validate(need(results$summarySpaceData()$data, "Geen data beschikbaar"))
          
          newMap <- mapFlanders(
            regionLevel = results$regionLevel(), 
            species = species(),
            allSpatialData = allSpatialData,
            summaryData = results$summarySpaceData()$data,
            colorScheme = results$colorScheme(),
            legend = input$legend,
            addGlobe = input$globe %% 2 == as.numeric(hideGlobeDefault)
          )
          
          # save the zoom level and centering to the map object
          newMap <- newMap %>% setView(
            lng = input$spacePlot_center$lng,
            lat = input$spacePlot_center$lat,
            zoom = input$spacePlot_zoom
          )
          
          tmpFile <- tempfile(fileext = ".html")
          
          # write map to temp .html file
          htmlwidgets::saveWidget(newMap, file = tmpFile, selfcontained = FALSE)
          
          # output is path to temp .html file containing map
          tmpFile
          
        }) 
      
      
      # Download the map
      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            year = input$year, 
            content = "kaart", fileExt = "png"),
        content = function(file) {
          
          # convert temp .html file into .png for download
          webshot::webshot(url = results$finalMap(), file = file,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
        }
      )
      
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = paste(species(), collapse = "-"),
            year = input$year, 
            content = "kaartData", fileExt = "csv"),
        content = function(file) {
          
          myData <- results$summarySpaceData()$data
          # change variable names
          names(myData)[names(myData) == "freq"] <- results$unitText()
          names(myData)[names(myData) == "group"] <- "groep"
          
          ## write data to exported file
          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
      
      
      
      
      ## Time plot for Flanders (reference) ##
      ## ---------------------------------- ##
      
      results$timeDataFlanders <- reactive({
          
          validate(need(input$period, "Gelieve periode te selecteren"))
          
          ## Get data for Flanders
          createTrendData(
            data = geoData(),
            allSpatialData = allSpatialData,
            timeRange = input$period,
            species = species(),
            regionLevel = "flanders",
            unit = input$unit
          )
          
        })
      
      callModule(module = optionsModuleServer, id = "timePlotFlanders", 
        data = results$timeDataFlanders)
      callModule(module = plotModuleServer, id = "timePlotFlanders",
        plotFunction = "trendYearFlanders", 
        data = results$timeDataFlanders,
        timeRange = reactive(input$period),
        unit = reactive(input$unit),
        isSchade = (type == "wildschade")
      )
      
      
      
      
      ## Time plot for selected region ##
      ## ----------------------------- ##
      
      # Create data for map, time plot
      results$timeData <- reactive({
          
          validate(need(input$period, "Gelieve periode te selecteren"))
          
          createTrendData(
            data = geoData(),
            allSpatialData = allSpatialData,
            biotoopData = biotoopData,
            timeRange = input$period,
            species = species(),
            regionLevel = results$regionLevel(),
            unit = input$unit
          )
          
        })
      
      # Title for selected region level
      output$timeTitle <- renderUI({
          
          h3("Evolutie", 
            if (type == "wildschade") "schademeldingen" else "gerapporteerd afschot", 
            if (type != "wbe") tags$br(),
            results$regionLevelName())
          
        })
      
      
      callModule(module = optionsModuleServer, id = "timePlot", 
        data = results$timeData)
      callModule(module = plotModuleServer, id = "timePlot",
        plotFunction = "trendYearRegion", 
        data = results$timeData,
        locaties = reactive({
            if (!is.null(currentWbe()))
              results$regionLevelName() else
              input$region
          }),
        timeRange = reactive(input$period),
        unit = reactive(input$unit),
        combinatie = reactive(input$combinatie),
        isSchade = (type == "wildschade")
      )
      
      
      ## Biotoop plot for selected region ##
      ## -------------------------------- ##
      
      
      # Title for selected region level
      output$biotoopTitle <- renderUI({
          
          req(type != "empty")
          
          uiText <- uiText[uiText$plotFunction == "barBiotoop", ]
          
          tagList(
            h3(uiText$title, tags$br(), 
              req(results$regionLevelName()), 
              if (!is.null(input$year)) paste("in", input$year))
          )
          
        })
      
      output$biotoopPlotText <- renderUI({
          
          tags$p(HTML(uiText[uiText$plotFunction == "barBiotoop", strsplit(id, "_")[[1]][1]]))
          
        })
      
      callModule(module = optionsModuleServer, id = "biotoopPlot", 
        data = reactive({
            if (!is.null(currentWbe()))
              subset(biotoopData, regio %in% currentWbe()) else
              biotoopData[[req(input$regionLevel)]]
          })
      )
      callModule(module = plotModuleServer, id = "biotoopPlot",
        plotFunction = "barBiotoop", 
        data = reactive({
            if (!is.null(currentWbe()))
              subset(biotoopData, year == input$year & regio %in% currentWbe()) else {
              validate(need(input$region, "Gelieve regio('s) te selecteren"))
              subset(biotoopData[[req(input$regionLevel)]], regio %in% input$region)
            }
          })
      )
      
    })  
}



#' Shiny module for creating the plot \code{\link{mapFlanders}} - UI side
#' @inheritParams mapFlandersServer 
#' @param showRegion boolean, whether to show choices for regionLevel and selected region(s)
#' @param showSource boolean, whether to show choices for sources
#' @param showCombine boolean, whether to show option to combine selected regions
#' @param unitChoices named character vector, choices for unit option;
#' default is \code{c("Aantal" = "absolute", "Aantal/100ha" = "relative")}
#' @param plotDetails character vector, detail plots to be shown below the map;
#' should be subset of \code{c("flanders", "region", "biotoop")}
#' @return UI object
#' 
#' @author mvarewyck
#' @import shiny
#' @export
mapFlandersUI <- function(id, showRegion = TRUE, showSource = FALSE, 
  showCombine = TRUE, type = c("grofwild", "wildschade", "wbe"),
  unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative", "Aantal/100ha bos & natuur" = "relativeDekking"),
  plotDetails = c("flanders", "region")) {
  
  ns <- NS(id)
  type <- match.arg(type)
  
  # Map with according line plot
  
  tags$div(class = "container",
    
    if (id != "schade")
      h2("Landkaart"),
    
    uiOutput(ns("mapFlandersDescription")),
    
    ## countMap: all species
    wellPanel(
      if (showRegion)
        fixedRow(
          column(4, selectInput(inputId = ns("regionLevel"), label = "Regio-schaal",
              choices = c(
                "Vlaanderen" = "flanders",
                "Provincie" = "provinces", 
                "Faunabeheerzones" = "faunabeheerzones",
                "Gemeente" = "communes",
                "Gemeente per Faunabeheerzone" = "fbz_gemeentes",
                "5x5 UTM" = "utm5"
              ),
              selected = "communes")),
          column(8, uiOutput(ns("region")))      
        ),
      
      fixedRow(
        column(6, uiOutput(ns("year"))),
        column(6, if (type == "wbe") 
              selectInput(inputId = ns("legend"), label = "Legende",
                choices = c(
                  "Bovenaan rechts" = "topright",
                  "Onderaan rechts" = "bottomright",
                  "Bovenaan links" = "topleft",
                  "Onderaan links" = "bottomleft",
                  "<geen>" = "none")) else 
              uiOutput(ns("period")))
      ),
      
      if (type != "wbe")
        fixedRow(
          column(12/(2+showSource),
            selectInput(inputId = ns("legend"), label = "Legende (kaart)",
              choices = c(
                "Bovenaan rechts" = "topright",
                "Onderaan rechts" = "bottomright",
                "Bovenaan links" = "topleft",
                "Onderaan links" = "bottomleft",
                "<geen>" = "none"))
          ),
          column(12/(2+showSource),
            selectInput(inputId = ns("unit"), label = "Eenheid",
              choices = unitChoices)
          ),
          if (showSource)
            column(4, 
              selectInput(inputId = ns("bronMap"),
                label = "Data bron",
                choices = names(loadMetaSchade()$sources),
                multiple = TRUE)
            )
        ),
      
      if (showCombine)
        checkboxInput(inputId = ns("combinatie"), 
          label = "Combineer alle geselecteerde regio's (grafiek: Evolutie gerapporteerd afschot Gemeente)"),
      actionLink(inputId = ns("globe"), label = "Voeg landkaart toe",
        icon = icon("globe"))
    
    ),
    
    fixedRow(
      column(if ("biotoop" %in% plotDetails) 6 else 12,
        uiOutput(ns("mapFlandersTitle")),
        withSpinner(leafletOutput(ns("spacePlot"))),
        tags$div(align = "center", uiOutput(ns("stats"))),
        tags$br(),
        downloadButton(ns("download"), label = "Download figuur", class = "downloadButton"),
        downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton")
      ),
      
      if ("biotoop" %in% plotDetails)
        column(6, 
          uiOutput(ns("biotoopTitle")),
          uiOutput(ns("biotoopPlotText")),
          plotModuleUI(id = ns("biotoopPlot"), height = "400px"),
          optionsModuleUI(id = ns("biotoopPlot"), exportData = TRUE,
            doWellPanel = FALSE)
        )
    ),
    
    
    fixedRow(
      if ("flanders" %in% plotDetails) 
        column(6,
          h3("Evolutie", 
            if (type == "wildschade") "schademeldingen" else "gerapporteerd afschot", 
            tags$br(), "Vlaanderen"),
          plotModuleUI(id = ns("timePlotFlanders"), height = "400px"),
          optionsModuleUI(id = ns("timePlotFlanders"), exportData = TRUE,
            doWellPanel = FALSE)
        ),
      if ("region" %in% plotDetails)
        column(6,
          uiOutput(ns("timeTitle")),
          plotModuleUI(id = ns("timePlot"), height = "400px"),
          optionsModuleUI(id = ns("timePlot"), exportData = TRUE,
            doWellPanel = FALSE)
        )      
    )
    
    ,tags$hr()
  
  )
  
}
