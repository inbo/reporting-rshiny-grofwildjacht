# Functions to plot the interactive map for flanders
# 
# Author: mvarewyck
###############################################################################




#' Get display name for region level
#' @param level character, regionLevel
#' @return character, name for region level to be displayed
#' 
#' @author mvarewyck
#' @export
getRegionLevel <- function(level) {
  
  switch(level,
    "flanders" = "Vlaanderen",
    "provinces" = "Provincie",
    "faunabeheerzones" = "Faunabeheerzones",
    "communes" = "Gemeente",
    "communes_wolf" = "Gemeente",
    "municipalities" = "Gemeente",
    "fbz_gemeentes" = "Gemeente per faunabeheerzone",
    "utm5" = "5x5 UTM",
    "pixels" = "2x2 UTM"
  )
  
}

#' Calculate window range for centering a map 
#' 
#' @param sf_object object of class sf
#' @return vector with x_min, x_max, y_min and y_max
#' 
#' @author mvarewyck
#' @export
getCenterView <- function(sf_object) {
  
  coordData <- sf::st_bbox(sf_object)
  toReturn <- coordData[c(1, 3, 2, 4)]
  names(toReturn) <- NULL
  
  toReturn
  
}

outputFunction <- function(type, specie = NULL) {
  if (type == "dash") {
    "F17_1"
  } else if ( type != "schade") {
    "mapFlandersUI"
  } else if ( ! is.null(specie) && specie == "Wolf" && type == "schade" ) {
    "mapSchadeWolvesUI"
  } else {
    paste0("mapFlandersUI-", type)
  }
}

#' Create summary data of geographical data for selected year, species and region level
#' @param data data.frame, geographical data
#' @param allSpatialData list of sp, spatial data for all spatial levels
#' @param biotoopData data.frame, background data for the WBE, as read from \code{loadHabitats}
#' @param selectedYear integer, year of interest
#' @param regionLevel character, regional level of interest should be one of 
#' \code{c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes", "utm5" )}
#' @param unit character, whether absolute frequencies, relative frequencies (aantal/100ha),
#' absolute cases, or relative bos freq (aantal/100ha bos & natuur); if 'region'
#' the legend shows different types of regions for WBE
#' @param countVariable character, column name in \code{data} that contains counts;
#' if NULL then each row in the data contains 1 count
#' @param groupVariable character, column name of frequency to calculate
#' @inheritParams filterDataSource
#' @inheritParams createShapeData
#' @inheritParams reportingGrofwild-common-args
#' @return a list with two items: data - a data.frame with the summary data; stats - a data.frame with the summary statistics
#' @author mvarewyck
#' @importFrom reshape2 dcast
#' @importFrom sf st_drop_geometry
#' @importFrom stats as.formula
#' @export
createSpaceData <- function(data, allSpatialData, biotoopData, 
  selectedYear, species, regionLevel,
  unit = c("absolute", "relative", "absoluteCases", "relativeDekking", "region"), 
  sourceIndicator = NULL, countVariable = NULL, groupVariable = NULL) {
  
  
  # To prevent warnings with R CMD check
  afschotjaar <- NULL
  wildsoort <- NULL
  
  unit <- match.arg(unit)
  
  # Select correct spatial data
  spatialData <- filterSpatial(allSpatialData = allSpatialData, 
    species = species, regionLevel = regionLevel, year = selectedYear)
  
  if (is.null(groupVariable))
    groupVariable <- "dataSource"
  
  if (is.null(spatialData))
    return(NULL) else
    spatialData <- sf::st_drop_geometry(spatialData)
  
  # Framework for summary data
  fullData <- if (regionLevel %in% c("communes", "communes_wolf")) {

      spatialData[, c("NAAM", "AREA", "NISCODE")]
      
    } else if (regionLevel %in% "fbz_gemeentes") {
      
      spatialData[, c("NAAM", "AREA", "provincie")]
      
    } else if (regionLevel == "WBE_buitengrenzen") {
      
      tmpData <- spatialData[, c("NAAM", "AREA")]        
      tmpData[tmpData$NAAM %in% unique(data$PartijNummer), , drop = FALSE]
      
    } else {
      
      spatialData[, c("NAAM", "AREA")]
      
    }
  
  # Bind area bos&natuur
  if (unit %in% c("relative", "relativeDekking")) {
    areaVariable <- if (unit == "relative")
        "Area_km2" else
        "Area_hab_km2_bos"
    if (regionLevel == "WBE_buitengrenzen")
      tmpData <- biotoopData[biotoopData$year %in% selectedYear, c("regio", areaVariable)] else 
      tmpData <- biotoopData[, c("regio", areaVariable)]
    
    colnames(tmpData) <- c("NAAM", "AREA")
    
    if (unit == "relativeDekking")
      # cut-off at 100ha bos&natuur #385
      tmpData$AREA[tmpData$AREA < 1] <- NA
    
    fullData$AREA <- NULL
    fullData <- merge(fullData, tmpData)
  }
  
  
  if (nrow(fullData) == 0)
    return(NULL)
  
  colnames(fullData)[1] <- "locatie"
  
  
  # Select subset for time & species
  if (length(species) > 1 || species != "")
    plotData <- subset(data, subset = afschotjaar %in% selectedYear & wildsoort %in% species) else if (!is.null(selectedYear))
    plotData <- subset(data, subset = afschotjaar %in% selectedYear) else 
    plotData <- data
  
  
  plotData <- filterDataSource(plotData = plotData, sourceIndicator = sourceIndicator,
    returnStop = "data")
  
  #compute total number of cases to output in stats
  myStats <- list(nTotal = if (is.null(countVariable)) 
        nrow(plotData) else 
        sum(plotData[[countVariable]])
  )
  
  if (nrow(plotData) == 0) {
    
    allData <- fullData 
    allData$freq <- 0
    
  } else {
    
    # Create general plot data names
    plotData$locatie <- switch(regionLevel,
      flanders = "Vlaams Gewest",
      provinces = plotData$provincie, 
      communes = plotData$gemeente_afschot_locatie,
      communes_wolf = plotData$gemeente_afschot_locatie,
      faunabeheerzones = plotData$FaunabeheerZone,
      fbz_gemeentes = plotData$fbz_gemeente,
      utm5 = plotData$UTM5,
      WBE_buitengrenzen = plotData$PartijNummer
    )
    
    # Exclude data with missing locatie
    plotData <- subset(plotData, !is.na(plotData$afschotjaar) &
        !is.na(plotData$locatie) & !plotData$locatie %in% c("", "Onbekend"),
      c("afschotjaar", "locatie", countVariable, if (!is.null(sourceIndicator) & !is.null(groupVariable)) groupVariable)
    )
    
    # Summarize data over afschotjaar/locaties/dataSource
    if (is.null(countVariable)) {
      summaryData <- plyr::count(df = plotData, vars = names(plotData)) 
    } else {
      summaryData <- aggregate(plotData[[countVariable]], 
        by = sapply(names(plotData)[!names(plotData) %in% countVariable], function(x) c(x = plotData[[x]]), simplify = FALSE), sum)
      summaryData$freq <- summaryData$x
      summaryData$x <- NULL
    }
    
    if (!is.null(sourceIndicator)) {
      summaryData <- dcast(summaryData, as.formula(paste("afschotjaar + locatie ~", groupVariable)), value.var = "freq", fun.aggregate = sum)
      summaryData$freq <- apply(summaryData[, -(1:2), drop = FALSE], 1, sum, na.rm = TRUE)      
    }
    
    # Add names & times with 0 observations
    allData <- merge(summaryData, fullData, all = TRUE)
    allData$freq[is.na(allData$freq)] <- 0
    
    if (!is.null(sourceIndicator)) {
      sourceIndicator <- sourceIndicator[sourceIndicator %in% colnames(allData)]
      allData[, sourceIndicator][is.na(allData[, sourceIndicator])] <- 0
    }
    
  }
  
  ## stats
  # compute all cases with full info available
  myStats$nAvailable <- sum(allData$freq)
  myStats$percentage <- myStats$nAvailable / myStats$nTotal * 100
  
  # Remove redundant variables
  allData$afschotjaar <- NULL
  
  
  summaryData2 <- plyr::count(df = allData, 
    vars = names(allData)[!names(allData) %in% "freq"], 
    wt_var = "freq")
  
  
  # unit taken into account
  if (grepl("relative", unit))
    summaryData2$freq <- ifelse(summaryData2$freq == 0, 0, summaryData2$freq/summaryData2$AREA)
  
  
  # Create group variable
  if (unit == "region") {
    
    jachtData <- filterSpatial(allSpatialData = allSpatialData, 
      species = species, regionLevel = "WBE", year = selectedYear)
    
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
  
  # Re-arrange colums
  if (regionLevel %in% c("communes", "communes_wolf")) {
    
    summaryData2 <- summaryData2[c(c("locatie", "NISCODE"), 
        setdiff(names(summaryData2), c("locatie", "NISCODE")))]
    
  }
  
#    return(summaryData2)
  return(list(data = summaryData2, stats = myStats))
  
  
}




#' Create map for Flanders - color by incidence
#' @inheritParams createSpaceData
#' @param borderRegion character, region level for which to draw black borders;
#' if NULL no borders are plotted
#' @param borderLocaties character, locations for which to draw black borders;
#' if NULL default regions are selected
#' @param summaryData data.frame, as returned by \code{\link{createSpaceData}}
#' @param colorScheme character vector, specifies the color palette for the different groups
#' in the summary data; if NULL map is not colored
#' @param legend character, legend placement; default is "none", no legend
#' @param legendText character, legend title; default is 'Legende'
#' @param addGlobe boolean, whether to add world map to background; default is FALSE
#' @param statsMap character, statistical info to add to map
#' @inheritParams filterSpatial
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addPolygons addPolylines colorFactor addLegend addProviderTiles
#' @export
mapFlanders <- function(
  regionLevel = c("flanders", "provinces", "communes", "faunabeheerzones", 
    "fbz_gemeentes", "utm5", "WBE_buitengrenzen", "communes_wolf"),  
  borderRegion = NULL, borderLocaties = NULL,
  species, year = NA,
  allSpatialData, summaryData, colorScheme = NULL,
  legend = "none", legendText = "Legende", addGlobe = FALSE, statsMap = NULL) {
  
  
  spatialData <- filterSpatial(allSpatialData = allSpatialData, 
    species = species, regionLevel = regionLevel, year = year, 
    locaties = summaryData$locatie)
  
  
  palette <- colorFactor(palette = colorScheme, levels = levels(summaryData$group))
  
  if (regionLevel == "WBE_buitengrenzen")
    valuesPalette <- summaryData[spatialData$NAAM %in% summaryData$locatie, "group"] else
    valuesPalette <- summaryData[match(spatialData$NAAM, summaryData$locatie), "group"]
  
  
  if (any(!summaryData$locatie %in% spatialData$NAAM))
    stop("De geo-data kan niet gematcht worden aan de shape data.")
  
  if (regionLevel == "WBE_buitengrenzen") {
    
    jachtData <- filterSpatial(allSpatialData = allSpatialData, species = species,
      regionLevel = "WBE", year = year, locaties = summaryData$locatie)
    
    spatialData$NAAM <- NA
    
    if (!is.null(jachtData)) {
      # Retain only 'aangesloten' #327
      jachtData <- jachtData[jachtData$WBELID == "aangesloten", ]
      
      if (nrow(jachtData) > 0) {
        jachtData$NAAM <- paste0("Jachtterrein (", jachtData$WBELID, ")")
        jachtData$WBE_NR <- jachtData$WBE_NR_wbe
        jachtData <- jachtData[, c("WBE_NR", "NAAM", "AREA")]
        spatialData <- rbind(spatialData, jachtData)
      }
    }
    
    valuesPalette <- unique(spatialData$NAAM)
    
  } else {
    
    valuesPalette <- summaryData[match(spatialData$NAAM, summaryData$locatie), "group"]
    
  }
  
  myMap <- leaflet(
      spatialData,
      options = leafletOptions(
        zoomSnap = 0.25,   # allows zoom steps of 0.25 instead of 1
        zoomDelta = 0.25   # controls zoom increment
      )
    ) %>%
    
    addPolygons(
      weight = 1, 
      color = "gray",
      fillColor = if (is.null(colorScheme)) "white" else ~ palette(valuesPalette),
      fillOpacity = 0.8,
      layerId = spatialData$NAAM,
      group = "region"
    ) 
  
  # Add legend
  if (!is.null(colorScheme) && legend != "none") { 
    
    myMap <- addLegend(
      map = myMap,
      position = legend,
      pal = palette, 
      values = if (regionLevel == "WBE_buitengrenzen")
          valuesPalette[!is.na(valuesPalette)] else 
          valuesPalette,
      na.label = "bos & natuur < 100ha",
      opacity = 0.8,
      title = legendText,
      layerId = "legend"
    )
    
  }
  
  
  # Add black borders
  if (!is.null(borderRegion)) {
    
    myMap <- addPolylines(map = myMap,
      data = filterSpatial(allSpatialData = allSpatialData, species = species,
        regionLevel = borderRegion, year = year, 
        locaties = borderLocaties), 
      color = "black", 
      weight = 3,
      opacity = 0.8,
      group = "borderRegion"
    )
    
  }
  
  if (addGlobe) {
    
    myMap <- addProviderTiles(myMap, "OpenStreetMap.HOT")
    
  }
  
  if (!is.null(statsMap)) {
    myMap <- addControl(myMap, statsMap, position = "bottomleft")
  }
  myMap <- leaflet_bound_flanders(myMap)
  
  myMap
  
}



#' Shiny module for creating the plot \code{\link{mapFlanders}} - server side
#' @param species character, species for which to show the graphs
#' @param currentWbe numeric, KBO number; default value is NULL
#' @param hideGlobeDefault boolean, whether the globe is shown by default 
#' when the map is first created; default value is TRUE
#' @param type character, defines the layout depending on which page it is shown;
#' should be one of \code{c("beheer", "schade", "wbe", "dash")}
#' @param geoData data.frame with geographical data
#' @param biotoopData data.frame, with background biotoop data for selected region level;
#' default value is NULL
#' @param allSpatialData list with sf objects 
#' @inheritParams mapFlandersUI
#' @inheritParams createSpaceData
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @param sourceChoices named character vector, choices for the source
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @import leaflet
#' @importFrom sf st_coordinates
#' @importFrom webshot2 webshot
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr coalesce
#' @export
mapFlandersServer <- function(id, defaultYear, species, currentWbe = reactive(NULL),
  hideGlobeDefault = TRUE, type = c("beheer", "schade", "wbe", "empty", "dash"),
  geoData, biotoopData = NULL, allSpatialData,
  countVariable = NULL,
  sourceChoices = NULL,
  uiText = NULL,
  preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      regio <- NULL
      
      ns <- session$ns
      
      results <- reactiveValues(
        legend = "topright")
      
      
      year <- reactive({
          req(preSelected())
          year <- coalesce(input$year, preSelected()$year(), NA)
          if (all(is.na(year))) NULL else year
        })
      bronMap <- reactive({
          req(preSelected())
          bronMap <- coalesce(input$bronMap, preSelected()$dataSource_schade(), preSelected()$type(), NA)
          if (all(is.na(bronMap))) NULL else bronMap
        })
      unit <- reactive({
          req(preSelected())
          unit <- coalesce(input$unit, preSelected()$unit(), NA)
          if (all(is.na(unit))) NULL else unit
        })
      period <- reactive({
          req(preSelected())
          period <- coalesce(input$period, preSelected()$time(), NA)
          if (all(is.na(period))) NULL else period
        })
      
      # Minimum year
      minYear <- reactive({
          
          req(nrow(geoData()) > 0)
          
          regionLev <- coalesce(input$regionLevel, preSelected()$regionLevel(), NA)
          
          if (type %in% c("beheer", "dash") && req(regionLev) %in% c("faunabeheerzones", "fbz_gemeentes", "utm5") | type == "wbe")
            2014 else 
            min(geoData()$afschotjaar)
        })
      
      output$description <- renderUI({
        description <- getOutputDescription(
          output = outputFunction(type, species()), 
          uiText = uiText, 
          context = if (type == "wbe") "wbe" else "description"
        )
        tags$div(class = "larger-description", HTML(description))
      })
      
      
      # Data dependent input #
      # -------------------- #
      
      ## For verspreiding
      regionLevel <- reactive({
          req(preSelected())
          if (type == "dash")
            preSelected()$regionLevel()
          else NULL
        })
      output$borderRegion <- renderUI({
          
          tmpSpatial <- filterSpatial(
            allSpatialData = allSpatialData, 
            species = if (!is.null(species())) species(), 
            regionLevel = regionLevel(), 
            year = req(year())
          )
          
          choices <- sort(unique(tmpSpatial$NAAM))
          
          selectInput(inputId = ns("borderRegion"), label = "Regio('s)",
            choices = choices, 
            selected = if (regionLevel() == "flanders") choices[1] else NULL, 
            multiple = TRUE)
          
        })
        locaties <- reactive({
            req(preSelected())
            if (type == "dash")
              preSelected()$region()
            else NULL
        })
      
      ## Region level
      regionLevelLocal <- reactive({
          
          if (!is.null(regionLevel()))
            validate(need(locaties(), "Gelieve regio('s) te selecteren"))
          
          regionLev <- coalesce(input$regionLevel, preSelected()$regionLevel(), NA)
          if (all(is.na(regionLev))) regionLev <- NULL
          
          if (!is.null(currentWbe()))
            "WBE_buitengrenzen" else 
            req(regionLev)
          
        })
      
      regionLevelName <- reactive({
          
          if (req(regionLevelLocal()) == "WBE_buitengrenzen")
            unique(geoData()$WBE_Naam_Toek[match(geoData()$PartijNummer, currentWbe())]) else
            getRegionLevel(regionLevelLocal())
          
        })
      
      
      ## Geselecteerd Jaar (kaart)
      # freeze value
      observe({
          
          if (is.null(year())) {
            results$year_value <- defaultYear
          } else {
            results$year_value <- year()
          }
          
        })
      
      output$year <- renderUI({
          
          req(nrow(geoData()) > 0)
          
          div(class = "sliderBlank", 
            sliderInput(inputId = ns("year"), 
              label = if (type %in% c("wbe", "empty") | !is.null(locaties())) 
                  "Geselecteerd Jaar" else 
                  "Geselecteerd Jaar (kaart)",
              min = minYear(),
              max = max(geoData()$afschotjaar),
              value = isolate(results$year_value),
              sep = "", step = 1))
          
        })
      
      
      ## Periode (grafiek)
      # freeze value
      observe({
          
          if (is.null(period())) {
            results$period_value <- c(minYear(), defaultYear)
          } else {
            results$period_value <- period()
          }
          
        })
      
      output$period <- renderUI({
          
          req(nrow(geoData()) > 0)
          
          sliderInput(inputId = ns("period"), 
            label = if (type == "wbe") "Periode" else "Periode (grafiek)", 
            value = isolate(results$period_value),
            min = minYear(),
            max = max(geoData()$afschotjaar),
            step = 1,
            sep = "")
          
        })
      
      
      ## Regio's
      # freeze value - when species() changes
      observe({
          
          req(preSelected())
          region <- coalesce(input$region, preSelected()$region(), NA)
          
          results$region_value <- if (all(is.na(region))) {
              if (req(regionLevelLocal()) == "flanders")
                spatialData()$NAAM[1] else if (!is.null(currentWbe()))
                currentWbe() else if (!is.null(locaties()))
                locaties() else
                NULL
            } else {
              region
            }
          
        })
      
      spatialData <- reactive({
          
          req(allSpatialData)
          
          filterSpatial(
            allSpatialData = allSpatialData, 
            species = if (!is.null(species())) species(), 
            regionLevel = regionLevelLocal(), 
            year = req(year()),
            locaties = if (!is.null(currentWbe())) currentWbe()
          )
          
        })
      
      output$region <- renderUI({
          
          if (is.null(locaties())) {
            
            choices <- sort(unique(spatialData()$NAAM))
            selectInput(inputId = ns("region"), label = "Regio('s)",
              choices = choices,
              selected = results$region_value, multiple = TRUE)
            
          }
        })
      
      
      
      
      ## Map for Flanders ##
      ## ---------------- ##
      
      # Unit text
      unitText <- reactive({
          
          paste0(
            if (type == "schade") 
                ifelse(species() == "Wolf", "aantal bevestigde schadegevallen", "aantal schadegevallen") else if (!is.null(countVariable)) 
                countVariable else 
                "afschot",
            if (!is.null(unit()) && !type %in% c("wbe", "empty")) 
              switch(unit(),
                absolute = "",
                relative = "/100ha",
                relativeDekking = "/100ha bos & natuur"
              )  
          )
          
        })
      
      # Restrict bron
      # Not via updateSelectInput: this is slower, multiple rendering of the plot
      output$bronMap <- renderUI({
          
          req(year())
          
          newChoices <- unique(geoData()$dataSource[geoData()$afschotjaar == year()])
          isolate(previousChoice <- if (is.null(bronMap())) newChoices else bronMap())
          
          selectInput(inputId = ns("bronMap"),
            label = "Databron(nen)",
            choices = sourceChoices[sourceChoices %in% newChoices], 
            selected = previousChoice[previousChoice %in% newChoices],
            multiple = TRUE)
          
        })
      
      
      # Title for the map
      output$mapTitle <- renderUI({
          
          if (type == "empty")
            return(NULL)
          
          nSpecies <- length(species())
          
          if (type == "wbe") {
            
            myTitle <- paste("WBE grenzen en jachtterreinen in", year())
            
          } else {
            
            myTitle <- paste(if (type != "dash") paste("Gerapporteerd", unitText()) else "Verspreiding", 
              "van", if (nSpecies > 1)
                  paste(toString(tolower(species())[1:nSpecies-1]), "en", tolower(species()[nSpecies])) else 
                  tolower(species()),
              "in", year())
            
          }
          
          h3(myTitle)
          
        })     
      
      
      # Create data for map, summary of ecological data, given year, species and regionLevel
      summarySpaceData <- reactive({
          
          if (type != "empty")
            validate(need(year(), "Gelieve jaar te selecteren"))
          if (!is.null(sourceChoices))
            validate(need(bronMap(), "Gelieve bron te selecteren"))
          
          req(!is.null(regionLevelLocal()))
          
          createSpaceData(
            data = geoData(), 
            allSpatialData = allSpatialData,
            biotoopData = if (is.list(biotoopData))
                biotoopData[[regionLevelLocal()]] else
                biotoopData,
            selectedYear = year(),
            species = species(),
            regionLevel = regionLevelLocal(),
            unit = if (type == "wbe") "region" else if (type != "empty") unit(),
            sourceIndicator = bronMap(),
            countVariable = countVariable,
            groupVariable = input$groupVariable
          )
          
        })
      
      
      # Define text to be shown in the pop-ups
      textPopup <- reactive({
          
          if (type %in% c("wbe", "empty"))
            return(NULL)
          
          validate(need(summarySpaceData()$data, "Geen data beschikbaar"))
          
          regionNames <- summarySpaceData()$data$locatie
          titleText <- paste(
            if (type != "dash" && species() != "Wolf"){
              "Gerapporteerd"
            } else if (species() == "Wolf") {
              "Bevestigd"
            }, 
            paste0(
              if (type == "schade") {
                "aantal schadegevallen"
              } else if (!is.null(countVariable)) {
                countVariable
              } else {
                "afschot"
              },
              if (!is.null(unit()) && type %in% c("beheer")) 
                switch(unit(),
                  absolute = "",
                  relative = "/100ha",
                  relativeDekking = "/100ha bos & natuur"
                )  
            ), "in", year()[1])
          
          tmpData <- summarySpaceData()$data
          tmpData[is.na(tmpData)] <- 0
          
          contentText <- if (!is.null(bronMap())) {
              if (is.null(input$groupVariable)) {
                availableGroups <- bronMap()[bronMap() %in% colnames(summarySpaceData()$data)]
                names(availableGroups) <- sapply(availableGroups, function(x) strsplit(x, split = "\\.")[[1]][1])
              } else {
                availableGroups <- unique(geoData()[[input$groupVariable]])
                availableGroups <- as.character(availableGroups[availableGroups %in% colnames(summarySpaceData()$data)])
                if (input$groupVariable == "dataSource") {
                  availableGroups <- availableGroups[availableGroups %in% bronMap()]
                }
              }              
              
              if (!is.null(input$groupVariable) && input$groupVariable == "schadeCode") {
                metaSchade <- unlist(loadMetaSchade()$codes)
                availableGroups <- metaSchade[metaSchade %in% availableGroups]
                names(availableGroups) <- sapply(names(availableGroups), function(x) strsplit(x, split = "\\.")[[1]][2])
              } else {
                names(availableGroups) <- sapply(availableGroups, function(x) strsplit(x, split = "\\.")[[1]][1])
              }
              
              
              if (length(availableGroups) > 1)
                apply(do.call(cbind, Map(paste, names(availableGroups), tmpData[, availableGroups, drop = FALSE], sep = ": ")), 1, function(x)
                    paste("</br>", paste(x, collapse = "</br>"))) else
                sapply(tmpData[, availableGroups], function(x) paste0("</br>", names(availableGroups), ": ", x))
            } else
              round(tmpData$freq, 2)
          
          textPopup <- paste0("<h4>", regionNames, "</h4>",  
            "<strong>", titleText, "</strong>: ", 
            contentText
          )
          
          return(textPopup)
          
        })
      
      
      # Define colors for the polygons
      colorScheme <- reactive({
          
          # Might give warnings if n < 3
          if (type == "empty") {
            
            NULL
            
          } else if (type == "wbe") {
            
            suppressWarnings(RColorBrewer::brewer.pal(
                n = nlevels(summarySpaceData()$data$group), name = "YlOrBr"))
            
          } else {
            
            suppressWarnings(c("white", RColorBrewer::brewer.pal(
                  n = nlevels(summarySpaceData()$data$group) - 1, name = "YlOrBr")))
            
          }
          
        })			
      
      
      # Send map to the UI
      spacePlot <- reactive({
          
          req(allSpatialData)
          
          validate(need(spatialData(), "Geen data beschikbaar"),
            need(nrow(summarySpaceData()$data) > 0, "Geen data beschikbaar"))
          
          mapFlanders(
            regionLevel = regionLevelLocal(),
            species = species(), 
            year = year(),
            allSpatialData = allSpatialData,
            summaryData = summarySpaceData()$data,
            colorScheme = colorScheme(),
            addGlobe = !hideGlobeDefault,
            borderRegion = if (!is.null(locaties()))
                regionLevel() else if (regionLevelLocal() %in% c("communes", "communes_wolf", "fbz_gemeentes", "utm5"))
                switch(regionLevelLocal(),
                  "communes" = "provinces",
                  "communes_wolf" = "provinces",
                  "fbz_gemeentes" = "faunabeheerzones",
                  "utm5" = "provinces"
                ),
            borderLocaties = locaties(),
            legend = "topright",
            legendText = isolate(simpleCap(unitText(), keepNames = FALSE)),
            statsMap = statsMap()
          ) |> leaflet_bound_flanders()
        })
      
      output$spacePlot <- renderLeaflet({
          result <- tryCatch({
              spacePlot()
              
            }, error = function(e) {
              print(e)
              NULL
            })
          
          return(result)
          
        })
      
      output$spacePlotMessage <- renderUI({
          msg <- tryCatch({
              spacePlot()
              NULL
            }, error = function(e) conditionMessage(e))
          
          if (is.null(msg)) {
            return(NULL)
          } else {
            div(style = "color:#595959; margin: 1em 0;",
              msg)
          }
        })
      
      # Statistics with map
      statsMap <- reactive({
          
          if (type %in% c("wbe", "empty"))
            return(NULL)
          
          regionLev <- coalesce(input$regionLevel, preSelected()$regionLevel(), NA)
          if (is.na(regionLev) || regionLev == "flanders")
            return(NULL)
          
          percentage <- round(with(summarySpaceData()$stats, nAvailable / nTotal) * 100, 1) 
          
          paste0(percentage, "% van data met nodige info (", summarySpaceData()$stats$nAvailable, "/", 
            summarySpaceData()$stats$nTotal, ")" )
          
        })
      
      
      # Which region(s) are selected?
      observe({
          
          event <- input$spacePlot_shape_click
          
          if (!is.null(event) && !is.null(event$id)) {
            currentSelected <- isolate(results$region_value)
            
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
      
      
      # Center view for WBE
      observe({
          
          req(!is.null(currentWbe()) | !is.null(locaties()))
          
          # Polygons to center on (!= selectedPolygons() for F17_1,2)
          tmpSpatial <- filterSpatial(
            allSpatialData = allSpatialData, 
            species = req(species()), 
            regionLevel = regionLevel(), 
            year = req(year()),
            locaties = if (!is.null(currentWbe())) currentWbe() else locaties()
          )
          validate(need(tmpSpatial, "Geen data beschikbaar"))
          
          selectedPolygons <- subset(tmpSpatial, tmpSpatial$NAAM %in% results$region_value)
          
          centerValues <- getCenterView(sf_object = selectedPolygons)
          
          leafletProxy("spacePlot", data = spatialData()) |>
            leaflet_bound_flanders()
          
        })
      
      
      # Pre-selected polygons to highlight
      selectedPolygons <- reactive({
          if (type %in% "dash") {
            return(NULL)
          }
          
          tmpSpatial <- filterSpatial(
            allSpatialData = allSpatialData, 
            species = req(species()), 
            regionLevel = regionLevelLocal(), 
            year = req(year()),
            locaties = currentWbe()
          )
          validate(need(tmpSpatial, "Geen data beschikbaar"))
          
          subset(tmpSpatial, tmpSpatial$NAAM %in% results$region_value)
          
        })
      
      
      # Plot thick border for selected regions
      observeEvent(c(selectedPolygons(), spacePlot()), {
          
          if (length(selectedPolygons()) > 0) {
            
            leafletProxy("spacePlot", data = spatialData()) %>%
              
              clearGroup(group = "regionLines") %>%
              
              addPolylines(data = selectedPolygons(), color = "gray", weight = 5,
                group = "regionLines")
            
          } else {
            
            leafletProxy("spacePlot", data = spatialData()) %>%
              
              clearGroup(group = "regionLines")
            
          }
          
        })
      
      
      # Add world map
      observe({
          
          req(!is.null(input$globe))
          
          proxy <- leafletProxy("spacePlot", data = spatialData())
          
          if (!is.null(proxy)){
            
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
      observeEvent(input$legend, {
          
          results$legend <- input$legend
          
          proxy <- leafletProxy("spacePlot", data = spatialData())
          proxy %>% removeControl(layerId = "legend")
          
          if (input$legend != "none") {
            
            palette <- colorFactor(palette = colorScheme(), 
              levels = levels(summarySpaceData()$data$group))
            
            if (type == "wbe")
              valuesPalette <- summarySpaceData()$data$group else 
              valuesPalette <- summarySpaceData()$data[
                match(spatialData()$NAAM, summarySpaceData()$data$locatie),
                "group"]
            
            if (!all(is.na(valuesPalette)))
              proxy %>% addLegend(
                position = input$legend,
                pal = palette, 
                values = valuesPalette,
                na.label = "bos & natuur < 100ha",
                opacity = 0.8,
                title = simpleCap(unitText(), keepNames = FALSE),
                layerId = "legend"
              )                      
            
          }
          
        }, ignoreInit = TRUE)
      
      
      # Add popups
      observe({
          
          currentMap <- leafletProxy("spacePlot", data = spatialData()) 
          currentMap %>% clearPopups()
          
          event <- input$spacePlot_shape_click
          
          if (!is.null(event) && !is.null(event$id) && 
            event$id %in% summarySpaceData()$data$locatie) {
            
            textSelected <- textPopup()[
              summarySpaceData()$data$locatie == event$id]
            
            isolate({
                
                currentMap %>% 
                  addPopups(event$lng, event$lat, popup = textSelected)
                
              }) 
            
          }
          
        })
      
      
      
      
      # Create final map (for download)
      finalMap <- reactive({
          
          validate(need(summarySpaceData()$data, "Geen data beschikbaar"))
          
          newMap <- mapFlanders(
              regionLevel = regionLevelLocal(), 
              species = species(),
              year = req(year()),
              allSpatialData = allSpatialData,
              summaryData = summarySpaceData()$data,
              colorScheme = colorScheme(),
              legend = results$legend,
              legendText = simpleCap(unitText(), keepNames = FALSE),
              addGlobe = input$globe %% 2 == as.numeric(hideGlobeDefault),
              borderRegion = if (!is.null(locaties()))
                  regionLevel() else if (regionLevelLocal() %in% c("communes", "communes_wolf", "fbz_gemeentes", "utm5"))
                  switch(regionLevelLocal(),
                    "communes" = "provinces",
                    "communes_wolf" = "provinces",
                    "fbz_gemeentes" = "faunabeheerzones",
                    "utm5" = "provinces"
                  ),
              borderLocaties = locaties() 
            ) %>%
            # save the zoom level and centering to the map object
            setView(
              lng = input$spacePlot_center$lng,
              lat = input$spacePlot_center$lat,
              zoom = input$spacePlot_zoom
            )
          
          if (isTruthy(selectedPolygons())) {
            # Selected regions
            newMap <- newMap |>
              leaflet::addPolylines(
                data = selectedPolygons(),
                color = "gray",
                weight = 5,
                group = "regionLines"
              ) 
          }

          newMap

        }) 

      output$download <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            year = year(), 
            extraInfo =  if (regionLevelLocal() == "WBE_buitengrenzen") regionLevelName(),
            content = "kaart", fileExt = "png"),
        content = function(file) {
          
          idNote <- showNotification("Aanvraag wordt verwerkt... Even geduld.", type = "message", duration = NULL)
          
          tmpFile <- tempfile(fileext = ".html")
          
          # write map to temp .html file
          htmlwidgets::saveWidget(finalMap(), file = tmpFile, selfcontained = FALSE)
          
          # convert temp .html file into .png for download
          webshot2::webshot(url = tmpFile, file = file,
            vwidth = 1000, vheight = 500, cliprect = "viewport")
          
          removeNotification(id = idNote)
          
        }
      )
      
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = species(),
            year = year(), 
            extraInfo =  if (regionLevelLocal() == "WBE_buitengrenzen") regionLevelName(),
            content = "kaartData", fileExt = "csv"),
        content = function(file) {
          
          myData <- summarySpaceData()$data
          # change variable names
          names(myData)[names(myData) == "freq"] <- unitText()
          names(myData)[names(myData) == "group"] <- "groep"
          
          ## write data to exported file
          write.table(x = myData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ",")
          
        })
      
      
      ## ------------------ ##
      ## Extra plot details ##
      ## ------------------ ##     
      
      ## Time plot for selected region ##
      ## ----------------------------- ##
      
      # Create data for map, time plot
      timeData <- reactive({
          
          validate(need(period(), "Gelieve periode te selecteren"))
          
          createTrendData(
            data = filterDataSource(plotData = geoData(), 
              sourceIndicator = bronMap(),
              returnStop = "data"),
            allSpatialData = allSpatialData,
            biotoopData = biotoopData[[regionLevelLocal()]],
            timeRange = period(),
            species = species(),
            regionLevel = regionLevelLocal(),
            unit = unit()
          )
          
        })
      
      # Title for selected region level
      output$timeTitle <- renderUI({
          
          h3(
            paste0("Jaarlijks",
            if (type == "schade") "e schadegevallen door " else " gerapporteerd afschot van ", 
            tolower(species()), if (regionLevelName() != "Vlaanderen") paste0(" per geselecteerde ", tolower(regionLevelName())) else (" in Vlaanderen") 
          ))
          
        })
      
      
      callModule(module = optionsModuleServer, id = "timePlot", 
        data = timeData)
      callModule(module = plotModuleServer, id = "timePlot",
        plotFunction = "trendYearRegion", 
        data = timeData,
        locaties = reactive({
            if (!is.null(currentWbe()))
              regionLevelName() else
              results$region_value
          }),
        filterDataOnRegion = FALSE,
        timeRange = reactive(period()),
        unit = reactive(unit()),
        combinatie = reactive(input$combinatie),
        isSchade = (type == "schade"),
        height = "400px",
        preSelected = preSelected
      )
      
      
      ## Biotoop plot for selected region ##
      ## -------------------------------- ##
      
      
      # Title for selected region level
      output$biotoopTitle <- renderUI({
          
          req(type != "empty")
          
          title <- getOutputTitle(output = "barBiotoop", uiText = uiText)
          
          tagList(
            h3(title, tags$br(), 
              req(regionLevelName()), 
              if (!is.null(year())) paste("in", year()))
          )
          
        })
      
      output$biotoopPlotText <- renderUI({
          
          context <- strsplit(id, "_")[[1]][1]
          
          description <- getOutputDescription(output = "barBiotoop", 
            uiText = uiText, context = context)
          
          tags$div(class = "larger-description", HTML(description))
        })
      

      output$biotoopTableText <- renderUI({
          
          description <- getOutputDescription(output = "tableBackground", 
            uiText = uiText, context = strsplit(id, "_")[[1]][1])
          
          tags$div(class = "larger-description", HTML(description))
          
        })


      # Plot
      biotoopPlotData <- reactive({
          
          subData <- if (!is.null(currentWbe()))
              biotoopData[biotoopData$year == year() & biotoopData$regio %in% currentWbe(), ] else
              subset(biotoopData[[req(regionLevelLocal())]], regio %in% results$region_value)
          
          if (!is.null(input$combinatieBiotoop) && input$combinatieBiotoop) {
            subData$regio <- "Totaal"
            subData <- merge(
              aggregate(subData[, grepl("Area", colnames(subData))], 
                by = list(regio = subData$regio), FUN = sum),
              aggregate(subData[, grepl("perc", colnames(subData))], 
                by = list(regio = subData$regio), FUN = mean)
            )
          }
          
          subData
          
        })
      
      callModule(module = optionsModuleServer, id = "biotoopPlot", 
        data = biotoopPlotData
      )
      callModule(module = plotModuleServer, id = "biotoopPlot",
        plotFunction = "barBiotoop", 
        data = biotoopPlotData,
        height = "400px",
        preSelected = preSelected
      )
      
      # Table
      biotoopTableData <- reactive({
          
          if (regionLevelLocal() == "flanders")
            biotoopData$flanders else 
            rbind(subset(biotoopData[[req(regionLevelLocal())]], regio %in% results$region_value),
              biotoopData$flanders)
          
        })
      
      callModule(module = optionsModuleServer, id = "biotoopTable", 
        data = biotoopTableData 
      )
      callModule(module = plotModuleServer, id = "biotoopTable",
        plotFunction = "tableBackground",
        data = biotoopTableData,
        preSelected = preSelected
      )
      
      
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

#' Shiny module for creating the plot \code{\link{mapFlanders}} - UI side
#' @inheritParams mapFlandersServer 
#' @param showRegion boolean, whether to show choices for regionLevel and selected region(s)
#' @param showCombine boolean, whether to show option to combine selected regions
#' @param unitChoices named character vector, choices for unit option;
#' default is \code{c("Aantal" = "absolute", "Aantal/100ha" = "relative")}
#' @param showTitle boolean, whether to show title above the map
#' @param mapScaleChoices named character vector, choices for the
#' map scales
#' @inheritParams getOutputTitle
#' @inheritParams reportingGrofwild-common-args
#' @return UI object
#' @author mvarewyck
#' @import shiny
#' @export
mapFlandersUI <- function(id, showRegion = (type != "dash"),
  showCombine = TRUE, uiText, specie = NULL, doHide = TRUE,
  type = c("beheer", "schade", "wbe", "dash"),
  regionChoices = c(
    "Vlaanderen" = "flanders",
    "Provincie" = "provinces", 
    "Faunabeheerzones" = "faunabeheerzones",
    "Gemeente" = "communes",
    "Gemeente per Faunabeheerzone" = "fbz_gemeentes",
    "5x5 UTM" = "utm5"
  ),
  mapScaleChoices = regionChoices,
  unitChoices = c("Aantal" = "absolute", "Aantal/100ha" = "relative", "Aantal/100ha bos & natuur" = "relativeDekking"),
  plotDetails = c("region", "biotoop"),
  variableChoices = c("Bron" = "dataSource",
    "Type schade" = "schadeCode",
    "Seizoen" = "season"
  ),
  showTitle = TRUE) {
  
  ns <- NS(id)
  type <- match.arg(type)
  
  legendChoices <- c(
    "Bovenaan rechts" = "topright",
    "Onderaan rechts" = "bottomright",
    "Bovenaan links" = "topleft",
    "Onderaan links" = "bottomleft",
    "<geen>" = "none")
  sourceChoices <- if (type == "dash")
      c("waarnemingen.be", "afschot") else
      loadMetaSchade()$sources
  

  
  mainTitle <- if (type == "wbe") 
      "Landkaart" else
      getOutputTitle(
        output = outputFunction(type, specie), 
        uiText = uiText, specie = specie, type = type
      )
  
  # Map with according line plot
  
  tagList(
    
    if (showTitle)
      actionLink(inputId = ns("linkMapFlandersUI"), label = h3(HTML(mainTitle))),
    
    conditionalPanel(paste("(", tolower(!showTitle), ") || input.linkMapFlandersUI % 2  ==", as.numeric(doHide)), ns = ns,
      ## countMap: all species
      wellPanel(
        if (showRegion)
          fixedRow(
            column(8, uiOutput(ns("region"))),
            column(4, selectInput(inputId = ns("regionLevel"), label = "Regio-schaal",
                choices = regionChoices,
                selected = if (type == "dash") regionChoices[1] else "communes"))
          ),  
        
        if (type == "dash") {
            
            fixedRow(
#              column(8, uiOutput(ns("borderRegion"))),  
#              column(4, selectInput(inputId = ns("borderLevel"), label = "Regio-schaal",
#                  choices = regionChoices,
#                  selected = regionChoices[1])),
              column(6, selectInput(inputId = ns("regionLevel"), label = "Kaartweergave",
                  choices = mapScaleChoices,
                  selected = "communes")),
              column(6, selectInput(inputId = ns("legend"), label = "Legende",
                  choices = legendChoices)
              )#,
#              column(6, uiOutput(ns("bronMap"))),
#              column(6, selectInput(inputId = ns("unit"), label = "Eenheid",
#                  choices = unitChoices)),
#              column(6, uiOutput(ns("year")))
            )
            
          } else {
            
            tagList(            
              fixedRow(
                column(6, uiOutput(ns("year"))),
                column(6, if (type %in% c("wbe")) 
                      selectInput(inputId = ns("legend"), label = "Legende",
                        choices = legendChoices) else if (type %in% c("beheer"))
                      selectInput(inputId = ns("unit"), label = "Eenheid",
                        choices = unitChoices) else if (type %in% c("schade"))
                      selectInput(inputId = ns("groupVariable"), label = "Variabele",
                        choices = variableChoices) else
                    if (!(type %in% c("schade"))) uiOutput(ns("period")))
              ),
              
              if (!type %in% c("wbe", "beheer"))
                fixedRow(
                  column(12/(2+(type=="schade")),
                    selectInput(inputId = ns("legend"), label = "Legende (kaart)",
                      choices = legendChoices)
                  ),
                  column(12/(2+(type=="schade")),
                    selectInput(inputId = ns("unit"), label = "Eenheid",
                      choices = unitChoices)
                  )
                )
            )
            
          },
        
        if (showCombine & "region" %in% plotDetails)
          if (!type %in% c("beheer", "schade")) checkboxInput(inputId = ns("combinatie"), 
              label = "Combineer alle geselecteerde regio's (grafiek: Evolutie gerapporteerd afschot Gemeente)"),
        actionLink(inputId = ns("globe"), label = "Voeg landkaart toe",
          icon = icon("globe"))
      
      ),
      
      fixedRow(
        column(if ("biotoop" %in% plotDetails) 6 else 12,
          uiOutput(ns("mapTitle")),
          uiOutput(ns("spacePlotMessage")),
          withSpinner(leafletOutput(ns("spacePlot"))),
          tags$div(align = "center", uiOutput(ns("stats"))),
          tags$br(),
          downloadButton(ns("download"), label = "Download figuur", class = "downloadButton"),
          downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton")
        ),
        
        if (any(grepl("biotoop", plotDetails)))
          uiOutput(ns("biotoopTitle")),
        
        if ("biotoop" %in% plotDetails)
          column(6, 
            uiOutput(ns("biotoopPlotText")),
            if (showCombine)
              checkboxInput(inputId = ns("combinatieBiotoop"), 
                label = "Combineer alle geselecteerde regio's"),
            plotModuleUI(id = ns("biotoopPlot")),
            optionsModuleUI(id = ns("biotoopPlot"), exportData = TRUE,
              doWellPanel = FALSE)
          ),
        if ("biotoopTable" %in% plotDetails)
          column(6,
            uiOutput(ns("biotoopTableText")),
            tableModuleUI(id = ns("biotoopTable")),
            optionsModuleUI(id = ns("biotoopTable"), exportData = TRUE,
              doWellPanel = FALSE)
          )
      ),
      
      
      
      if ("region" %in% plotDetails) {
        
        fixedRow(
          column(12,
            uiOutput(ns("timeTitle")),
            if (type %in% c("beheer", "schade")) 
                tagList(
                  column(8, withSpinner(plotModuleUI(id = ns("timePlot")))),
                  column(4, wellPanel(
#                      uiOutput(ns("period")),
                      checkboxInput(inputId = ns("combinatie"), 
                        label = "Combineer alle geselecteerde regio's")
                    ))) else plotModuleUI(id = ns("timePlot")),
            column(12, 
              tags$br(), 
              optionsModuleUI(id = ns("timePlot"), exportData = TRUE,
                doWellPanel = FALSE))
          )
        )
        
      },
      br(),
      uiOutput(ns("description")),
      tags$hr()
    
    )
  )
  
}