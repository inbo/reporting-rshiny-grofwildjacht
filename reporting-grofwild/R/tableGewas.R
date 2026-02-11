# Project: inbo-grofwildjacht_git
# Frequency table for gewas 
# 
# Author: mvarewyck
###############################################################################


#' Create summary table for gewas by region and year.
#' @inheritParams tableSchadeCode
#' @inheritParams reportingGrofwild-common-args
#' @param variable character, defines the dependent variable in the table besides \code{type}
#' @param jaartallen integer vector, defines the year(s) that should be considered
#' @return data.frame, number of opbservations per region and per \code{variable}
#' @author Eva Adriaensen
#' @importFrom plyr count
#' @importFrom reshape2 dcast
#' @importFrom sf st_drop_geometry
#' @export
tableGewas <- function(data, jaartallen = NULL, variable, 
  sourceIndicator = NULL, summarizeBy = c("count", "percent"), regio = "",
  regionLevel = c("provinces", "flanders", "faunabeheerzones")) {
  
  # For R CMD check
  varOfInterest <- leeftijd_comp_inbo <- NULL
  
  summarizeBy <- match.arg(summarizeBy)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  allData <- data
  
  # filter for source
  allData <- filterDataSource(plotData = data, sourceIndicator = sourceIndicator,
    returnStop = "message")
  
  if (inherits(allData, "sf"))
    allData <- sf::st_drop_geometry(allData)  
  
  if (regionLevel == "flanders") {
    allData$locatie <- as.factor("Vlaams Gewest")
  } else if (regionLevel == "provinces") {
    allData$locatie <- factor(allData$provincie)
  } else if (all(regio %in% c(as.character(1:10), "Onbekend"))) {
    allData$locatie <- allData$FaunabeheerZone
    allData$locatie <- factor(allData$locatie, levels = levels(droplevels(factor(unique(allData$locatie), 
            levels = c(1:10)))))
  } else {
    allData$locatie <- factor(allData$gemeente_afschot_locatie)
  }
    
  # select relevant columns
  tableData <- allData[allData$afschotjaar %in% jaartallen, c(variable, c("afschotjaar", "locatie"))] #nrow(tableData) [1] 1525
  # exclude logs with unknown location or NA for variable of interest
  tableData <- tableData[!is.na(tableData[, variable]) & !is.na(tableData$locatie), ] #nrow(tableData) [1] 1255
  
  # generate counts
  summaryData <- count(tableData, vars = setdiff(names(tableData), "afschotjaar"))
  
  if (nrow(summaryData) == 0)
    return(NULL)

  # include all provinces/fbz
  fullData <- expand.grid(
      unique(tableData[[variable]]),
      locatie = levels(allData$locatie)
          )
  names(fullData)[1] <- variable
  fullSummaryData <- merge(summaryData, fullData, all = TRUE)
  
  # rename var of interest
  colnames(fullSummaryData)[which(colnames(fullSummaryData) == variable)] <- "varOfInterest" 
  
  # generate var x location table
  summaryTable <- dcast(fullSummaryData, varOfInterest ~ locatie, value.var = "freq")
  summaryTable[is.na(summaryTable)] <- 0
  
  # add col and row sum 
  if (all(regio != "Vlaams Gewest")) {
    summaryTable <- cbind(summaryTable, 
        Vlaanderen = apply(subset(summaryTable, select = -1), 1, sum))
  }
  summaryTable <- rbind(summaryTable,
    c(varOfInterest = "Alle", as.list(apply(subset(summaryTable, select = -1), 2, sum))))   
  
  if (summarizeBy == "percent") {
    last_row <- nrow(summaryTable)
    
    summaryTable <- summaryTable %>%
      mutate(dplyr::across(-varOfInterest, ~ as.numeric(as.character(.x)))) %>%
      mutate(dplyr::across(
          -varOfInterest,
          ~ dplyr::if_else(dplyr::row_number() < last_row, paste0(round(.x / summaryTable[last_row, dplyr::cur_column()] * 100, 2), "%"), "100%")
        ))
  }
    
    variableLabel <- switch(variable,
#        wildsoort = "Wildsoort",
#        schadeBasisCode = "Type Schade",
#        schadeCode = "Type Subschade",
      SoortNaam = "Gewas")
    
    colnames(summaryTable)[colnames(summaryTable) == "varOfInterest"] <- variableLabel
    
    return(list(data = summaryTable))
    
  }
  



#' Shiny module for creating the plot \code{\link{tableGewas}} - server side
#' @inheritParams optionsModuleServer 
#' @inheritParams plotModuleServer
#' @inheritParams tableSchadeCode
#' @inheritParams welcomeSectionUI 
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
tableGewasServer <- function(id, data, types, labelTypes, typesDefault, timeRange,
  variable, allRegionsSelected = FALSE, preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      callModule(module = optionsModuleServer, id = "tableGewas",
        data = data,
        types = types, 
        labelTypes = labelTypes, 
        typesDefault = typesDefault,
        timeRange = timeRange,
        allRegionsSelected = allRegionsSelected
      )
      
      callModule(plotModuleServer, id = "tableGewas",
        plotFunction = "tableGewas",
        data = data,
        variable = variable,
        preSelected = preSelected)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{tableGewas}} - UI side
#' @inherit welcomeSectionUI
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @export
tableGewasUI <- function(id, 
  uiText, context = id, specie = NULL, regionLevels = NULL, regionLevelSelected = NULL,
  doHide = TRUE, showTime = FALSE, showDataSource = c(), summarizeBy = NULL) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(
    output = "tableGewasUI", specie = specie, 
    uiText = uiText
  )
  description <- getOutputDescription(
    output = "tableGewasUI", 
    specie = specie, uiText = uiText, context = context
  )
  
  tagList(
    
    actionLink(inputId = ns("linkTableGewas"), 
      label =  tags$h3(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkTableGewas % 2 ==", as.numeric(doHide)),
      ns = ns,
      optionsModuleUI(
        id = ns("tableGewas"), 
        showTime = showTime, 
        regionLevels = regionLevels, 
        regionLevelSelected = regionLevelSelected,
        summarizeBy = summarizeBy,
        showDataSource = showDataSource,
        exportData = TRUE
      ),
      tableModuleUI(id = ns("tableGewas")),
      tags$br(),
      tags$div(class = "larger-description", HTML(description)),
      tags$hr()
    )
  )
  
  
}


