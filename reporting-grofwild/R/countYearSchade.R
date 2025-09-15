# Interactive barplot for wildschade data in function of year
# 
# Author: mvarewyck
###############################################################################



#' Create interactive barplot for wildschade data variable of interest ifo year
#' @inheritParams countYearAge
#' @param type character, variable name in \code{data} of interest
#' @inheritParams filterDataSource
#' @param fullNames named character vector, values for the \code{variable} to be 
#' displayed instead of original data values
#' @return list with
#' \itemize{
#' \item 'plot': plotly object 
#' \item 'data': data.frame used for plot 
#' } 
#' @author mvarewyck
#' @import plotly
#' @importFrom sf st_drop_geometry
#' @export
countYearSchade <- function(data, jaartallen = NULL, type = NULL,
    summarizeBy = c("count", "percent"), fullNames = NULL,
    regio = "",
    sourceIndicator = NULL, width = NULL, height = 600) {
  
  # For R CMD check
  freq <- NULL
  
  typeNaam <- switch(type,
      "wildsoort" = "Wildsoort",
      "SoortNaam" = "Gewas", 
      "schadeCode" = "Type Schade",
      "season" = "Seizoen",
      type
  )
  
  summarizeBy <- match.arg(summarizeBy)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  plotData <- filterDataSource(plotData = data, sourceIndicator = sourceIndicator,
    returnStop = "message")
  
  if (inherits(plotData, "sf"))
    plotData <- sf::st_drop_geometry(plotData)
  
  # Select data
  plotData <- plotData[plotData$afschotjaar %in% jaartallen, 
      c("afschotjaar", type)]
  names(plotData) <- c("jaar", "variabele")
  
  # Replace by group names
  if (type == "SoortNaam") {
    fullNames <- loadMetaSchade()$gewassen
    newNames <- unlist(sapply(names(fullNames), function(x) rep(x, length(fullNames[[x]]))))
    plotData$variabele <- newNames[match(plotData$variabele, unlist(fullNames))]
  }
  
  # Percentage collected
  nRecords <- nrow(plotData)
  
  # Remove some categories
  if(any(is.na(plotData$variabele)))
    plotData[is.na(plotData$variabele), "variabele"] <- "Onbekend"
  plotData <- plotData[!is.na(plotData$jaar) & !is.na(plotData$variabele), ]
  
  # Summarize data per year and age category
  summaryData <- count(df = plotData, vars = names(plotData))
  
  
  # Add line for records with 0 observations
  fullData <- cbind(expand.grid(
          jaar = min(summaryData$jaar):max(summaryData$jaar),
          variabele = unique(summaryData$variabele)))
  summaryData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
  summaryData$freq[is.na(summaryData$freq)] <- 0
  
  
  # Calculate percentages 
  summaryData <- ddply(summaryData, "jaar", transform, 
      percent = freq / sum(freq) * 100)
  
  # Summarize data per year
  totalCount <- count(df = plotData, vars = "jaar")
  totalCount$totaal <- totalCount$freq
  totalCount$freq <- NULL
  
  summaryData <- merge(summaryData, totalCount)
  
  
  # Make full schade names
  if (type == "schadeCode" & !is.null(fullNames)) {
    summaryData$variabele <- names(fullNames)[match(summaryData$variabele, fullNames)]
  }
  
  # For optimal displaying in the plot
#  summaryData$jaar <- as.factor(summaryData$jaar)
  
  if (summarizeBy == "count") {
    
    summaryData$text <- paste0("<b>", summaryData$variabele, " in ", summaryData$jaar, "</b>",
        "<br>Aantal: ", summaryData$freq, " (", round(summaryData$percent), "%)", 
        "<br>Totaal: ", summaryData$totaal)
    
  } else {
    
    summaryData$text <- paste0("<b>", summaryData$variabele, " in ", summaryData$jaar, "</b>",
        "<br>Percent: ", round(summaryData$percent), "%")
    
  }
  
  colorList <- replicateColors(values = unique(summaryData$variabele))
  colors <- colorList$colors
  
  title <- paste0(typeNaam, " ",
      ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
          paste("in", jaartallen)),
      if (!all(regio == "")) paste0("\n(", toString(regio), ")")
  )
  
  singleYear <- length(unique(totalCount$jaar)) == 1
  
  # Create plot
  toPlot <- plot_ly(data = summaryData, x = ~jaar,
          y = if (summarizeBy == "count") ~freq else ~percent, 
          color = ~variabele, text = ~text,
          textposition = "none", hoverinfo = "text+name",
          colors = colors, type = "bar",
          width = width, height = height) %>%
        plotly::layout(title = title,
          xaxis = list(title = "Jaar", 
            tickvals = unique(summaryData$jaar), 
            ticktext = unique(summaryData$jaar)), 
          yaxis = list(title = if (summarizeBy == "count") "Aantal" else "Percentage"),
          barmode = if (singleYear) "group" else "stack",
          # hardcode graph size to prevent legend overlapping plot
          autosize = FALSE,
          margin = list(b = 120, t = 100),
          legend = list(y = 0.1),
          showlegend = TRUE
      )
  
  
  
  colsFinal <- colnames(summaryData)[
      !colnames(summaryData) %in% c("text", 
          if(summarizeBy == "count")	"percent"	else	c("freq", "totaal")
      )
  ]
  
  
  summaryDataFinal <- summaryData[, colsFinal]
  
  # Change variable name
  if ("freq" %in% names(summaryDataFinal)) {
    names(summaryDataFinal)[names(summaryDataFinal) == "freq"] <- "aantal"
    
  }
  
  # To prevent warnings in UI
  toPlot$elementId <- NULL
  
  return(list(plot = toPlot, data = summaryDataFinal, warning = colorList$warning))
  
}





#' Shiny module for creating the plot \code{\link{countYearSchade}} - server side
#' @inheritParams countYearSchade
#' @inheritParams countAgeGenderServer
#' @inheritParams optionsModuleServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countYearSchadeServer <- function(
  id, data, 
  types = NULL, labelTypes = "Type", typesDefault = types, type = NULL,
  timeRange, fullNames = NULL) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      callModule(module = optionsModuleServer, id = "yearSchade", 
        data = data,
        types = types,
        labelTypes = labelTypes,
        typesDefault = typesDefault,
        timeRange = timeRange
      )
      callModule(
        module = plotModuleServer, id = "yearSchade",
        plotFunction = "countYearSchade", 
        data = data,
        fullNames = fullNames,
        type = type
      )
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{countYearSchade}} - UI side
#' @inherit welcomeSectionUI
#' @inheritParams getOutputDescription
#' @inheritParams optionsModuleUI
#' @inheritParams reportingGrofwild-common-args
#' @export
countYearSchadeUI <- function(id, 
  uiText, context = id, specie = NULL, type = NULL, doHide = TRUE,
  regionLevels = NULL) {
  
  showType <- (is.null(type))
  
  title <- getOutputTitle(
    output = "countYearSchadeUI", specie = specie, 
    uiText = uiText, type = type)
  description <- getOutputDescription(
    output = "countYearSchadeUI", 
    specie = specie, uiText = uiText, context = context,
    type = type
  )
  
  ns <- NS(id)
  
  tagList(
    
    actionLink(inputId = ns("linkYearSchade"), label = tags$h3(HTML(title))),
    conditionalPanel(
      condition = 
        paste("input.linkYearSchade % 2 ==", as.numeric(doHide)), 
      ns = ns,
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("yearSchade"))
        ),
        column(4,
          optionsModuleUI(
            id = ns("yearSchade"), 
            summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
            showTime = TRUE, 
            showType = showType, 
            showDataSource = "schade",
            regionLevels = regionLevels,
            exportData = TRUE
          )
        )
      ),
      tags$p(HTML(description))
    )
  )
}