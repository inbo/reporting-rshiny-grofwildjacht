# Project: grofWild_git
# 
# Author: dbemelmans
###############################################################################

#' Create interactive plot for percentage per hunting method
#' @param data data.frame with raw data for plotting
#' @param regio character, regional level of interest should be one of 
#' \code{c("provinces", "flanders", "faunabeheerzones")}
#' @param locaties character vector, selected set of \code{regio} to be retained
#' @param jaartallen integer vector, defines the year(s) that should be considered
#' in the plot; if NULL no selection on year(s) is made
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given specie the observed number 
#' per year and per province is plotted in a stacked bar chart}
#' \item{'data': }{data.frame, summary data for download}
#' }
#' @import plotly
#' @importFrom INBOtheme inbo_palette
#' @export
countHuntingMethod <- function(data, regio = NULL, locaties = NULL, jaartallen, width = NULL, height = NULL) {
  
  plotData <- data
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  
  # Select data
  if (is.null(regio)) {
    plotData <- plotData 
  } else if(regio == "provinces") {
    plotData <- plotData[plotData$provincie %in% locaties, ]
  } else if(regio == "faunabeheerzones") {
    plotData <- plotData[plotData$FaunabeheerZone %in% locaties, ]
  } else
    plotData <- plotData
  
  plotData <- plotData[plotData$afschotjaar %in% jaartallen, c("afschotjaar", "jachtmethode_comp")]
  plotData <- plotData[!is.na(plotData$afschotjaar), ]
  
  plotData$jachtmethode_comp[is.na(plotData$jachtmethode_comp)] <- "onbekende jachtmethode"
  
  
  # Summarize data per province and year
  plotData$afschotjaar <- with(plotData, factor(afschotjaar, levels = 
              min(jaartallen):max(jaartallen)))
  
  summaryData <- melt(table(plotData), id.vars = "afschotjaar")
  
  # Summarize data per year
  totalCount <- table(plotData$afschotjaar)
  
  # For optimal displaying in the plot
  summaryData$jachtmethode_comp <- as.factor(summaryData$jachtmethode_comp)
  summaryData$jachtmethode_comp <- factor(summaryData$jachtmethode_comp, levels = rev(levels(summaryData$jachtmethode_comp)))
  summaryData$afschotjaar <- as.factor(summaryData$afschotjaar)
  
  colors <- rev(inbo_palette(n = nlevels(summaryData$jachtmethode_comp)))
  title <- paste0("Afschot van ",
      ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
          jaartallen)
  )
  
  # Create plot
  pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~value, color = ~jachtmethode_comp,
          colors = colors, type = "bar",  width = width, height = height) %>%
      layout(title = title,
          xaxis = list(title = "Jaar"), 
          yaxis = list(title = "Aantal"),
          margin = list(b = 80, t = 100), 
          barmode = ifelse(nlevels(summaryData$afschotjaar) == 1, "group", "stack"),
          annotations = list(x = levels(summaryData$afschotjaar), 
              y = totalCount, 
              text = paste(ifelse(nlevels(summaryData$afschotjaar) == 1, "totaal:", ""), totalCount),
              xanchor = 'center', yanchor = 'bottom',
              showarrow = FALSE),
          showlegend = TRUE)  
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  return(list(plot = pl, data = summaryData))
}




#' Shiny module for creating the plot \code{\link{countHuntingMethod}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @param timeRange numeric vector of length 2, min and max year to subset data
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countHuntingMethodServer <- function(id, data, timeRange) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Afschot per jachtmethode
      callModule(module = optionsModuleServer, id = "huntingMethod", 
        data = data,
        timeRange = timeRange)
      callModule(module = plotModuleServer, id = "huntingMethod",
        plotFunction = "countHuntingMethod", 
        data = data)
      
    })
  
}


#' Shiny module for creating the plot \code{\link{countHuntingMethod}} - UI side
#' @param regionLevels numeric vector, region level choices
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
countHuntingMethodUI <- function(id, regionLevels = NULL, uiText = uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    actionLink(inputId = ns("linkHuntingMethod"), 
      label = h3(HTML(uiText$title))),
    conditionalPanel("input.linkHuntingMethod % 2 == 1", ns = ns,

      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("huntingMethod"), showTime = TRUE, 
            regionLevels = regionLevels, exportData = TRUE),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, plotModuleUI(id = ns("huntingMethod")))
      
      ),
      tags$hr()
    )
  )
  
}






