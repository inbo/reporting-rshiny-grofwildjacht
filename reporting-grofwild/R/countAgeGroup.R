#' Create interactive plot for comparing age and group variable
#' 
#' @inheritParams countYearAge 
#' @param groupVariable character, variable in \code{data}
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the percentage per age category
#' and per group, based on meldingsformulier data in a stacked bar chart}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'leeftijd': }{age category}
#' \item{'group': }{group indicator}
#' \item{'freq': }{counts of animals}
#' \item{'percent': }{percentage of counts of animals}
#' }
#' }
#' }
#' 
#' @author mvarewyck
#' @import plotly
#' @importFrom plyr count ddply
#' @importFrom INBOtheme inbo_palette
#' @export
countAgeGroup <- function(data, groupVariable, jaartallen = NULL) {
  
  
  wildNaam <- unique(data$wildsoort)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  
  plotData <- data[data$afschotjaar %in% jaartallen, c(groupVariable, "leeftijd_comp")]
  names(plotData)[names(plotData) == "leeftijd_comp"] <- "leeftijd"

  
#  # For percentage collected
#  nRecords <- nrow(plotData)
  
  # Remove missing groups
  plotData <- plotData[!is.na(plotData[, groupVariable]), ]
  
  
  
  # Summarize data per age
  summaryData <- count(df = plotData, vars = names(plotData))
  freq <- NULL  # to prevent warnings with R CMD check 
  summaryData <- ddply(summaryData, "leeftijd", transform, 
    percent = freq / sum(freq) * 100)
  
  # For optimal displaying in the plot
  summaryData$leeftijd <- factor(summaryData$leeftijd, 
    levels = c(loadMetaEco(species = wildNaam)$leeftijd_comp, "Onbekend"))
  
  summaryData$text <- paste0(round(summaryData$percent), "%",
    " (", summaryData$freq, ")")
  
  totalCount <- count(df = summaryData, vars = "leeftijd", wt_var = "freq")$freq
  
  groupLevels <- levels(as.factor(summaryData[[groupVariable]]))
  colors <- inbo_palette(n = length(groupLevels))
  names(colors) <- groupLevels
  if ("Onbekend" %in% groupLevels)
    colors[groupLevels == "Onbekend"] <- "gray"
  
  title <- paste(wildNaam, paste0("(", 
      ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
        jaartallen), ")"))
  groupLabel <- simpleCap(groupVariable)
  names(groupLabel) <- NULL
  
  # Create plot
  pl <- plot_ly(data = summaryData, x = ~leeftijd, y = ~freq, color = ~get(groupVariable),
      text = ~text,  hoverinfo = "x+text+name",
      colors = colors, type = "bar") %>%
    
    layout(title = title,
      xaxis = list(title = "Leeftijdscategorie (INBO of Meldingsformulier)"), 
      yaxis = list(title = "Aantal geschoten dieren"),
      legend = list(y = 0.8, yanchor = "top"),
      margin = list(b = 120, t = 100), 
      barmode = "stack",
      annotations = list(x = levels(summaryData$leeftijd), y = -(max(summaryData$freq)/10), 
        text = totalCount, xanchor = 'center', yanchor = 'bottom', 
        showarrow = FALSE)) %>%
    
    add_annotations(text = groupLabel, 
      xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
      y = 0.8, yanchor = "bottom",    # Same y as legend below
      legendtitle = TRUE, showarrow = FALSE)
  
#  add_annotations(
#    text = percentCollected(nAvailable = nrow(plotData), nTotal = nRecords,
#      text = paste("gekende", groupVariable)),
#    xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
#    y = -0.3, yanchor = "bottom", showarrow = FALSE)  
  
  colsFinal <- colnames(summaryData)[colnames(summaryData) != "text"]
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  
  return(list(plot = pl, data = summaryData[, colsFinal]))
  
}




#' Shiny module for creating the plot \code{\link{countAgeGroup}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @param timeRange numeric vector of length 2, min and max year to subset data
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countAgeGroupServer <- function(id, data, timeRange, groupVariable) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      callModule(module = optionsModuleServer, id = "ageGroup", 
        data = data, 
        timeRange = timeRange
      )
      toReturn <- callModule(module = plotModuleServer, id = "ageGroup",
        plotFunction = "countAgeGroup", 
        data = data,
        groupVariable = groupVariable
      )
      
      return(reactive(toReturn()))
      
    })
  
}


#' Shiny module for creating the plot \code{\link{countAgeGroup}} - UI side
#' @template moduleUI
#' @param title character, plot title in the app, overrides the automatic title
#' @param doHide boolean, whether to initially hide the plot; default TRUE
#' 
#' @author mvarewyck
#' @export
countAgeGroupUI <- function(id, uiText, title = NULL, groupVariable, doHide = TRUE) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == paste0(as.character(match.call())[1], "-", groupVariable), ]
  
  tagList(
    
    actionLink(inputId = ns("linkAgeGroup"),
      label = h3(HTML(if (!is.null(title)) title else uiText$title))),
    conditionalPanel(paste("input.linkAgeGroup % 2 ==", as.numeric(doHide)), ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("ageGroup"), showTime = TRUE, exportData = TRUE),
          tags$p(HTML(uiText[, strsplit(id, split = "_")[[1]][1]]))
        ),
        column(8, 
          plotModuleUI(id = ns("ageGroup"))
        )
      ),
      tags$hr()
    )
  )
  
}



