#' Create interactive plot for comparing age and group variable
#' 
#' @inheritParams countYearAge 
#' @param groupVariable character, variable in \code{data}
#' @param sourceIndicator character, source to filter embryo data on
#' @param sourceIndicator_leeftijd character, source to filter leeftijd data on
#' @param sourceIndicator_geslacht character, source to filter geslacht data on
#' @return list with:
#' \itemize{
#' \item 'plot':  plotly object, for a given species the percentage per age category
#' and per group, based on meldingsformulier data in a stacked bar chart 
#' \item 'data':  data displayed in the plot, as data.frame with:
#' \itemize{
#' \item 'leeftijd':  age category 
#' \item 'group':  group indicator 
#' \item 'freq':  counts of animals 
#' \item 'percent':  percentage of counts of animals 
#' }
#' }
#' @author mvarewyck
#' @import plotly
#' @importFrom plyr count ddply
#' @importFrom INBOtheme inbo_palette
#' @export
countAgeGroup <- function(data, groupVariable, jaartallen = NULL,
  sourceIndicator = c("inbo", "meldingsformulier", "both"), regio = "", 
  sourceIndicator_leeftijd = NULL, sourceIndicator_geslacht = NULL,
  width = NULL, height = NULL) {
  
  
  wildNaam <- unique(data$wildsoort)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  
  # Filter on source & rename to embryos
  plotData <- filterGrofwild(plotData = data, 
    sourceIndicator_embryos = sourceIndicator,
    sourceIndicator_leeftijd = sourceIndicator_leeftijd,
    sourceIndicator_geslacht = sourceIndicator_geslacht)
  
  plotData <- plotData[plotData$geslacht_comp == "Vrouwelijk",]
  validate(need(nrow(plotData) > 0, "Geen data beschikbaar"))
  
  plotData$reproductiestatus <- ifelse(
    is.na(plotData$embryos), "Onbekend",
    ifelse(plotData$embryos != 0, "Drachtig", "Niet drachtig")
  )
  
  plotData <- plotData[plotData$afschotjaar %in% jaartallen, c(groupVariable, "leeftijd_comp")]
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
  
  colors <- replicateColors(values = levels(as.factor(summaryData[[groupVariable]])))$colors
  
  title <- paste(wildNaam, paste0("(", 
      ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
        jaartallen), ")"), 
    if (!all(regio == "")) paste0("\n (", toString(regio), ")"))
  groupLabel <- simpleCap(groupVariable)
  names(groupLabel) <- NULL
  
  # Create plot
  pl <- plot_ly(data = summaryData, x = ~leeftijd, y = ~freq, color = ~base::get(groupVariable),
      text = ~text, textposition = "none", hoverinfo = "x+text+name",
      colors = colors, type = "bar") %>%
    
    plotly::layout(title = title,
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
#' @param data data.frame for the plot function
#' @param timeRange numeric vector of length 2, min and max year to subset data
#' @param title reactive character, title with asterisk to show in the \code{actionLink}
#' @inheritParams countAgeGroup
#' @inheritParams reportingGrofwild-common-args  
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countAgeGroupServer <- function(id, data, timeRange, groupVariable, 
  title = reactive(NULL), preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$disclaimerAgeGroup <- renderUI({
          
          req(title())
          
          if (grepl("\\*", title()))
            getDisclaimerLimited()
          
        })
      
      callModule(module = optionsModuleServer, id = "ageGroup", 
        data = data, 
        timeRange = timeRange
      )
      toReturn <- callModule(module = plotModuleServer, id = "ageGroup",
        plotFunction = "countAgeGroup", 
        data = data,
        groupVariable = groupVariable,
        preSelected = preSelected
      )
      
      return(reactive(toReturn()))
      
    })
  
}


#' Shiny module for creating the plot \code{\link{countAgeGroup}} - UI side
#' @inherit welcomeSectionUI 
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @export
countAgeGroupUI <- function(id, regionLevels = NULL,
  uiText, context = id, specie = NULL, showTime = FALSE, showDataSource = c(),
  doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = "countAgeGroupUI", specie = specie, 
    uiText = uiText)
  description <- getOutputDescription(output = "countAgeGroupUI", 
    specie = specie, uiText = uiText, context = context)

  tagList(
    
    actionLink(inputId = ns("linkAgeGroup"),
      label = tags$h3(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkAgeGroup % 2 ==", as.numeric(doHide)), 
      ns = ns,
      
      uiOutput(ns("disclaimerAgeGroup")),
      
      fixedRow(
        column(8, plotModuleUI(id = ns("ageGroup"))),
        column(4,
          optionsModuleUI(id = ns("ageGroup"), 
            showTime = showTime,
            regionLevels = regionLevels, exportData = TRUE,
            showDataSource = showDataSource)
        )
      ),
      tags$p(HTML(description))
    )
  )
  
}



