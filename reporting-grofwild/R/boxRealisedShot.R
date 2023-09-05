# Project: grofWild_git
# 
# Author: mvarewyck
###############################################################################




#' Create interactive boxplot for percentage verwezenlijkt per jaar
#' @inheritParams countEmbryos
#' @param data data.frame, with \code{loadToekenningen()} for specific WBE 
#' @param regio, empty function argument needed for generalization in \code{\link{plotModuleServer}}
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for the specified type and jaartallen}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'jaar': }{year at which the animal was shot}
#' \item{'verwezenlijkt': }{count for actually shot animals}
#' \item{'toegekend': }{count for assigned animals}
#' \item{'percent': }{verwezenlijkt / toegekend * 100}
#' }}
#' }
#' @author mvarewyck
#' @import plotly
#' @export
boxRealisedShot <- function(data, regio, type = NULL,
  jaartallen = NULL, 
  width = NULL, height = NULL){
  
  # R CMD check notes
  jaar <- NULL
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$labeljaar)[!is.na(unique(data$labeljaar))]
  
  if (is.null(type))
    type <- unique(data$labeltype)[!is.na(unique(data$labeltype))]
  
  
  # Select on years/type
  plotData <- data[data$labeljaar %in% jaartallen & data$labeltype %in% type,
    c("labeljaar", "labeltype", "verwezenlijkt", "toegekend", "KboNummer_Toek")]
  
  if (nrow(plotData) == 0)
    stop("Geen data beschikbaar")
  
  # Aggregate over type
  summaryData <- aggregate(plotData[, c("verwezenlijkt", "toegekend")], 
    by = list(jaar = plotData$labeljaar, wbe = plotData$KboNummer_Toek), sum)
  summaryData$jaar <- as.factor(summaryData$jaar)
  summaryData$percent <- summaryData$verwezenlijkt / summaryData$toegekend * 100
  
  title <- paste0("Percentage verwezenlijkte labels\n", 
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen)
  )
  
  pl <- plot_ly(data = summaryData, x = ~jaar, y = ~percent,
      type = "box", width = width, height = height) %>%
    layout(title = title,
      xaxis = list(title = "Labeljaar"), 
      yaxis = list(title = "Percentage"),
      legend = list(y = 0.8, yanchor = "top")
  ) 
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  
  return(list(plot = pl, data = summaryData))
  
}



#' Shiny module for creating the plot \code{\link{boxRealisedShot}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams optionsModuleServer 
#' @inheritParams percentageRealisedShot
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
boxRealisedShotServer <- function(id, data, timeRange, types) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Gerealiseerd afschot
      callModule(module = optionsModuleServer, id = "boxRealisedShot", 
        timeRange = timeRange,
        types = types,
        multipleTypes = TRUE,
        data = data)
      callModule(module = plotModuleServer, id = "boxRealisedShot",
        plotFunction = "boxRealisedShot",
        data = data)
      
    })
  
}


#' Shiny module for creating the plot \code{\link{plotBioindicator}} - UI side
#' @param showAccuracy boolean, whether to show gauge for accuracy
#' @param regionLevels numeric vector, region level choices
#' @inherit welcomeSectionUI
#' 
#' @export
boxRealisedShotUI <- function(id, showAccuracy = FALSE, regionLevels = NULL, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    actionLink(inputId = ns("linkBoxRealisedShot"), 
      label = h3(HTML(uiText$title))
    ),
    conditionalPanel("input.linkBoxRealisedShot % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("boxRealisedShot"),
            showTime = TRUE, showType = TRUE, regionLevels = regionLevels,
            exportData = TRUE),
          tags$p(HTML(uiText[, id]))
        ),
        column(8, 
          plotModuleUI(id = ns("boxRealisedShot"))
        ),
        tags$hr()
      )
    )
  )
  
}



