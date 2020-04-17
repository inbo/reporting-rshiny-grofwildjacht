#' Create interactive plot for counts per native region and year of introduction
#' 
#' Based on \code{\link{countYearProvince}} plot from grofwild
#' @param type character, native_range level of interest should be one of 
#' \code{c("native_continent", "native_range")} 
#' @inheritParams countYearProvince
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given specie the observed number 
#' per year and per province is plotted in a stacked bar chart}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'year': }{year at which the animal was introduced}
#' \item{'nativeRange': }{native range of the introduced animal}
#' \item{'aantal': }{counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom reshape2 melt
#' @importFrom INBOtheme inbo.2015.colours
#' @export
countYearNativerange <- function(data, jaartallen = NULL, 
    type = c("native_continent", "native_range"),
    width = NULL, height = NULL) {
  
  type <- match.arg(type)
  
  if (is.null(jaartallen))
    jaartallen <- sort(unique(data$first_observed))
  
  plotData <- data
  plotData$locatie <- switch(type,
      native_range = plotData$native_range,
      native_continent = plotData$native_continent
  )
  
  # Select data
  plotData <- plotData[plotData$first_observed %in% jaartallen, c("first_observed", "locatie")]
  plotData <- plotData[!is.na(plotData$first_observed) & !is.na(plotData$locatie), ]
  
  # Exclude unused provinces
  plotData$locatie <- as.factor(plotData$locatie)    
  plotData$locatie <- droplevels(plotData$locatie)
  
  # Summarize data per native_range and year
  plotData$first_observed <- with(plotData, factor(first_observed, levels = 
              min(jaartallen):max(jaartallen)))
  
  summaryData <- melt(table(plotData), id.vars = "first_observed")
  
  # Summarize data per year
  totalCount <- table(plotData$first_observed)
  
  
  # For optimal displaying in the plot
  summaryData$locatie <- as.factor(summaryData$locatie)
  summaryData$locatie <- factor(summaryData$locatie, levels = rev(levels(summaryData$locatie)))
  summaryData$first_observed <- as.factor(summaryData$first_observed)
  
  colors <- rev(inbo.2015.colours(n = nlevels(summaryData$locatie)))
  title <- yearToTitleString(year = c(jaartallen[1], tail(jaartallen, 1)), brackets = FALSE)
  
  # Create plot
  pl <- plot_ly(data = summaryData, x = ~first_observed, y = ~value, color = ~locatie,
          colors = colors, type = "bar",  width = width, height = height) %>%
      layout(title = title,
          xaxis = list(title = "Jaar"), 
          yaxis = list(title = "Aantal"),
          margin = list(b = 80, t = 100), 
          barmode = ifelse(nlevels(summaryData$first_observed) == 1, "group", "stack"),
          annotations = list(x = levels(summaryData$first_observed), 
              y = totalCount, 
              text = paste(ifelse(nlevels(summaryData$first_observed) == 1, "totaal:", ""), ifelse(totalCount > 0, totalCount, "")),
              xanchor = 'center', yanchor = 'bottom',
              showarrow = FALSE))  
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  # Change variable name
  names(summaryData)[names(summaryData) == "value"] <- "aantal"
  names(summaryData)[names(summaryData) == "first_observed"] <- "jaar"
  names(summaryData)[names(summaryData) == "locatie"] <- "regio van oorsprong"
    
  return(list(plot = pl, data = summaryData))
  
}
