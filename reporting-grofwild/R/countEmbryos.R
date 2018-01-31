#' Create interactive plot for number of embryos versus year
#' 
#' Adapted version from Figure p. 30 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param type animal type, used to filter \code{data} ('ageGender' column)
#' default is \code{c("Smalree", "Geit")}
#' @inheritParams countYearAge
#' @import plotly
#' @importFrom INBOtheme inbo.2015.colours
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for the specified specie and years}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animal was shot}
#' \item{'variable': }{age/gender category}
#' \item{'Freq': }{counts of females}
#' \item{'percent': }{percentage of females with given number of embryos per year}
#' }}
#' }
#' @author mvarewyck
#' @export
countEmbryos <- function(data, wildNaam = "", type = c("Smalree", "Geit"), 
    jaartallen = NULL, regio = "", width = NULL, height = NULL) {
  
  
  bioindicator <- "aantal_embryos"
  bioindicatorName <- "aantal embryo's"
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  
  # Select data of specified years and type
  plotData <- data[data$afschotjaar %in% jaartallen & data$ageGender %in% type,
      c("afschotjaar", bioindicator)]
  
  colnames(plotData)[colnames(plotData) == bioindicator] <- "variable"
  
  ## For aantal_embryos
  # remove > 3 embryos
  plotData <- plotData[is.na(plotData$variable) | plotData$variable <= 3, ]
  
  # replace NA by 'Niet ingevuld', convert to a factor
  plotData$variable[is.na(plotData$variable)] <- "Niet ingevuld"
  newLevels <- rev(c("Niet ingevuld", "3", "2", "1", "0"))
  plotData$variable <- factor(plotData$variable, levels = newLevels)
  
  plotData$afschotjaar <- as.factor(plotData$afschotjaar)
  
  # use table with factor to have 0 when no counts for certain year/number of embryos
  tmpSummary <- as.data.frame(with(plotData, table(afschotjaar, variable)))
  
  # Calculate percentages excluding "niet ingezameld"
  variable <- NULL  # to prevent warnings with R CMD check
  Freq <- NULL
  subData <- subset(tmpSummary, variable != "Niet ingevuld")
  tmpPercent <- ddply(subData, "afschotjaar", transform, 
      percent = Freq / sum(Freq) * 100)
  summaryData <- merge(tmpPercent, tmpSummary, all.y = TRUE)
  
  # Hover text
  summaryData$text <- ifelse(is.na(summaryData$percent), "",
      paste0(round(summaryData$percent), "%"))
  
  
  if (sum(summaryData$Freq) == 0)
    stop("Geen data beschikbaar")
  
  # Summarize data per year
  totalCounts <- table(plotData$afschotjaar)
  
  
  title <- paste0(wildNaam, " ", bioindicatorName, " ",
      ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
      if (!all(regio == "")) paste0(" (", toString(regio), ")"))
  
  
  colors <- c(inbo.2015.colours(nlevels(summaryData$variable) - 1), inbo.lichtgrijs)
  names(colors) <- newLevels
  
  
  pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~Freq, color = ~variable,
          text = ~text, hoverinfo = "x+text+name",
          colors = colors, type = "bar",  width = width, height = height) %>%
      
      layout(title = title,
          xaxis = list(title = "afschotjaar"), 
          yaxis = list(title = "Aantal vrouwelijke ree\u00EBn"),
          margin = list(b = 40, t = 100),
          legend = list(y = 0.8, yanchor = "top"),
          barmode = if(length(totalCounts) == 1) "group" else "stack",
          annotations = list(x = as.numeric(names(totalCounts)), y = totalCounts, 
              text = paste(if(length(totalCounts) == 1) "totaal:" else "", totalCounts),
              xanchor = 'center', yanchor = 'bottom', showarrow = FALSE)) %>%
      
      add_annotations(text = "Aantal embryo's", 
          xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
          y = 0.8, yanchor = "bottom",    # Same y as legend below
          legendtitle = TRUE, showarrow = FALSE)
  
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  summaryData$text <- NULL
  
  
  return(list(plot = pl, data = summaryData))
  
}