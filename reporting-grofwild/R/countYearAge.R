#' Create interactive plot for counts per age category and year
#' 
#' Figure p. 11 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearProvince
#' @param regio character vector, names of the selected regions in \code{data}
#' to be shown in the plot title
#' @param summarizeBy character, whether to summarize data in terms of counts or percentages
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the observed number 
#' per year and per age category is plotted in a stacked bar chart}
#' \item{'data'}{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'jaar': }{year at which the animals was counted}
#' \item{count, depending if \code{summarizeBy} is: }{
#' \itemize{
#' \item{'count': }{counts of animals in the 'freq' column}
#' \item{'percent': }{percentage of counts of animals in the 'percent'  column}
#' }
#' }
#' \item{'totaal': }{total number of animals across categories}
#' }
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @importFrom INBOtheme inbo.2015.colours inbo.lichtgrijs
#' @export
countYearAge <- function(data, wildNaam = "", jaartallen = NULL, regio = "",
    doodsoorzaak = "afschot", summarizeBy = c("count", "percent"),
    width = NULL, height = NULL) {
  
  
  summarizeBy <- match.arg(summarizeBy)
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  # Select data
  plotData <- data[data$afschotjaar %in% jaartallen &
          data$doodsoorzaak %in% doodsoorzaak, c("afschotjaar", "Leeftijdscategorie_onderkaak")]
  names(plotData) <- c("jaar", "kaak")
  
  # Percentage collected
  nRecords <- nrow(plotData)
  
  # Remove some categories
  plotData <- plotData[!is.na(plotData$jaar) & !is.na(plotData$kaak), ]
  
  # Define names and ordering of factor levels
  if ("Frisling" %in% plotData$kaak) {  # wild zwijn
    
    newLevelsKaak <- c("Frisling", "Overloper", "Volwassen", "Niet ingezameld")
    
  } else {  # ree
    
    newLevelsKaak <- c("Kits", "Jongvolwassen", "Volwassen", "Niet ingezameld")
    
  }
  
  # Summarize data per year and age category
  summaryData <- count(df = plotData, vars = names(plotData))
  
  
  # Add line for records with 0 observations
  fullData <- cbind(expand.grid(jaar = min(summaryData$jaar):max(summaryData$jaar),
          kaak = unique(summaryData$kaak)))
  summaryData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
  summaryData$freq[is.na(summaryData$freq)] <- 0
  
  
  # Calculate percentages excluding "niet ingezameld"
  kaak <- NULL  # to prevent warnings with R CMD check
  subData <- subset(summaryData, kaak != "Niet ingezameld")
  nCollected <- sum(subData$freq)
  percentCollected <- nCollected/nRecords
  freq <- NULL  # to prevent warnings with R CMD check 
  subData <- ddply(subData, "jaar", transform, 
      percent = freq / sum(freq) * 100)
  subDataMissing <- subset(summaryData, kaak == "Niet ingezameld")
  subDataMissing$percent <- NA
  summaryData <- rbind(subData, subDataMissing)
  
  # Summarize data per year
  totalCount <- count(df = plotData, vars = "jaar")
  totalCount$totaal <- totalCount$freq
  totalCount$freq <- NULL
  
  summaryData <- merge(summaryData, totalCount)
  
  
  # For optimal displaying in the plot
  summaryData$kaak <- factor(summaryData$kaak, levels = newLevelsKaak)
  summaryData$jaar <- as.factor(summaryData$jaar)
  
  if (summarizeBy == "count") {
    
    summaryData$text <- paste0("<b>", summaryData$kaak, " in ", summaryData$jaar, "</b>",
        "<br>Aantal: ", summaryData$freq, 
        "<br>Totaal: ", summaryData$totaal)
    
  } else {
    
    summaryData$text <- paste0("<b>", summaryData$kaak, " in ", summaryData$jaar, "</b>",
        "<br><i>Subset ingezamelde onderkaken </i>",
        "<br>", round(summaryData$percent), "%")
    
  }
	

  
  colors <- c(inbo.2015.colours(3), inbo.lichtgrijs)
  names(colors) <- newLevelsKaak
  
  title <- paste0(wildNaam, " ",
      ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
          paste("in", jaartallen)),
      " (", paste(doodsoorzaak, collapse = " en "), 
      if (!all(regio == "")) 
        paste(" in", paste(regio, collapse = " en ")),
      ")")
  
  
  
  # Create plot
  toPlot <- plot_ly(data = summaryData, x = ~jaar, 
          y = if (summarizeBy == "count") ~freq else ~percent, 
          color = ~kaak, text = ~text,  hoverinfo = "text+name",
          colors = colors, 
          type = "scatter", mode = "lines+markers", 
          width = width, height = height) %>%
      layout(title = title,
          xaxis = list(title = "Jaar"), 
          yaxis = if (summarizeBy == "count") 
                list(title = "Aantal") else 
                list(title = "Percentage", range = c(0, 100)),
          margin = list(b = 120, t = 100))     
  
  
  if (summarizeBy == "percent")
    toPlot <- toPlot %>% add_annotations(text = paste0(round(percentCollected, 2)*100, 
            "% ingezamelde onderkaken van totaal (", nCollected, "/", nRecords, ")"),
        xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
        y = -0.3, yanchor = "bottom", showarrow = FALSE)
  
  
	colsFinal <- 	colnames(summaryData)[
		!colnames(summaryData) %in% c("text", 
			if(summarizeBy == "count")	"percent"	else	c("freq", "totaal")
		)
	]
	
	return(list(plot = toPlot, data = summaryData[, colsFinal]))
  
}

