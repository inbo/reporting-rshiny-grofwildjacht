# Project: grofWild_git
# 
# Author: mvarewyck
###############################################################################


#' Create interactive plot to represent yearly percentage killed and non-killed animals
#' 
#' Visualization of results obtained by tableProvince with categorie = typePercent
#' @inheritParams tableProvince 
#' @inheritParams percentageYearlyShotAnimals
#' @inheritParams countYearAge
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the realised percentage
#' killed animals out of assigned animals for the year \code{jaar}}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'jaar': }{the year}
#' \item{'aantal': }{the number of animals that were shot or not shot}
#' \item{'totaal': }{the number of assigned animals for the given year}
#' \item{'percent': }{percentage calculated as aantal/totaal*100}
#' \item{'class': }{character, whether the numbers refer to shot (gerealiseerd)
#' or non-shot (niet-gerealiseerd) animals}
#' }
#' }
#' }
#' @author mvarewyck
#' @import plotly
#' @import INBOtheme
#' @importFrom plyr count
#' @export
percentageRealisedShotAnimals <- function(data, assignedData,
		jaartallen = NULL, type = NULL,
		regio = "", summarizeBy = c("count", "percent"),
		width = NULL, height = NULL) {
	
	
	wildNaam <- unique(data$wildsoort)
	
	summarizeBy <- match.arg(summarizeBy)
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Data observed afschot
	plotData <- data[data$afschotjaar %in% jaartallen, c("afschotjaar", "type")]
	names(plotData) <- c("jaar", "type")
	
	# Data assigned afschot
	assignedData <- assignedData[, c("Labeltype", "Jaar", "Aantal")]
	names(assignedData) <- c("type", "jaar", "freq")
	assignedData <- count(df = assignedData, vars = c("type", "jaar"), wt_var = "freq")
	
	# Exclude records with jaar = NA
	plotData <- plotData[with(plotData, !is.na(jaar)), ]
	
	# Rename categorie NA to "Onbekend"
	plotData$type[is.na(plotData$type) | plotData$type == ""] <- "Onbekend"
	
	
	if (is.null(type))
		type <- c("geit", "bok", "kits")
	
	plotData <- plotData[plotData$type %in% type, ]
	plotData$type <- NULL
	
	assignedData <- assignedData[assignedData$type %in% type, ]
	assignedData$type <- NULL
	assignedData <- count(df = assignedData, vars = c("jaar"), wt_var = "freq")
	
	
	# Percentage collected
	nRecords <- nrow(plotData)
	
	# Summarize data per year and age category
	summaryData <- count(df = plotData, vars = names(plotData))
	names(summaryData)[names(summaryData) == "freq"] <- "observed" 
	
	summaryData <- merge(x = summaryData, y = assignedData, all = TRUE)
	summaryData$percent <- summaryData$observed/summaryData$freq*100
	
	
	
	# For optimal displaying in the plot
	summaryData$jaar <- as.factor(summaryData$jaar)
	
	# Create data for non-realised afschot
	allData <- rbind(
			cbind(summaryData, realised = TRUE),
			cbind(summaryData, realised = FALSE))
	allData$observed[!allData$realised] <- allData$freq[!allData$realised] - allData$observed[!allData$realised]
	allData$percent <- allData$observed/allData$freq*100
	allData$class <- factor(ifelse(allData$realised, "Gerealiseerd", "Niet-gerealiseerd"))
	
	
#  if (summarizeBy == "count") {
#    
#    summaryData$text <- paste0("<b>", summaryData$kaak, " in ", summaryData$jaar, "</b>",
#        "<br>Aantal: ", summaryData$freq, 
#        "<br>Totaal: ", summaryData$totaal)
#    
#  } else {
#    
#    summaryData$text <- paste0("<b>", summaryData$kaak, " in ", summaryData$jaar, "</b>",
#        "<br><i>Subset ingezamelde onderkaken </i>",
#        "<br>", round(summaryData$percent), "%")
#    
#  }
	
	
	
	colors <- c(inbo.2015.colours(1), inbo.lichtgrijs)
	names(colors) <- levels(allData$class)
	
	title <- paste0(wildNaam, " (", paste(type, collapse = ", "), ") ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)),
					paste("in", jaartallen)),
			if (!all(regio == ""))
				paste0("\n(in ", paste(regio, collapse = " en "), ")")
	)
	
	
	
	# Create plot
	toPlot <- switch(summarizeBy,
			count = plot_ly(data = allData, x = ~jaar, 
							y = ~observed, color = ~class, 
#              text = ~text, hoverinfo = "text+name",
							colors = colors, type = "bar",
							width = width, height = height) %>%
					layout(title = title,
							xaxis = list(title = "Jaar"), 
							yaxis = list(title = "Aantal"),
							barmode = "stack",
							margin = list(b = 120, t = 100),
							annotations = list(x = summaryData$jaar, 
									y = summaryData$observed, 
									text = ifelse(!is.na(summaryData$percent), 
											paste0(round(summaryData$percent), "%"), ""),
									xanchor = 'center', yanchor = 'bottom',
									showarrow = FALSE)),
			percent = plot_ly(data = allData, x = ~jaar, 
							y = ~percent, color = ~class, 
#              text = ~text, hoverinfo = "text+name",
							colors = colors, type = "scatter", mode = "lines+markers",
							width = width, height = height) %>%
					layout(title = title,
							xaxis = list(title = "Jaar"), 
							yaxis = list(title = "Percentage", range = c(0, 100)),
							margin = list(b = 120, t = 100))
	)
	
	
	allData$realised <- NULL
	names(allData)[names(allData) == "observed"] <- "aantal"
	names(allData)[names(allData) == "freq"] <- "totaal"
	
	
	# To prevent warnings in UI
	toPlot$elementId <- NULL
	
	
	return(list(plot = toPlot, data = allData))
	
}


