# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



#' Create interactive plot for comparing age per gender
#' 
#' Figure p. 15 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countAgeCheek
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the percentage per age category
#' and per gender, based on meldingsformulier data in a stacked bar chart}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'geslacht': }{gender}
#' \item{'leeftijd': }{age category}
#' \item{'freq': }{counts of animals}
#' \item{'percent': }{percentage of counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom plyr count ddply
#' @importFrom INBOtheme inbo.2015.colours inbo.lichtgrijs
#' @author mvarewyck
#' @export
countAgeGender <- function(data, jaartallen = NULL, 
		width = NULL, height = NULL) {
	
	
	wildNaam <- unique(data$wildsoort)
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Select data
	plotData <- data[data$afschotjaar %in% jaartallen, 
				c("geslacht.MF", "leeftijd_comp")]
	names(plotData) <- c("geslacht", "leeftijd")
	
	# Percentage collected
	nRecords <- nrow(plotData)
	
	# Remove some categories
	# To prevent error with R CMD check
	leeftijd <- NULL
	geslacht <- NULL
	plotData <- subset(plotData, !is.na(geslacht) & geslacht != "Onbekend" &
					!is.na(leeftijd) & leeftijd != "Onbekend" & leeftijd != "Niet ingezameld")
	percentCollected <- nrow(plotData)/nRecords
	
	# Define names and ordering of factor levels
	if (wildNaam == "Wild zwijn") {  # wild zwijn
		
		newLevelsLeeftijd <- c("Frisling", "Overloper", "Volwassen")
		
	} else {  # ree
		
		newLevelsLeeftijd <- c("Kits", "Jongvolwassen", "Volwassen")
		
	}
	
	
	# Summarize data per province and year
	summaryData <- count(df = plotData, vars = names(plotData))
	freq <- NULL  # to prevent warnings with R CMD check 
	summaryData <- ddply(summaryData, "leeftijd", transform, 
			percent = freq / sum(freq) * 100)
	
	
	
	# For optimal displaying in the plot
	summaryData$leeftijd <- factor(summaryData$leeftijd, levels = newLevelsLeeftijd)
	
	summaryData$text <- paste0(round(summaryData$percent), "%",
			" (", summaryData$freq, ")")
	
	totalCount <- count(df = summaryData, vars = "leeftijd", wt_var = "freq")$freq
	
	colors <- inbo.2015.colours(n = nlevels(as.factor(summaryData$geslacht)))
	title <- paste(wildNaam, paste0("(", 
					ifelse(length(jaartallen) > 1, paste(min(jaartallen), "tot", max(jaartallen)),
							jaartallen), ")"))
	
	
	# Create plot
	pl <- plot_ly(data = summaryData, x = ~leeftijd, y = ~percent, color = ~geslacht,
					text = ~text,  hoverinfo = "x+text+name",
					colors = colors, type = "bar",  width = width, height = height) %>%
			
			layout(title = title,
					xaxis = list(title = "Leeftijdscategorie (INBO of Meldingsformulier)"), 
					yaxis = list(title = "Percentage"),
					legend = list(y = 0.8, yanchor = "top"),
					margin = list(b = 120, t = 100), 
					barmode = "stack",
					shapes = list(type = "line", line = list(color = inbo.lichtgrijs, dash = "dash"), 
							xref = "paper", x0 = 0, x1 = 1, y0 = 50, y1 = 50),
					annotations = list(x = levels(summaryData$leeftijd), y = -10, 
							text = totalCount, xanchor = 'center', yanchor = 'bottom', 
							showarrow = FALSE)) %>%
			
			add_annotations(text = "Geslacht", 
					xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
					y = 0.8, yanchor = "bottom",    # Same y as legend below
					legendtitle = TRUE, showarrow = FALSE) %>%
			
			add_annotations(text = paste0(round(percentCollected, 2)*100, 
							"% met gekende leeftijd en geslacht (", nrow(plotData), "/", nRecords, ")"),
					xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
					y = -0.3, yanchor = "bottom", showarrow = FALSE)  
	
	colsFinal <- colnames(summaryData)[colnames(summaryData) != "text"]
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(plot = pl, data = summaryData[, colsFinal]))
	
	
}

