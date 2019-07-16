#' Create interactive plot for comparing length of lower jaw between age/gender category
#' 
#' Figure p.17 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearAge
#' @param type animal type, used to filter \code{data}, based on 'ageGender' column
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object with length lower jaw per 
#' age/gender category for specified \code{jaartallen}}
#' \item{'data': }{raw data used for the plot, as data.frame with:
#' \itemize{
#' \item{'ageGender': }{age/gender category}
#' \item{'onderkaaklengte': }{length of the lower jaw in mm}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @import plotly
#' @importFrom INBOtheme inbo.lichtgrijs
#' @importFrom plyr count
#' @export
boxAgeGenderLowerJaw <- function(data, 
		type, jaartallen = NULL, regio = "", width = NULL, height = NULL) {
	
	
	wildNaam <- unique(data$wildsoort)	
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Select data
	plotData <- data[
			# data of specified years
			data$afschotjaar %in% jaartallen, 
			c("onderkaaklengte_comp", "leeftijd_comp", "leeftijd_maanden", "geslacht.MF", "provincie")]
	names(plotData) <- c("onderkaaklengte", "leeftijd", "maanden", "geslacht", "provincie")
	
	
	# Percentage collected
	nRecords <- nrow(plotData)
	
	# Remove some categories
	# To prevent error with R CMD check
	leeftijd <- NULL
	onderkaaklengte <- NULL
	geslacht <- NULL
	plotData <- subset(plotData, !is.na(leeftijd) & leeftijd != "Niet ingezameld" &
					!is.na(onderkaaklengte) & geslacht != "Onbekend" & !is.na(geslacht))
	plotData$geslacht <- factor(plotData$geslacht)
	
	if (nrow(plotData) == 0)
		stop("Geen data beschikbaar")
	
	# Define names and ordering of factor levels
	if (wildNaam == "Wild zwijn") {  # wild zwijn
		
		plotData$leeftijd[plotData$leeftijd == "Frisling"] <- 
				ifelse(plotData$maanden[plotData$leeftijd == "Frisling"] < 6,
						"Frisling (<6m)", "Frisling (>6m)")
		
		newLevelsLeeftijd <- c("Frisling (<6m)", "Frisling (>6m)", "Overloper", "Volwassen")
		
	} else {  # ree
		
		newLevelsLeeftijd <- c("Kits", "Jongvolwassen", "Volwassen")
		
	}
	
	plotData$leeftijd <- factor(plotData$leeftijd, levels = newLevelsLeeftijd)
	
	plotData <- subset(plotData, leeftijd %in% type)
	
	if (nrow(plotData) == 0)
		stop("Geen data beschikbaar")
	
	
	# For optimal displaying in the plot
	colors <- inbo.2015.colours(n = 2)
	names(colors) <- unique(plotData$geslacht)
	
	totalCounts <- count(plotData, vars = c("leeftijd", "geslacht"))
	nIndices <- nrow(totalCounts)/2
	totalCounts$index[totalCounts$geslacht == "Mannelijk"] <- (-2/3 + seq_len(nIndices))/nIndices
	totalCounts$index[totalCounts$geslacht == "Vrouwelijk"] <- (-1/3 + seq_len(nIndices))/nIndices
	
	title <- paste0(wildNaam, " onderkaak lengte ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), 
					paste("in", jaartallen)), 
			if (!all(regio == "")) 
				paste0(" (", toString(regio), ")")) 
			
	# create plot
	pl <- plot_ly(data = plotData, x = ~leeftijd, y = ~onderkaaklengte,
					color = ~geslacht, colors = colors, type = "box", 
					width = width, height = height) %>%
			layout(title = title,
					xaxis = list(title = "Categorie"), 
					yaxis = list(title = "Onderkaaklengte (mm)"),
					margin = list(t = 100),
					boxmode = "group",
					annotations = list(x = totalCounts$index, 
							y = -diff(range(plotData$onderkaaklengte, na.rm = TRUE))/10, 
							xref = "paper", text = totalCounts$freq, xanchor = 'center', 
							yanchor = 'bottom', showarrow = FALSE)
			)
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(
					plot = pl, 
					data = subset(plotData, select = c("onderkaaklengte", "leeftijd", "geslacht", "provincie"))
	))
	
}
