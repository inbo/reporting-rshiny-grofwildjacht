#' Create interactive plot for comparing length of lower jaw between age/gender category
#' 
#' Figure p.17 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @inheritParams countYearAge
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
boxAgeGenderLowerJaw <- function(data, wildNaam = "", 
	jaartallen = NULL, regio = "", width = NULL, height = NULL) {
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Select data
	plotData <- data[
		# data of specified years
		data$afschotjaar %in% jaartallen &
		# with specified gender/age category
		data$ageGender != "" & !is.na(data$ageGender), ]

	plotData$ageGender <- droplevels(plotData$ageGender)
  
  # filter animal with left/right cheek length measured
	plotData <- plotData[!is.na(plotData$onderkaaklengte), ]
	# filter all averages < 100 and > 200
	plotData <- plotData[plotData$onderkaaklengte >= 100 & plotData$onderkaaklengte <= 200, ]
	
	# only keep columns displayed in the plot
	plotData <- plotData[, c("ageGender", "onderkaaklengte")]
  
  if (nrow(plotData) == 0)
    stop("Geen data beschikbaar")
  
  
  totalCounts <- count(plotData, vars = c("ageGender"))
  totalCounts$index <- (seq_along(totalCounts$ageGender) - 1/2)/nrow(totalCounts) 
	
	# create plot
	pl <- plot_ly(data = plotData, x = ~ageGender, y = ~onderkaaklengte,
			colors = inbo.lichtgrijs, type = "box", width = width, height = height) %>%
			layout(title = paste0(wildNaam, " onderkaak lengte ",
					ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
					if (!all(regio == "")) paste0(" (", toString(regio), ")")),
					xaxis = list(title = "Categorie op basis van leeftijdscategorie and geslacht"), 
					yaxis = list(title = "Onderkaaklengte (mm)"),
					margin = list(b = 40, t = 100),
          annotations = list(x = totalCounts$index, 
              y = -diff(range(plotData$onderkaaklengte, na.rm = TRUE))/10, 
              xref = "paper", text = totalCounts$freq, xanchor = 'center', 
              yanchor = 'bottom', showarrow = FALSE)
			)
  
  # To prevent warnings in UI
  pl$elementId <- NULL

  
	return(list(plot = pl, data = plotData))
	
}
