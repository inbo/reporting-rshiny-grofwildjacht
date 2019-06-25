#' Create interactive plot for counts per selected communes and years
#' 
#' @param data data.frame with raw data for plotting
#' @param locaties character vector, regions that were selected to plot
#' @param timeRange numeric vector, time range selected for plot
#' @inheritParams countYearProvince
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the observed number 
#' per year and per selected commune is plotted in a line plot}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animals was shot}
#' \item{'locatie': }{comune name}
#' \item{'aantal': }{counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom INBOtheme inbo.2015.colours
#' @export
countYearRegion <- function(data, locaties = NULL, timeRange = NULL,
		width = NULL, height = NULL) {
	
	
	# To prevent warnings with R CMD check
	locatie <- NULL
	
	wildNaam <- unique(data$wildsoort)
	
	
	if (is.null(locaties))
		stop("Gelieve regio('s) te selecteren")
	
	# Select data
	plotData <- subset(data, locatie %in% locaties)
	plotData$wildsoort <- NULL
	
	colors <- rev(inbo.2015.colours(n = length(locaties)))
	title <- paste("Gerapporteerd aantal voor", tolower(wildNaam),
			ifelse(timeRange[1] != timeRange[2],
					paste("van", timeRange[1], "tot", timeRange[2]),
					paste("in", timeRange[1])
			)
	)
	
	
	# Create plot
	pl <- plot_ly(data = plotData, x = ~afschotjaar, y = ~aantal,
					color = ~locatie, hoverinfo = "x+y+name",
					type = "scatter", mode = "lines+markers",
					width = width, height = height) %>%
			layout(title = title,
					xaxis = list(title = "Jaar"), 
					yaxis = list(title = "Aantal"),
					showlegend = TRUE,
					margin = list(b = 80, t = 100))     
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(plot = pl, data = plotData))
	
}

