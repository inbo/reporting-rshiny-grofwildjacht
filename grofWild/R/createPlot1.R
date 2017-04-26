#' Create interactive version of plot at page 4
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return plotly object
#' @import plotly
#' @importFrom stats reshape
#' @export
createPlot1 <- function(width = NULL, height = NULL) {
	
	voeren <- c(1, 7, 12, 26, 52, 28, 29, 34, 68)
	limburg <- c(NA, 20, 28, 68, 91, 190, 400, 444, 
			410)
	antwerpen <- c(NA, NA, NA, 3, NA, NA, 8, 15, 38)
	vlaamsBrabant <- c(NA, 1, 2, 1, 1, 1, 4, 6, 0)
	oostVlanderen <- c(NA, 1, 1, 2, 1, 2, 0, 2, 0)
	westVlanderen <- c(20, 15, 7, 5, 8, 41, 78, 7)
	
	data <- rbind.data.frame(voeren, limburg, antwerpen, 
			vlaamsBrabant, oostVlanderen, westVlanderen)
	colnames(data) <- 2006:2014
	rownames(data) <- c("Voeren", "Limburg", "Antwerpen", 
			"Vlaams-Brabant", "Oost-Vlaanderen", "West-Vlaanderen")
	
	# plotly
	plotData <- reshape(data = data, varying = list(as.character(colnames(data))), 
			v.names = "aantal", ids = rownames(data), direction = "long")
	plotData$time <- as.character(colnames(data))[plotData$time]
	rownames(plotData) <- NULL
	plot_ly(x = plotData$time, y = plotData$aantal, color = plotData$id,
					type = "scatter", mode = "lines+markers",
					width = width, height = height) %>% 
			layout(xaxis = list(title = "Jaar"), 
					yaxis = list(title = "Aantal everzwijnen (afschot en valwild)"),
					margin = list(b = 120))  
	
}
