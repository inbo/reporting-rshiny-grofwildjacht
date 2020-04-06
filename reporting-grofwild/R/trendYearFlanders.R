

#' Create interactive plot for counts in Flanders per selected years
#' @param data data.frame, formatted data for plotting as returned by \code{\link{createTrendData}}
#' @inheritParams createSpaceData
#' @inheritParams trendYearRegion
#' @return list with plot and data
#' 
#' @author mvarewyck
#' @import plotly
#' @export
trendYearFlanders <- function(data, timeRange, unit = c("absolute", "relative"), schadeTitles = FALSE, 
		width = NULL, height = NULL) {
	
	
	unit <- match.arg(unit)
  
	wildNaam <- unique(data$wildsoort)
  title_wildnaam <- unlist(strsplit(wildNaam, split = ", "))
  
#	data$wildsoort <- NULL 
  
  titlePrefix <- if (!schadeTitles) "Gerapporteerd afschot" else "Evolutie schademeldingen"
	
	title <- paste0(titlePrefix,
			if (unit == "relative") "/100ha",
			
      " voor ", 
      if (length(title_wildnaam) > 3) paste0(paste(tolower(title_wildnaam[1:3]), collapse = ", "), ", ...") 
      else if (length(title_wildnaam) > 1) paste0(paste(tolower(title_wildnaam[1:length(title_wildnaam)-1]), collapse = ", "), " en ", tolower(title_wildnaam[length(title_wildnaam)]) )
      else tolower(wildNaam),
      
      "\nin Vlaanderen ",
			ifelse(timeRange[1] != timeRange[2],
					paste("van", timeRange[1], "tot", timeRange[2]),
					paste("in", timeRange[1])
			)
	)
	
	## Create plot
	toPlot <- plot_ly(data = data, x = ~afschotjaar, y = ~freq,
					color = ~locatie, hoverinfo = "x+y+name",
					type = "scatter", mode = "lines+markers",
					width = width, height = height) %>%
			layout(title = title,
					xaxis = list(title = "Jaar"), 
					yaxis = list(title = if (unit == "absolute") "Aantal" else "Aantal/100ha",
                       tickformat = if (unit == "absolute") ",d" else NULL,
                       rangemode = "nonnegative"),
					showlegend = TRUE,
					margin = list(b = 80, t = 100))
	
	# To prevent warnings in UI
	toPlot$elementId <- NULL
	
	# change variable names
	names(data)[names(data) == "freq"] <- if (unit == "absolute")
				"aantal" else "aantal/100ha"
	
	
	return(list(plot = toPlot, data = data))
	
}
