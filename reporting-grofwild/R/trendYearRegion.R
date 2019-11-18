#' Create data for plotting trend over years in \code{\link{trendYearFlanders}}
#' @inheritParams createSpaceData
#' @inheritParams trendYearRegion
#' @return data.frame, summary of number of animals per species, region and year.
#' Ready for plotting with \code{\link{trendYearFlanders}} 
#' @author mvarewyck
#' @export
createTrendData <- function(data, allSpatialData, 
        timeRange, species, regionLevel, unit = c("absolute", "relative")) {
    
    
    # Select correct spatial data
    if ("Wild zwijn" %in% species & regionLevel == "provinces") {
        
        spatialData <- allSpatialData[["provincesVoeren"]]
        
    } else {
        
        spatialData <- allSpatialData[[regionLevel]]
        
    }
    
    
    # Select subset for time
    chosenTimes <- timeRange[1]:timeRange[2]
    tmpData <- subset(data, afschotjaar %in% chosenTimes & wildsoort %in% species)
    
    # Create general plot data names
    plotData <- data.frame(
            wildsoort = species,
            afschotjaar = tmpData$afschotjaar)
    plotData$locatie <- switch(regionLevel,
            flanders = "Vlaams Gewest",
            provinces = tmpData$provincie,
            communes = tmpData$gemeente_afschot_locatie,
            faunabeheerzones = tmpData$FaunabeheerZone,
            fbz_gemeentes = tmpData$fbz_gemeente
    )
    
    # Exclude data with missing time or space
    plotData <- plotData[!is.na(plotData$afschotjaar) & 
                    !is.na(plotData$locatie) & plotData$locatie != "",]
    
    # Summarize data over years
    summaryData <- plyr::count(df = plotData, vars = names(plotData))
    
    # Add names & times with 0 observations
    fullData <- cbind(expand.grid(
                    wildsoort = species,
                    afschotjaar = chosenTimes,
                    locatie = unique(spatialData@data$NAAM)))
    # add Area
    fullData <- merge(fullData, spatialData@data[, c("NAAM", "AREA")],
            by.x = "locatie", by.y = "NAAM")
    
    allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
    allData$freq[is.na(allData$freq)] <- 0
    
    # unit taken into account
    if (unit == "relative")
        allData$freq <- allData$freq/allData$AREA 
    
    allData$AREA <- NULL
    
    allData$afschotjaar <- as.factor(allData$afschotjaar)
    
    
    return(allData)
    
}



#' Create interactive plot for counts per selected communes and years
#' 
#' @param data data.frame with raw data for plotting
#' @param locaties character vector, regions that were selected to plot
#' @param timeRange numeric vector, time range selected for plot
#' @inheritParams createSpaceData
#' @inheritParams countYearProvince
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given species the observed number 
#' per year and per selected commune is plotted in a line plot}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animals was shot}
#' \item{'locatie': }{comune name}
#' \item{'aantal' or 'aantal/100ha': }{absolute or relative counts of animals}
#' }
#' }
#' }
#' @import plotly
#' @importFrom INBOtheme inbo.2015.colours
#' @export
trendYearRegion <- function(data, locaties = NULL, timeRange = NULL, 
        unit = c("absolute", "relative"), 
		width = NULL, height = NULL) {
	
	
	# To prevent warnings with R CMD check
	locatie <- NULL
	
	unit <- match.arg(unit)
	wildNaam <- unique(data$wildsoort)
	
	
	if (is.null(locaties))
		stop("Gelieve regio('s) te selecteren")
	
	# Select data
	plotData <- subset(data, locatie %in% locaties)
	plotData$wildsoort <- NULL
	
	colors <- rev(inbo.2015.colours(n = length(locaties)))
	title <- paste("Gerapporteerd",
			if (unit == "absolute") "aantal" else "aantal/100ha",
			"voor", tolower(wildNaam), "\n",
			ifelse(timeRange[1] != timeRange[2],
					paste("van", timeRange[1], "tot", timeRange[2]),
					paste("in", timeRange[1])
			)
	)
	
	
	# Create plot
	pl <- plot_ly(data = plotData, x = ~afschotjaar, y = ~freq,
					color = ~locatie, colors = colors, 
					hoverinfo = "x+y+name",
					type = "scatter", mode = "lines+markers",
					width = width, height = height) %>%
			layout(title = title,
					xaxis = list(title = "Jaar"), 
					yaxis = list(title = if (unit == "absolute") "Aantal" else "Aantal/100ha"),
					showlegend = TRUE,
					margin = list(b = 80, t = 100))     
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	# change variable names
	names(data)[names(data) == "freq"] <- if (unit == "absolute")
				"aantal" else "aantal/100ha"
	
	
	return(list(plot = pl, data = plotData))
	
}

