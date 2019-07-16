# Functions to plot the interactive map for flanders
# 
# Author: mvarewyck
###############################################################################



#' Create summary data of geographical data for selected year, species and region level
#' @param data data.frame, geographical data
#' @param spatialData sp, spatial data for the selected \code{regionLevel}
#' @param year integer, year of interest
#' @param species character, species of interest
#' @param regionLevel character, regional level of interest should be one of 
#' \code{c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes")}
#' @return data.frame
#' @author mvarewyck
#' @export
createSpaceData <- function(data, spatialData, year, species, regionLevel) {
	
	# Framework for summary data
	fullData <- cbind(expand.grid(
					wildsoort = species,
					afschotjaar = year,
					locatie = unique(spatialData$NAAM)
	))
	
	
	# Select subset for time
	plotData <- subset(data, subset = afschotjaar %in% year & wildsoort == species)
	
	# Create general plot data names
	plotData$locatie <- switch(regionLevel,
			flanders = "Vlaams Gewest",
			provinces = plotData$provincie, 
			communes = plotData$gemeente_afschot_locatie,
			faunabeheerzones = plotData$FaunabeheerZone,
			fbz_gemeentes = plotData$fbz_gemeente
	)
	# Exclude data with missing time or space
	plotData <- subset(plotData, !is.na(plotData$afschotjaar) & 
					!is.na(plotData$locatie) & plotData$locatie != "",
			c("afschotjaar", "locatie")
	)
	
	# Summarize data over years
	summaryData <- plyr::count(df = plotData, vars = names(plotData))
	
	# Add names & times with 0 observations
	
	allData <- merge(summaryData, fullData, all.x = TRUE, all.y = TRUE)
	allData$freq[is.na(allData$freq)] <- 0
	
	allData$afschotjaar <- as.factor(allData$afschotjaar)
	
	
	summaryData2 <- plyr::count(df = allData, vars = "locatie", wt_var = "freq")
	
	# Create group variable
	if (regionLevel %in% c("flanders", "provinces")) {
		
		otherBreaks <- sort(unique(summaryData2$freq))
		
		summaryData2$group <- cut(x = summaryData2$freq, 
				breaks = c(-Inf, otherBreaks),
				labels = otherBreaks) 
		
	} else if (regionLevel == "faunabeheerzones" & species == "Ree") {
		
		summaryData2$group <- cut(x = summaryData2$freq, 
					breaks = c(-Inf, 0, 100, 200, 500, 1000, Inf),
					labels = c("0", "1-100", "100-200", "200-500", "500-1000", ">1000")) 
		
	} else {
		
		if (species %in% c("Wild zwijn", "Ree"))
			summaryData2$group <- cut(x = summaryData2$freq, 
					breaks = c(-Inf, 0, 10, 20, 40, 80, Inf),
					labels = c("0", "1-10", "11-20", "21-40", "41-80", ">80")) else
			summaryData2$group <- cut(x = summaryData2$freq, 
					breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),
					labels = c("0", "1-5", "6-10", "11-15", "16-20", ">20"))
		
	}
	
	return(summaryData2)
	
	
}


mapColors <- function(summaryData) {
	
	
	
}


#' Create map for Flanders - color by incidence
#' @inheritParams createSpaceData
#' @param allSpatialData list of sp, spatial data for all spatial levels
#' @param summaryData data.frame, as returned by \code{\link{createSpaceData}}
#' @param colorScheme character vector, specifies the color palette for the different groups
#' in the summary data
#' @param legend character, legend placement; default is "none", no legend
#' @param addGlobe boolean, whether to add world map to background; default is FALSE 
#' @return leaflet map
#' @author mvarewyck
#' @importFrom leaflet leaflet addPolygons addPolylines
#' @export
mapFlanders <- function(
		regionLevel = c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes"), 
		spatialData, allSpatialData, summaryData, colorScheme, 
		legend = "none", addGlobe = FALSE) {
	
	
	palette <- colorFactor(palette = colorScheme, levels = levels(summaryData$group))
	
	valuesPalette <- summaryData[match(spatialData$NAAM, summaryData$locatie),
			"group"]
	
	if (any(!summaryData$locatie %in% spatialData$NAAM))
		stop("De geo-data kan niet gematcht worden aan de shape data.")
	
	
	myMap <- leaflet(spatialData) %>%
			
			addPolygons(
					weight = 1, 
					color = "gray",
					fillColor = ~ palette(valuesPalette),
					fillOpacity = 0.8,
					layerId = spatialData$NAAM,
					group = "region"
			) 
	
	# Add legend
	if (legend != "none") { 
		
		myMap <- addLegend(
				map = myMap,
				position = legend,
				pal = palette, 
				values = valuesPalette,
				opacity = 0.8,
				title = "Legende",
				layerId = "legend"
		)
		
	}
	
	
	# Add black borders
	if (regionLevel %in% c("communes", "fbz_gemeentes")) {
		
		borderRegion <- switch(regionLevel,
				"communes" = "provinces",
				"fbz_gemeentes" = "faunabeheerzones")  
		
		myMap <- addPolylines(map = myMap,
				data = allSpatialData[[borderRegion]], 
				color = "black", 
				weight = 3,
				opacity = 0.8,
				group = "borderRegion"
		)
		
	}
	
	if (addGlobe) {
		
		myMap <- addProviderTiles(myMap, "Hydda.Full")
		
	}
	
	
	myMap
	
}
