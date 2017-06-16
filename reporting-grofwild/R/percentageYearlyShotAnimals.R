#' Create interactive plot to represent yearly percentage killed
#' 
#' Adapted version from Figure p. 13 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param jaartallen integer vector, defines the year(s) considered for the reference period
#' in the plot; if NULL no selection on year(s) is made
#' @param jaar year of interest
#' @param openingstijden vector with start and end of season, see output of
#' \link{loadOpeningstijdenData}
#' @param type animal type
#' @inheritParams countYearProvince
#' @return plotly object, for a given specie the observed yearly percentage
#' killed animals for the year \code{jaar}. 
#' The mean in the entire year is represented by a dotted line.
#' The range of the yearly percentage killed for the \code{jaartallen} period is represented
#' by a ribbon, and its median by a full line.
#' @import plotly
#' @import INBOtheme
#' @importFrom plyr ddply
#' @importFrom grDevices col2rgb
#' @export
percentageYearlyShotAnimals <- function(data, 
	wildNaam = "", 
	type = NULL,
	doodsoorzaak = "afschot",
	jaartallen = NULL, jaar = NULL,
	width = NULL, height = NULL,
	openingstijden = NULL) {

	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	if (is.null(jaar))
		jaar <- max(data$afschotjaar)

	# consider only animals shot in specified zaak
	inputData <- data[data$doodsoorzaak == doodsoorzaak, ]
	
	# only retains animals of specified type
	if(!is.null(type) && type != "all"){
		inputData <- data[data$type == type, ]
	}
	
	# format with half-month resolution
	formatDate <- function(x, format){
		xDate <- as.Date(x, format = format)
		xReformatted <- paste0(months(xDate), 
			" (", ifelse(format(xDate, format = "%d") < 15, "01", "02"), ")")
		return(xReformatted)
	}
	afschotDatumHalfMonth <- formatDate(inputData$afschot_datum, format = "%Y-%m-%d")
	allHalfMonth <- paste(
		rep(months(seq(as.Date("2000/1/1"), by = "month", length.out = 12)), each = 2),
		rep(c("(01)", "(02)"), times = 12))
	inputData$afschot_datum_halfMonth <- factor(afschotDatumHalfMonth, levels = allHalfMonth)
	
	# reformat opening time (season)
	if(!is.null(openingstijden)){
		openingstijdenHalfMonth <- formatDate(x = openingstijden, format = "%d/%m/%Y")
		openingstijdenIdx <- which(allHalfMonth %in% openingstijdenHalfMonth)-1
	}
	
	# compute percentage jaarlijks afschot
	dataPercShotInYear <- ddply(inputData, "afschotjaar", function(x){
			percShotInYear <- table(x$afschot_datum_halfMonth)/nrow(x)*100
			names(dimnames(percShotInYear)) <- "dateHalfMonth"
			as.data.frame(percShotInYear, responseName = "percShotInYear")
	})

	# format data for plot
	dataPlot <- ddply(dataPercShotInYear, "dateHalfMonth", function(x){
		obsYear <- x[x$afschotjaar == jaar, "percShotInYear"] # year observation
		xRange <- x[x$afschotjaar %in% jaartallen, "percShotInYear"] # range in reference perio
		cbind(obsYear = obsYear,
			medianRange = median(xRange),
			minRange = min(xRange), maxRange = max(xRange)
		)
	})

	# mean of year
	dataPlot$meanYear <- mean(dataPlot$obsYear)

	## create plot
	
	getNameRange <- function(name)
		paste0(name, " (", paste(range(jaartallen), collapse = "-"), ")")

	# ribbon color with transparency
	colorRibbon <- paste0("rgba(", paste(c(col2rgb(inbo.lichtblauw), "0.5"), collapse = ","), ")")
	
	# base plot
  pl <- plot_ly(dataPlot) %>%
			
			## specified range
			
			# min-max range
			add_ribbons(x = ~dateHalfMonth, 
					ymin = ~minRange, ymax = ~maxRange,
#					color = 'rgba(0,100,80,0.2)',
					fill = 'tonexty', fillcolor = colorRibbon,
					line = list(color = inbo.lichtblauw), 
					name = getNameRange("Min-Max")
			) %>%
			
			# median
			add_trace(x = ~dateHalfMonth, y = ~medianRange, 
					type = 'scatter', mode = 'lines',
					line =  list(color = inbo.lichtblauw, dash = "dot"), 
					name = getNameRange("Mediaan")
			) %>%
			
			## specified year
			
			# observation
			add_trace(x = ~dateHalfMonth, y = ~obsYear, 
					type = 'scatter', mode = 'markers+lines',
					line = list(color = inbo.steun.donkerroos), 
					name = paste0("Huidig geobserveerd (", as.character(jaar), ")")
			) %>%
		
			# mean in entire year
			add_lines(x = ~dateHalfMonth, y = ~ meanYear,
					name = paste0("Gemiddelde (", as.character(jaar), ")"), 
					line = list(color = inbo.steun.donkerroos, dash = "dot")
			)	%>%	
			
			## title axes and margin bottom
			layout(
				title = paste(wildNaam, "percentage jaarlijks afschot in", jaar),
				xaxis = list(title = ""), 
				yaxis = list(title = "Percentage jaarlijks afschot"),
				margin = list(b = 60, t = 40)
		)

		# add rectangle for openingstijden
		if(!is.null(openingstijden))
			pl <- pl %>% layout(
					shapes = list(
							list(type = "rect",
									fillcolor = inbo.lichtgrijs, line = list(color = inbo.lichtgrijs), 
									opacity = 0.2,
									x0 = openingstijdenIdx[1], x1 = openingstijdenIdx[2], 
									y0 = min(dataPlot[, 2:6]), y1 = max(dataPlot[, 2:6]),
									layer = "below"
									)
					)
			)
		# cannot add polygon: Can't display both discrete & non-discrete data on same axis
		#			pl %>% add_polygons(
		#				x = c(rep(openingstijdenIdx[1], 2), rep(openingstijdenIdx[2], 2)),
		#				y = rep(range(dataPlot[, 2:6]), times = 2),
		#				color = I("red"), opacity = 0.1)

	
		return(pl)

}