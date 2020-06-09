#' Create interactive plot to represent yearly percentage killed
#' 
#' Adapted version from Figure p. 13 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param jaartallen integer vector, defines the year(s) considered for the reference period
#' in the plot; if NULL no selection on year(s) is made
#' @param jaar year of interest
#' @param openingstijdenData data.frame with opening season, 
#' as returned by the link{loadOpeningstijdenData} function
#' @param type animal type, used to filter \code{data} and \code{openingstijdenData} ('type' column)
#' If NULL (by default) or 'all', the data is not filtered.
#' @inheritParams countYearProvince
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for a given specie the observed yearly percentage
#' killed animals for the year \code{jaar}. 
#' The mean in the entire year is represented by a dotted line.
#' The range of the yearly percentage killed for the \code{jaartallen} period is represented
#' by a ribbon, and its median by a full line.}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'dateHalfMonth': }{date in half-month resolution,
#' e.g. January (01) for 01/01 -> 14/01 and January (02) for 15/01 -> end month (31/01)
#' }
#' \item{'obsYear': }{observed percentage of the counts for the specific half-month}
#' }
#' }
#' }
#' @import plotly
#' @import INBOtheme
#' @importFrom plyr ddply
#' @importFrom grDevices col2rgb
#' @importFrom stats median
#' @export
percentageYearlyShotAnimals <- function(
		data, openingstijdenData = NULL,
		type = NULL,
		jaartallen = NULL, jaar = NULL,
		width = NULL, height = NULL) {
	
	
	wildNaam <- unique(data$wildsoort)
	
	if (is.null(openingstijdenData))
		stop("Geen openingstijden data beschikbaar.")
	
	## default
	
	if (is.null(jaartallen))
		jaartallen <- unique(openingstijdenData$Jaar)
	
	if (is.null(jaar))
		jaar <- max(openingstijdenData$Jaar)
	
	## checks
	
	# check if specified year/year range are in the data
	# and in the opening season
	if(! all(jaartallen %in% openingstijdenData$Jaar)) 
		stop("Niet voor alle jaartallen zijn er observaties in de openingstijden data.")
	if(! jaar %in% data$afschotjaar) 
		stop("Geen data voor het gekozen jaar.")
	if(! jaar %in% openingstijdenData$Jaar) 
		stop("Geen data voor het gekozen jaar in de openingstijden data.")
	
	
	## filtering
	
	# consider only animals shot in specified zaak
	inputData <- data
	
	# only retains animals of specified type
	specifiedType <- !is.null(type) && type != "all"
	if(specifiedType){
        
        inputData$type <- ifelse(inputData$wildsoort != "Ree",
                "", ifelse(grepl("kits", inputData$type_comp), "kits",
                        ifelse(inputData$geslacht.MF == "Mannelijk", "bok", "geit")))
        
		inputData <- inputData[inputData$type %in% type, ]
		openingstijdenData <- openingstijdenData[openingstijdenData$Type %in% type, ]
        
	}
	
	## Not allow for multiple types
	if(length(unique(openingstijdenData$Type)) > 1)
		stop("Meerdere types in openingstijden data.")
	
	# only retains counts with no missing afschot_datum
	# select jaartallen
	inputData <- inputData[!is.na(inputData$afschot_datum) &
					inputData$afschotjaar %in% c(jaar, jaartallen), ]
	
	
	## format date to factor with half-month resolution
	
	# reformat dates as 'Date' object
	inputData$afschot_datum_Date <- as.Date(inputData$afschot_datum, format = "%Y-%m-%d")
	openingstijdenData$Startdatum_Date <- as.Date(openingstijdenData$Startdatum, format = "%d/%m/%Y")
	openingstijdenData$Stopdatum_Date <- as.Date(openingstijdenData$Stopdatum, format = "%d/%m/%Y")
	
	# only retain data of opening season for each year
	inputDataFilter <- ddply(inputData, "afschotjaar", function(x){
				year <- unique(x$afschotjaar)
				openingseasonInfo <- openingstijdenData[openingstijdenData$Jaar == year, ]
				idxOpeningSeason <- which(
						x$afschot_datum_Date >= openingseasonInfo$Startdatum_Date & 
								x$afschot_datum_Date <= openingseasonInfo$Stopdatum_Date
				)
				x[idxOpeningSeason, ]
			})
	
	# format with half-month resolution
	formatDate <- function(x, dateSeparation = 14){
		xDate <- as.Date(x, format)
		xReformatted <- paste0(months(xDate), 
				" (", ifelse(format(xDate, format = "%d") <= dateSeparation, "01", "02"), ")")
		return(xReformatted)
	}
	
	# format date in data
	afschotDatumHalfMonth <- formatDate(inputDataFilter$afschot_datum_Date)
	
	# extract available formatted date (the ones in opening season of specified year)
	allHalfMonth <- paste(
			rep(months(seq(as.Date("2000/1/1"), by = "month", length.out = 12)), each = 2),
			rep(c("(01)", "(02)"), times = 12))
	
	# format opening season
	# opening season: if 15/%m/%y is in start -> second part of the month
	# if 15/%m/%y is in end -> first part of the month
	openingSeasonYear <- unique(c(
					formatDate(openingstijdenData[openingstijdenData$Jaar == jaar, "Startdatum_Date"], dateSeparation = 14),
					formatDate(openingstijdenData[openingstijdenData$Jaar == jaar, "Stopdatum_Date"], dateSeparation = 15)
			))
	
#  # only consider half month in opening season
	openingSeasonHalfMonth <- allHalfMonth[do.call(':', as.list(which(allHalfMonth %in% openingSeasonYear)))]
	# consider all half months
#openingSeasonHalfMonth <- allHalfMonth

  # reformat date in data for case where afschotdatum equals a stopdatum of openingstijden
  # in such cases format afschotdatum with dateseparation = 15 (in stead of 14 as done before)
  # i.e. if 15/%m/%y is afschotdatum and also a stopdatum of openingstijden -> include it in first part of the month
  indices <- which(inputDataFilter$afschot_datum_Date %in% openingstijdenData[openingstijdenData$Jaar %in% c(jaar, jaartallen), "Stopdatum_Date"])
  afschotDatumHalfMonth[indices] <- formatDate(inputDataFilter$afschot_datum_Date[indices], 15)
  
# format date as factor
	inputDataFilter$afschot_datum_halfMonth <- factor(afschotDatumHalfMonth, levels = allHalfMonth)
	
	## compute statistics for plot
	
	# compute percentage across opening season
	dataPercShotInYear <- ddply(inputDataFilter, "afschotjaar", function(x){
				# table returns 0 if counts not present for a certain half month
				percShotInYear <- table(x$afschot_datum_halfMonth)/nrow(x)*100
				names(dimnames(percShotInYear)) <- "dateHalfMonth"
				as.data.frame(percShotInYear, responseName = "percShotInYear")
			})
	
	# format data for plot: extract percentage for observed year
	# median, min and max in reference period
	dataPlot <- ddply(dataPercShotInYear, "dateHalfMonth", function(x){
				obsYear <- if (!jaar %in% x$afschotjaar) # year observation
					NA else
					x[x$afschotjaar == jaar, "percShotInYear"] 
				xRange <- x[x$afschotjaar %in% jaartallen, "percShotInYear"] # range in reference period
				cbind(obsYear = obsYear,
						medianRange = median(xRange),
						minRange = min(xRange), maxRange = max(xRange)
				)
			})
	
	# extract mean percentage in selected year - during opening season
	dataPlot$meanYear <- mean(dataPlot$obsYear[dataPlot$dateHalfMonth %in% openingSeasonHalfMonth])
	
  # Translate english month names to dutch
  dataPlot$dateHalfMonth <- setMonthsInDutch(dataPlot$dateHalfMonth)
	
	## create plot
	
	# format specified time range
	getNameRange <- function(name)
		paste0(name, " (", paste(range(jaartallen), collapse = "-"), ")")
	
	# Error bar color with transparency
  colorErrorbar <- paste0("rgba(", paste(c(col2rgb(inbo.lichtblauw), "0.75"), collapse = ","), ")")
  colorErrorbarLine <- paste0("rgba(", paste(c(col2rgb(inbo.grijsblauw), "0.85"), collapse = ","), ")")
  
  # ribbon color with transparency
  colorRibbon <- paste0("rgba(", paste(c(col2rgb(inbo.steun.donkerroos), "0.15"), collapse = ","), ")")
  colorRibbonLine <- paste0("rgba(", paste(c(col2rgb(inbo.steun.donkerroos), "0.25"), collapse = ","), ")")
  
  # for open-ended ribbons include only months with non-NA data
  # and add this layer separately
  referenceData <- subset(dataPlot, 
                    subset = c(!(dataPlot$medianRange == 0 & dataPlot$maxRange == 0 &
                                 dataPlot$ minRange == 0 & dataPlot$obsYear == 0)))

  # new version plot
#  pl2 <- plot_ly(data = dataPlot,
#                 width = width, height = height) %>%
#            
#            # add bars
#            add_trace(type = "bar",
#                x = ~dateHalfMonth, y = ~obsYear,
#                marker = list(color = colorErrorbar,
#                              line = list(color = colorErrorbarLine,
#                                          width = 1.5)),
#                name = paste0("Huidig geobserveerd (", as.character(jaar), ")")
#            ) %>%
#            
#            # median
#            add_trace(x = ~dateHalfMonth, y = ~medianRange, 
#                type = 'scatter', mode = 'lines',
#                line = list(color = inbo.steun.donkerroos,
#                    dash = "dot"), #size = 6, , width = 2 (default) 
#                name = getNameRange("Mediaan"),
#                yaxis = 'y2'
#            ) %>%
#            
#            # min-max range
#            add_ribbons(x = ~dateHalfMonth, 
#                ymin = ~minRange, ymax = ~maxRange,
#                #					color = 'rgba(0,100,80,0.2)',
#                fill = 'tonexty', fillcolor = colorRibbon,
#                line = list(color = colorRibbonLine, width = 2),
#                name = getNameRange("Min-Max"),
#                yaxis = 'y2'
#            ) %>%
#             
#              # layout
#              layout(
#                  title = paste0(wildNaam, 
#                      if (specifiedType)	paste0(" (", paste(type, collapse = ", "), ")"),
#                      " percentage jaarlijks afschot in ", jaar),
#                  xaxis = list(title = "openingstijd (half-maand resolutie)", tickangle = -90,
#                               titlefont = list(size = 18)), 
#                  yaxis = list(title = "Percentage jaarlijks afschot", 
#                               titlefont = list(size = 18),
#                               range = c(0, max(dataPlot[, c("obsYear", "maxRange")])*1.05),
#                               overlaying = "y2"),
#                  yaxis2 = list(title = "",
#                               range = c(0, max(dataPlot[, c("obsYear", "maxRange")])*1.05)),
#                  margin = list(b = 70, t = 90),
#                  legend = list(orientation = "h", y = 100, x = 0.1),
#                  showlegend = TRUE
#                  )
              
    # open-ended ribbons plot
    pl <- plot_ly(data = dataPlot,
            width = width, height = height) %>%
        
        # add bars
        add_trace(type = "bar",
            x = ~dateHalfMonth, y = ~obsYear,
            marker = list(color = colorErrorbar,
                line = list(color = colorErrorbarLine,
                    width = 1.5)),
            name = paste0("Huidig geobserveerd (", as.character(jaar), ")")
        ) %>%
        
        # median
        add_trace(inherit = FALSE, data = referenceData, x = ~dateHalfMonth, y = ~medianRange, 
            type = 'scatter', mode = 'lines',
            line = list(color = inbo.steun.donkerroos,
                dash = "dot"), #size = 6, , width = 2 (default) 
            name = getNameRange("Mediaan"),
            yaxis = 'y2'
        ) %>%
        
        # min-max range
        add_ribbons(inherit = FALSE, data = referenceData, x = ~dateHalfMonth, 
            ymin = ~minRange, ymax = ~maxRange,
            #					color = 'rgba(0,100,80,0.2)',
            fill = 'tonexty', fillcolor = colorRibbon,
            line = list(color = colorRibbonLine, width = 2),
            name = getNameRange("Min-Max"),
            yaxis = 'y2'
        ) %>%
        
        # layout
        layout(
            title = paste0(wildNaam, 
                if (specifiedType)	paste0(" (", paste(type, collapse = ", "), ")"),
                " percentage jaarlijks afschot in ", jaar),
            xaxis = list(title = "openingstijd (half-maand resolutie)", tickangle = -90,
                titlefont = list(size = 18)), 
            yaxis = list(title = "Percentage jaarlijks afschot", 
                titlefont = list(size = 18),
                range = c(0, max(dataPlot[, c("obsYear", "maxRange")])*1.05),
                overlaying = "y2"),
            yaxis2 = list(title = "",
                range = c(0, max(dataPlot[, c("obsYear", "maxRange")])*1.05)),
            margin = list(b = 70, t = 90),
            legend = list(orientation = "h", y = 100, x = 0.1),
            showlegend = TRUE
        )
    
              
  colsOfInterest <- c("dateHalfMonth", "obsYear", "medianRange", "minRange", "maxRange")
  newColNames <-    c("Datum (half-maand resolutie)", 
                      "Geobserveerd percentage", 
                      getNameRange("Mediaan"),
                      getNameRange("Min"),
                      getNameRange("Max"))
  dataReturn <- dataPlot[, colsOfInterest]
  colnames(dataReturn) <- newColNames
  
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(plot = pl, data = dataReturn))
	
}