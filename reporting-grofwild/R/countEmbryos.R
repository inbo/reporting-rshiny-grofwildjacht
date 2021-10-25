#' Create interactive plot for number of embryos versus year
#' 
#' Adapted version from Figure p. 30 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param type animal type, used to filter \code{data} ('ageGender' column)
#' default is \code{c("Smalree", "Geit")}
#' @param sourceIndicator character, which source to be used; default value is "inbo"
#' @inheritParams countYearAge
#' @import plotly
#' @importFrom INBOtheme inbo_palette
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for the specified specie and years}
#' \item{'data': }{data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animal was shot}
#' \item{'embryos': }{aantal embryos}
#' \item{'Freq': }{counts of females}
#' \item{'percent': }{percentage of females with given number of embryos per year}
#' }}
#' }
#' @author mvarewyck
#' @export
countEmbryos <- function(data, type = c("Smalree", "Reegeit"), 
		jaartallen = NULL, regio = "", 
        sourceIndicator = c("inbo", "meldingsformulier", "both"),
        width = NULL, height = NULL) {
    
    
    # to prevent warnings with R CMD check
    embryos <- NULL  
    Freq <- NULL
	
	wildNaam <- unique(data$wildsoort)
	
	bioindicator <- c("aantal_embryos", "aantal_embryos_labo", "aantal_embryos_MF")
    bioindicatorName <- "aantal embryo's"
    
    sourceIndicator <- match.arg(sourceIndicator)
    
    if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
    # Exclude records with missing source
    data <- data[!is.na(data$aantal_embryos_bron), ]
    
    if (sourceIndicator == "both")
        data$embryos <- data$aantal_embryos else if (sourceIndicator == "inbo") 
        data$embryos <- data$aantal_embryos_labo else
        data$embryos <- data$aantal_embryos_MF
    
    
	# Select data of specified years and type
	plotData <- subset(data, data$afschotjaar %in% jaartallen & data$type_comp %in% type,
            c("afschotjaar", "embryos"))
    nRecords <- nrow(plotData)

    if (nRecords == 0)
        return(NULL)
	
	## For aantal_embryos
	# remove missing
    plotData <- plotData[!is.na(plotData$embryos), ]
    nCollected <- nrow(plotData)
    # remove > 3 embryos
    plotData <- plotData[plotData$embryos <= 3, ]
	
    if (nCollected == 0)
        return(NULL)
    
	# convert to a factor
	newLevels <- rev(c("3", "2", "1", "0"))
	plotData$embryos <- factor(plotData$embryos, levels = newLevels)
	
	plotData$afschotjaar <- as.factor(plotData$afschotjaar)
	
	# use table with factor to have 0 when no counts for certain year/number of embryos
	tmpSummary <- as.data.frame(with(plotData, table(afschotjaar, embryos)))
	
	# Calculate percentages
	tmpPercent <- ddply(tmpSummary, "afschotjaar", transform, 
			percent = Freq / sum(Freq) * 100)
	summaryData <- merge(tmpPercent, tmpSummary, all.y = TRUE)
	
	# Hover text
	summaryData$text <- ifelse(is.na(summaryData$percent), "",
			paste0(round(summaryData$percent), "%"))
	
	
	if (sum(summaryData$Freq) == 0)
		stop("Geen data beschikbaar")
	
	# Summarize data per year
	totalCounts <- table(plotData$afschotjaar)
	
	
	title <- paste0(wildNaam, " ", bioindicatorName, " ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
			if (!all(regio == "")) paste0("\n (", toString(regio), ")"))
	
	
 colorList <- replicateColors(nColors = nlevels(summaryData$embryos))
 colors <- colorList$colors
	names(colors) <- newLevels
	
	
	pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~Freq, color = ~embryos,
					text = ~text, hoverinfo = "x+text+name",
					colors = colors, type = "bar", width = width, height = height) %>%
			
			layout(title = title,
					xaxis = list(title = "afschotjaar"), 
					yaxis = list(title = "Aantal vrouwelijke ree\u00EBn"),
					margin = list(b = 120, t = 100, r = 200),
					legend = list(y = 0.8, yanchor = "top"),
					barmode = if(length(totalCounts) == 1) "group" else "stack",
					annotations = list(x = as.numeric(names(totalCounts)), y = totalCounts, 
							text = paste(if(length(totalCounts) == 1) "totaal:" else "", totalCounts),
							xanchor = 'center', yanchor = 'bottom', showarrow = FALSE)) %>%
			
			add_annotations(text = "Aantal embryo's", 
					xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
					y = 0.8, yanchor = "bottom",    # Same y as legend below
					legendtitle = TRUE, showarrow = FALSE) %>%
            add_annotations(text = paste0(round(nCollected/nRecords, 2)*100, 
                            "% met gekend aantal embryo's van totaal (", nCollected, "/", nRecords, ")"),
                    xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
                    y = -0.2, yanchor = "bottom", showarrow = FALSE)
	
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	summaryData$text <- NULL
	
	
	return(list(plot = pl, data = summaryData, warning = colorList$warning))
	
}
