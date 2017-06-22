#' Create interactive plot for bioindicator versus year
#' @param bioindicator string with column of \code{data}
#' with bioindicator, either 'onderkaaklengte',
#' 'ontweid_gewicht', or 'aantal_embryos'
#' @inheritParams countYearAge
#' @import plotly
#' @importFrom INBOtheme inbo.2015.colours
#' @importFrom stats na.omit loess predict qt
#' @return plotly object, for the specified specie and years
#' @author Laure Cougnaud
#' @export
plotBioindicator <- function(data, wildNaam = "", 
		jaartallen = NULL, regio = "",
		bioindicator = c("onderkaaklengte", "ontweid_gewicht", "aantal_embryos"),
		width = NULL, height = NULL){
	
	bioindicator <- match.arg(bioindicator)
	
	bioindicatorName <- switch(bioindicator,
			'onderkaaklengte' = "onderkaak lengte", 
			"ontweid_gewicht" = "ontweid gewicht", 
			"aantal_embryos" = "aantal embryos")
	
	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
	
	# Select data of specified years
	plotData <- data[
		data$afschotjaar %in% jaartallen,
		c("afschotjaar", "afschot_datum", bioindicator)]

	if(bioindicator != "aantal_embryos")
		plotData <- data[!is.na(data[, bioindicator]), ]

	colnames(plotData)[colnames(plotData) == bioindicator] <- "variable"
	
	if(bioindicator == "aantal_embryos"){
		
		# replace NA by 'Niet ingevuld', convert to a factor
		variable <- ifelse(is.na(plotData$variable), 
			"Niet ingevuld", 
			as.character(plotData$variable))
		uniqueNEmbryos <- as.character(sort(unique(na.omit(plotData$variable))))
		levelsVariable <- c("Niet ingevuld", 
			paste0(uniqueNEmbryos, " embryo", ifelse(uniqueNEmbryos > 1,	"'s", "")))
		
		plotData$variable <- factor(variable, levels = levelsVariable)
		plotData$afschotjaar <- as.factor(plotData$afschotjaar)
		# use table with factor to have 0 when no counts for certain year/number of embryos
		inputPlot <- as.data.frame(with(plotData, table(afschotjaar, variable)))
	}
	
#	plotData$afschot_datum <- as.Date(plotData$afschot_datum, format = "%Y-%m-%d")
#	plotData$afschotjaar <- as.Date(as.numeric(plotData$afschotjaar), format = "%Y")

	title <- paste0(wildNaam, " ", bioindicatorName, " ",
			ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
			if (!all(regio == "")) paste0(" (", toString(regio), ")"))
	
	if(bioindicator == "aantal embryos"){
		
		palette <- inbo.2015.colours(n = nlevels(inputPlot$variable))
		
		# base plot
#		pl <- plot_ly(inputPlot, 
#				x = ~afschotjaar, y = ~freq, color = ~variable,
#				width = width, height = height)  %>%
#				
#				# line
#				add_lines(line = list(color = palette),
#						name = "Aantal vrouwelijke reeen"
#						) %>%
		
			pl <- plot_ly()
			
			# to have INBO palette for each line:
			for(i in 1:nlevels(inputPlot$variable)){
					varI <- levels(inputPlot$variable)[i]
					pl <- pl %>% add_trace(
							data = inputPlot[which(inputPlot$variable == varI), , drop = FALSE],
							x = ~afschotjaar, y = ~Freq, 
							type = 'scatter', mode = 'lines',
							marker = list(color = col),
							line = list(color = palette[i]), 
							name = varI,
							showlegend = TRUE)
			}
		
			# title axes and margin bottom
			pl <- pl %>% layout(title = title,
				xaxis = list(title = "afchotjaar"), 
				yaxis = list(title = "Aantal vrouwelijke reeen"),
				margin = list(b = 40, t = 100)
			)
		
	}else{
	
		# in ggplot:
		#	ggplot(mapping = aes(y = variable, x = as.integer(afschotjaar)), data = plotData) +
		#		geom_smooth(method = "loess")
	
		# directly in plotly:
		#	plot_ly(plotData, x = ~afschotjaar, y = ~variable, type = "scatter") %>%
		#		add_lines(y = ~fitted(loess(variable ~ afschotjaar)))
		
		# compute lowess manually
		model <- loess(variable ~ afschotjaar, data = plotData)
		pred <- predict(object = model, newdata = NULL, se = TRUE)
	
		getCiLoess <- function(type)
			pred$fit + switch(type, 'lower' = -1, 'upper' = 1) * 
					qt(0.975, pred$df) * pred$se
		
		inputPlot <- data.frame(
			afschotjaar = as.factor(plotData$afschotjaar),
			fit = pred$fit, 
			ciLower = getCiLoess("lower"),
			ciUpper = getCiLoess("upper")
			)
			
		# ribbon color with transparency	
		colorRibbon <- paste0("rgba(", paste(c(col2rgb(inbo.lichtblauw), "0.5"), collapse = ","), ")")
			
		# base plot
		pl <- plot_ly(inputPlot, x = ~afschotjaar, width = width, height = height)  %>%
				
				# loess fit
				add_lines(y = ~fit,
					line = list(color = inbo.lichtblauw),
					name = "Loess Smoother") %>%
			
				# confidence interval
				add_ribbons(ymin = ~ciLower, ymax = ~ciUpper,
						fill = 'tonexty', fillcolor = colorRibbon,
						list(color = inbo.lichtblauw),
						name = "Betrouwbaarheidsinterval")  %>%
				
				# title axes and margin bottom
				layout(title = title,
					xaxis = list(title = "afchotjaar"), 
					yaxis = list(title = bioindicatorName),
					margin = list(b = 40, t = 100)
			)
	
		}

	return(pl)

}