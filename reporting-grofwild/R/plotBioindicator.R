#' Create interactive plot for bioindicator versus year
#' @param bioindicator string with column of \code{data}
#' with bioindicator, either 'onderkaaklengte',
#' 'ontweid_gewicht', or 'aantal_embryos'
#' @param type animal type, used to filter \code{data} ('ageGender' column)
#' If NULL (by default), for bioindicator set to:
#' \itemize{
#' \item{'aantal_embryos': }{only animals with age/gender type: "Smalree" "Geit" are considered}
#' \item{'ontweid_gewicht' or 'onderkaaklengte'}{only animals without age/gender type are filtered (\code{ageGender} set to '')}
#' }
#' @inheritParams countYearAge
#' @import plotly
#' @import mgcv
#' @importFrom INBOtheme inbo.2015.colours
#' @importFrom stats na.omit predict qnorm
#' @return list with:
#' \itemize{
#' \item{'plot': }{plotly object, for the specified specie and years}
#' \item{'data': }{
#' \itemize{
#' \item{for bioindicator set to 'aantal_embryos': }{
#' data displayed in the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animal was shot}
#' \item{'variable': }{age/gender category}
#' \item{'Freq': }{counts of females}
#' }}
#' \item{for bioindicator set to 'ontweid_gewicht' or 'onderkaaklengte': }{
#' raw data used for the plot, as data.frame with:
#' \itemize{
#' \item{'afschotjaar': }{year at which the animal was shot}
#' \item{'variable': }{value of the bioindicator, a.k.a
#' weight for 'ontweid_gewicht' or length of the lower jaw for
#' 'onderkaaklengte'}}
#' }}
#' }
#' }
#' @author Laure Cougnaud
#' @export
plotBioindicator <- function(data, wildNaam = "", 
    type = NULL,
    jaartallen = NULL, regio = "",
    bioindicator = c("onderkaaklengte", "ontweid_gewicht", "aantal_embryos"),
    sourceIndicator = c("inbo", "meldingsformulier", "both"),
    width = NULL, height = NULL){
  
#	message("type is:", type)
  
  bioindicator <- match.arg(bioindicator)
  sourceIndicator <- match.arg(sourceIndicator)
  
  bioindicatorName <- switch(bioindicator,
      'onderkaaklengte' = "lengte onderkaak", 
      "ontweid_gewicht" = "ontweid gewicht", 
      "aantal_embryos" = "aantal embryo's")
  
  if (is.null(jaartallen))
    jaartallen <- unique(data$afschotjaar)
  
  if(is.null(type))
    type <- if (bioindicator == "aantal_embryos")
          c("Smalree", "Geit") else
          levels(data$ageGender)[levels(data$ageGender) != ""]
  
  if(!is.null(type)){
    data <- data[data$ageGender %in% type, ]
  }
  
  # Bioindicator 'onderkaaklengte' depends on data source
  if (bioindicator == "onderkaaklengte")
    data$onderkaaklengte <- with(data, switch(sourceIndicator,
        inbo = onderkaaklengte,
        meldingsformulier = lengte_mm,
        both = ifelse(!is.na(onderkaaklengte), onderkaaklengte, lengte_mm)))
  
  # Select data of specified years
  plotData <- data[data$afschotjaar %in% jaartallen,
      c("afschotjaar", bioindicator)]
  
  if(bioindicator != "aantal_embryos" && length(unique(plotData$afschotjaar)) <= 2)
    stop("Niet beschikbaar: Gelieve periode met minstens 3 jaren te selecteren")
  
  if(bioindicator != "aantal_embryos")
    plotData <- plotData[!is.na(plotData[, bioindicator]), ]
  
  colnames(plotData)[colnames(plotData) == bioindicator] <- "variable"
  
  
  if(bioindicator == "aantal_embryos"){
    
    # remove > 3 embryos
    plotData <- plotData[is.na(plotData$variable) | plotData$variable <= 3, ]
    
    # replace NA by 'Niet ingevuld', convert to a factor
    formatVariable <- function(x)
      paste0(x, " embryo", ifelse(x > 1,	"'s", ""))
    variable <- ifelse(
        is.na(plotData$variable),  "Niet ingevuld", 
        formatVariable(plotData$variable))
    levelsVariable <- c("Niet ingevuld", 
        formatVariable(sort(unique(na.omit(plotData$variable)))))
    plotData$variable <- factor(variable, levels = levelsVariable)
    
    plotData$afschotjaar <- as.factor(plotData$afschotjaar)
    
    # use table with factor to have 0 when no counts for certain year/number of embryos
    inputPlot <- as.data.frame(with(plotData, table(afschotjaar, variable)))
    
  } else if (bioindicator == "ontweid_gewicht") {
    
    # remove weights < 5kg or > 25kg
    plotData <- subset(plotData, variable >= 5 & variable <= 25)    
    
  }
  
  if (nrow(plotData) == 0)
    stop("Geen data beschikbaar")
  
  # Summarize data per year
  totalCounts <- table(plotData$afschotjaar)
  
  
#	plotData$afschot_datum <- as.Date(plotData$afschot_datum, format = "%Y-%m-%d")
#	plotData$afschotjaar <- as.Date(as.numeric(plotData$afschotjaar), format = "%Y")
  
  title <- paste0(wildNaam, " ", bioindicatorName, " ",
      if (bioindicator == "onderkaaklengte") paste0("(", 
            switch(sourceIndicator,
                inbo = "INBO",
                meldingsformulier = "Meldingsformulier",
                both = "INBO en meldingsformulier"),
            ")\n"),
      ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
      if (!all(regio == "")) paste0(" (", toString(regio), ")"))
  
  if(bioindicator == "aantal_embryos"){
    
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
    
    # custom wrapper for trace
    traceWrapper <- function(...){
      argsTrace <- c(
          list(...,
              x = ~afschotjaar, y = ~Freq, 
              type = 'scatter',
              showlegend = TRUE
          ),
          # if only one year, nothing is plotted with 'lines'
          if(length(unique(inputPlot$afschotjaar)) == 1)
                list(mode = "markers", marker = list(color = palette[i]))else
                list(mode = "lines", line = list(color = palette[i]))
      )
      do.call(add_trace, argsTrace)
    }
    
    # base plot
    pl <- plot_ly()
    
    # to have INBO palette for each line:
    for(i in 1:nlevels(inputPlot$variable)){
      varI <- levels(inputPlot$variable)[i]
      pl <- traceWrapper(
          p = pl, 
          data = inputPlot[which(inputPlot$variable == varI), , drop = FALSE],
          name = varI
      )
    }
    
    # title axes and margin bottom
    pl <- pl %>% layout(title = title,
        xaxis = list(title = "afschotjaar"), 
        yaxis = list(title = "Aantal vrouwelijke reeen"),
        margin = list(b = 40, t = 100)
    )
    
  }else{
    
    # equivalent at in ggplot:
    #	ggplot(mapping = aes(y = variable, x = as.integer(afschotjaar)), data = plotData) +
    #		geom_smooth(method = "loess")
    
    # directly in plotly:
    #	plot_ly(plotData, x = ~afschotjaar, y = ~variable, type = "scatter") %>%
    #		add_lines(y = ~fitted(loess(variable ~ afschotjaar)))
    
    # compute lowess manually
#		system.time(model <- loess(variable ~ afschotjaar, data = plotData))
#		system.time(pred <- predict(object = model, se = TRUE))
#		getCiLoess <- function(type)
#			pred$fit + switch(type, 'lower' = -1, 'upper' = 1) * 
#					qt(0.975, pred$df) * pred$se
    
    returnedData <- plotData
    
    # Note: default used by ggplot for high number of points
    # but doesn't support use of <= 2 years
#		cs = Cubic regression splines
    model <- gam(
        formula = variable ~ s(afschotjaar, bs = "cs", k = length(unique(plotData$afschotjaar))), 
        data = plotData)
    pred <- predict(object = model, se.fit = TRUE)
    getCiLoess <- function(type)
      pred$fit + switch(type, 'lower' = -1, 'upper' = 1) * 
          qnorm(p = 0.975) * pred$se.fit
    inputPlot <- data.frame(
        afschotjaar = as.factor(plotData$afschotjaar),
        fit = pred$fit, 
        ciLower = getCiLoess("lower"),
        ciUpper = getCiLoess("upper")
    )
    
#		represent median and quantile		
#		inputPlot <- ddply(plotData, "afschotjaar", function(x){
#				quantiles <- quantile(x$variable, probs = c(0.025, 0.975))
#				data.frame(median = median(x$variable), 
#					quant1 = quantiles[1], quant2 = quantiles[2], 
#					stringsAsFactors = FALSE)
#		})
#		inputPlot$afschotjaar <- as.factor(inputPlot$afschotjaar)
    
    # ribbon color with transparency	
    colorRibbon <- paste0("rgba(", paste(c(col2rgb(inbo.lichtblauw), "0.5"), collapse = ","), ")")
    
    # base plot
    pl <- plot_ly(inputPlot, 
            x = ~afschotjaar, y = ~fit, 
            text = paste("n =", sapply(inputPlot$afschotjaar, function(x)
                      totalCounts[names(totalCounts) == as.character(x)])),
            width = width, height = height) %>%
        
        # loess fit
        add_lines(line = list(color = inbo.lichtblauw),
            name = "Gemiddelde") %>%
        
        # confidence interval
        add_ribbons(ymin = ~ciLower, ymax = ~ciUpper,
            fill = 'tonexty', fillcolor = colorRibbon,
            line = list(color = inbo.lichtblauw),
            name = "95% betrouwbaarheidsinterval")  %>%
        
        # title axes and margin bottom
        layout(title = title,
            xaxis = list(title = "afschotjaar"), 
            yaxis = list(title = bioindicatorName),
            margin = list(b = 40, t = 100),
            # Let y-axis start at 0 
            annotations = list(x = seq(0, 1, length.out = length(totalCounts)), 
                y = 0, 
                xref = "paper", text = "", xanchor = 'center', 
                yanchor = 'bottom', showarrow = FALSE)
        )
    
  }
  
  returnedData <- if(bioindicator == "aantal_embryos")	inputPlot	else	plotData
  
  # To prevent warnings in UI
  pl$elementId <- NULL
  
  
  return(list(plot = pl, data = returnedData))
  
}