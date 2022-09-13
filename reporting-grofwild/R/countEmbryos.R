#' Create interactive plot for number of embryos versus year
#' 
#' Adapted version from Figure p. 30 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param type animal type, used to filter \code{data} ('type_comp' column)
#' default is \code{c("Smalree", "Geit")}
#' @param sourceIndicator character, which source to be used; default value is "inbo"
#' @inheritParams countYearAge
#' @inheritParams filterGrofwild
#' @import plotly
#' @importFrom INBOtheme inbo_lichtgrijs
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
  sourceIndicator_leeftijd = NULL,
  sourceIndicator_geslacht = NULL,
  width = NULL, height = NULL) {
    
    
    # to prevent warnings with R CMD check
    embryos <- NULL  
    Freq <- NULL
    type_comp <- NULL
    
    sourceIndicator <- match.arg(sourceIndicator)
    
    wildNaam <- unique(data$wildsoort)
    
    bioindicator <- c("aantal_embryos", "aantal_embryos_labo", "aantal_embryos_MF")
    bioindicatorName <- "aantal embryo's"
    
 
 if (is.null(jaartallen))
   jaartallen <- unique(data$afschotjaar)
 
 
 # Select data of specified years and type
 plotData <- subset(data, data$afschotjaar %in% jaartallen & 
     data$type_comp %in% c(type, "Onbekend"),
   c("afschotjaar", bioindicator, "type_comp", "aantal_embryos_bron",
     "leeftijd_comp_bron", "geslacht_comp_bron"))
 nRecords <- nrow(plotData)
 
 # Filter on source & rename to embryos
 plotData <- filterGrofwild(plotData = plotData, 
   sourceIndicator_embryos = sourceIndicator,
   sourceIndicator_leeftijd = sourceIndicator_leeftijd,
   sourceIndicator_geslacht = sourceIndicator_geslacht)
  plotData <- subset(plotData, type_comp %in% type)
 
      
	# remove > 3 embryos
  if (wildNaam == "Ree") {
    plotData <- plotData[plotData$embryos <= 3 | is.na(plotData$embryos), ]
  }  
  
 
  ## For aantal_embryos
  nCollected <- sum(!is.na(plotData$embryos))
  
  if (nrow(plotData) == 0)
    stop("Geen data beschikbaar")
    
  # convert to a factor
  if (wildNaam == "Ree") {
    newLevels <- c("onbekend", 3:0)
    plotData$embryos[is.na(plotData$embryos)] <- "onbekend"
    plotData$embryos <- factor(plotData$embryos, levels = rev(newLevels))
  } else {
    newLevels <- c("onbekend", ">9", "7-9", "4-6", "1-3", "0")
    plotData$embryos <- as.character(cut(plotData$embryos, breaks = c(0, 1, 4, 7, 9, 20),
      include.lowest = TRUE, right = FALSE, labels = rev(newLevels[-1])))
    plotData$embryos[is.na(plotData$embryos)] <- "onbekend"
    plotData$embryos <- factor(plotData$embryos, levels = rev(newLevels))
  }
  
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
    paste0("(", 
      switch(sourceIndicator,
        inbo = "INBO",
        meldingsformulier = "Meldingsformulier",
        both = "INBO en meldingsformulier"),
      ")\n"),
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
    if (!all(regio == "")) paste0("\n (", toString(regio), ")"))
  
  
  colorList <- replicateColors(nColors = nlevels(summaryData$embryos))$colors
  colors <- c(inbo_lichtgrijs, colorList)[1:nlevels(summaryData$embryos)]
  names(colors) <- newLevels
	
  yTitle <- paste("Aantal vrouwelijke", switch(wildNaam,
    Ree = "ree\u00EBn",
    'Wild zwijn' = "wilde zwijnen"))
	
	pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~Freq, color = ~embryos,
					text = ~text, hoverinfo = "x+text+name",
					colors = colors, type = "bar", width = width, height = height) %>%
			
			layout(title = title,
					xaxis = list(title = "afschotjaar"), 
					yaxis = list(title = yTitle),
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
            add_annotations(text = percentCollected(nAvailable = nCollected,
                nTotal = nRecords, text = "gekend aantal embryo's, leeftijd en geslacht van totaal"),
                    xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
                    y = -0.2, yanchor = "bottom", showarrow = FALSE)
	
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	summaryData$text <- NULL
	
	
	return(list(plot = pl, data = summaryData))
	
}



#' Shiny module for creating the plot \code{\link{countEmbryos}} - UI side
#' @inheritParams countAgeGenderServer 
#' @inheritParams optionsModuleServer
#' @param uiText data.frame, HTML formatted text to be displayed in the UI
#' @param wildsoort character, species to be displayed. 
#' Needed to format title and description in \code{uiText}
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
countEmbryosServer <- function(id, data, timeRange, types, uiText, wildsoort) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      uiText <- uiText[uiText$plotFunction == "countEmbryosUI", ]
      
      output$titleEmbryos <- renderUI({
          
          req(wildsoort())
          
          oldTitle <- uiText$title
          newTitle <- gsub("\\{wildsoort\\}", switch(wildsoort(), 
              "Ree" = "ree\u00EBn",
              "Wild zwijn" = "wilde zwijnen",
              ""),
            oldTitle
          )
          
          h3(HTML(newTitle))
          
        })
      
      output$descriptionEmbryos <- renderUI({
          
          oldText <- uiText[, id]
          if (wildsoort() != "Ree")
            oldText <- strsplit(oldText, split = "Opmerking")[[1]][1]
          
          tags$p(HTML(oldText))
          
        })
      
      # Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar
      callModule(module = optionsModuleServer, id = "countEmbryos", 
        data = data,
        timeRange = timeRange,
        types = types,
        multipleTypes = TRUE)
      callModule(module = plotModuleServer, id = "countEmbryos",
        plotFunction = "countEmbryos",
        data = data)
      
    })
  
}


#' Shiny module for creating the plot \code{\link{countEmbryos}} - UI side
#' @param id character, identifier
#' @param regionLevels character, choices for region
#' 
#' @author mvarewyck
#' @export
countEmbryosUI <- function(id, regionLevels) {
  
  ns <- NS(id)
  
  tagList(
    
    actionLink(inputId = ns("countEmbryos"),
      label = uiOutput(ns("titleEmbryos"))),
    conditionalPanel("input.countEmbryos % 2 == 1", ns = ns,
      
      fixedRow(
        
        column(4,
          optionsModuleUI(id = ns("countEmbryos"), showTime = TRUE, showType = TRUE,
            regionLevels = regionLevels, exportData = TRUE,
            showDataSource = c("embryos", "leeftijd", "geslacht")),
          uiOutput(ns("descriptionEmbryos"))),
        column(8, 
          plotModuleUI(id = ns("countEmbryos"))
        ),
        tags$hr()
      )
    )
  )
  
}

