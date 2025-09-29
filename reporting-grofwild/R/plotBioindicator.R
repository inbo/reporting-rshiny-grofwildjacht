#' Create interactive plot for bioindicator versus year
#' @param bioindicator string with column of \code{data}
#' with bioindicator, either 'onderkaaklengte' or 'ontweid_gewicht'
#' @param sourceIndicator character, defines the data source to be used for
#' bioindicator 'onderkaak_comp_bron'
#' @param type animal type, used to filter \code{data} ('type_comp' column)
#' @inheritParams countYearAge
#' @inheritParams filterGrofwild
#' @import plotly
#' @import mgcv
#' @importFrom INBOtheme inbo_palette
#' @importFrom stats na.omit predict qnorm
#' @return list with:
#' \itemize{
#' \item 'plot':  plotly object, for the specified specie and years 
#' \item 'data':  
#' \itemize{
#' \item for bioindicator set to 'ontweid_gewicht' or 'onderkaaklengte':  
#' raw data used for the plot, as data.frame with:
#' \itemize{
#' \item 'afschotjaar':  year at which the animal was shot 
#' \item 'variable':  value of the bioindicator, a.k.a
#' weight for 'ontweid_gewicht' or length of the lower jaw for
#' 'onderkaaklengte'
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom INBOtheme inbo_lichtblauw
#' @export
plotBioindicator <- function(data, 
		type = NULL, type_leeftijd = NULL,
		jaartallen = NULL, regio = "",
		bioindicator = c("onderkaaklengte", "ontweid_gewicht"),
		sourceIndicator = c("inbo", "meldingsformulier", "both"),
    sourceIndicator_leeftijd = NULL,
    sourceIndicator_geslacht = NULL,
		width = NULL, height = NULL){
	  
	wildNaam <- unique(data$wildsoort)
	
	bioindicator <- match.arg(bioindicator)
	sourceIndicator <- match.arg(sourceIndicator)
	
	bioindicatorName <- switch(bioindicator,
			'onderkaaklengte' = "onderkaaklengte", 
			"ontweid_gewicht" = "ontweid gewicht")
  
  bioindicatorUnit <- switch(bioindicator,
      "onderkaaklengte" = "(mm)",
      "ontweid_gewicht" = "(kg)")
	
 	if (is.null(jaartallen))
		jaartallen <- unique(data$afschotjaar)
		
  # Select data of specified years
	plotData <- data[data$afschotjaar %in% jaartallen,
			c("afschotjaar", grep(bioindicator, colnames(data), value = TRUE), 
					"provincie", "leeftijd_comp_bron", "geslacht_comp_bron", 
          "leeftijd_comp_inbo", "leeftijd_comp", "geslacht_comp",
          "type_comp")]
      
      
  if (bioindicator == "ontweid_gewicht") {
    if (!is.null(type) && !all(type == "all")) {
      plotData <- plotData[plotData$geslacht_comp %in% c(type, "Onbekend"), ]  # to calculate nRecords
    }
    if (!is.null(type_leeftijd) && !all(type_leeftijd == "all")) {
      leeftijdVar <- if (sourceIndicator_leeftijd == "inbo") "leeftijd_comp_inbo" else "leeftijd_comp" 
      plotData <- plotData[plotData[[leeftijdVar]] %in% c(type_leeftijd, "Onbekend"), ]  # to calculate nRecords
    }
  } else {
    if (!is.null(type) && !all(type == "all"))
      plotData <- plotData[plotData$type_comp %in% c(type, "Onbekend"), ]
  }
  nRecords <- nrow(plotData)  #include onbekend for nRecords
      
  # Clean data
  ## Filter on bron
  if (bioindicator == "onderkaaklengte") {
    
    plotData <- filterGrofwild(plotData = plotData, 
      sourceIndicator_leeftijd = sourceIndicator_leeftijd, 
      sourceIndicator_geslacht = sourceIndicator_geslacht,
      sourceIndicator_onderkaak = sourceIndicator)
    
  } else {
    
    plotData <- filterGrofwild(plotData = plotData, 
      sourceIndicator_leeftijd = sourceIndicator_leeftijd, 
      sourceIndicator_geslacht = sourceIndicator_geslacht)
    
  }
  plotData <- plotData[!is.na(plotData[, bioindicator]), ]
  if (bioindicator == "ontweid_gewicht") {
    if (!is.null(type) && !all(type == "all")) {
      plotData <- plotData[plotData$geslacht_comp %in% c(type), ]
    }
    if (!is.null(type_leeftijd) && !all(type_leeftijd == "all")) {
      leeftijdVar <- if (sourceIndicator_leeftijd == "inbo") "leeftijd_comp_inbo" else "leeftijd_comp" 
      plotData <- plotData[plotData[[leeftijdVar]] %in% c(type_leeftijd), ]
    }
  } else {
    if (!is.null(type) && !all(type == "all"))
      plotData <- plotData[plotData$type_comp %in% c(type), ]
  }
  
	colnames(plotData)[colnames(plotData) == bioindicator] <- "variable"
	
	
	if (bioindicator == "ontweid_gewicht") {
		
		accuracy <- NULL
    
	} else {
    
    # Calculate accuracy - github #332
    accuracy <- round(mean(plotData$onderkaaklengte_correct, na.rm = TRUE) * 100, 1)
    
  }
    
	
	if (nrow(plotData) == 0)
		stop("Geen data beschikbaar")
	
	# Summarize data per year
	totalCounts <- as.data.frame(table(plotData$afschotjaar), stringsAsFactors = FALSE)
  names(totalCounts) <- c("afschotjaar", "aantal")
  ## add years with 0 counts
  allYears <- data.frame(afschotjaar = min(plotData$afschotjaar):max(plotData$afschotjaar))
  fullCounts <- merge(totalCounts, allYears, all.y = TRUE)
  fullCounts$aantal[is.na(fullCounts$aantal)] <- 0	
	
	
	
  title <- paste0(wildNaam, " ", bioindicatorName, " ",
    if (bioindicator == "onderkaaklengte") paste0("(", 
        switch(sourceIndicator,
          inbo = "INBO",
          meldingsformulier = "Meldingsformulier",
          both = "INBO en meldingsformulier"),
        ")\n"),
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
    if (!all(regio == "")) paste0(" (", toString(regio), ")"))
  
  colorList <- NULL
  
  
  # create plot
  pl <- plot_ly(data = plotData, x = ~afschotjaar, y = ~variable,
      colors = inbo_lichtblauw, type = "box", width = width, height = height) %>%
    plotly::layout(title = title,
      xaxis = list(title = "afschotjaar", showticklabels = FALSE), 
      yaxis = list(title = paste(bioindicatorName, bioindicatorUnit)),
      margin = list(b = 120, t = 100),
      annotations = list(x = fullCounts$afschotjaar, 
        y = 0, yanchor = "top", text = fullCounts$afschotjaar, 
        textangle = if (nrow(fullCounts) > 10) -45, 
        xref = "x", xanchor = 'center', showarrow = FALSE, 
        hovertext = paste0("(n = ", fullCounts$aantal, ")") 
        )
    ) %>%
    add_annotations(text = percentCollected(nAvailable = nrow(plotData), nTotal = nRecords,
        text = paste("gekend afschotjaar, leeftijd, geslacht en", bioindicatorName)),
      xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
      y = -0.25, yanchor = "bottom", showarrow = FALSE)
  
  # To prevent warnings in UI
  pl$elementId <- NULL
		
	colnames(plotData)[colnames(plotData) == "variable"] <- bioindicatorName
	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	
	return(list(plot = pl, data = plotData, warning = colorList$warning,
      accuracy = list(value = accuracy, total = nrow(plotData))))
	
}



#' Shiny module for creating the plot \code{\link{plotBioindicator}} - UI side
#' @inheritParams countAgeGenderServer 
#' @inheritParams optionsModuleServer 
#' @inheritParams plotBioindicator
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
plotBioindicatorServer <- function(id, data, timeRange = reactive(NULL), types, typesDefault = types,
  bioindicator = c("onderkaaklengte", "ontweid_gewicht"), preSelected = reactive(NULL)) {
  
  bioindicator <- match.arg(bioindicator)
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Bioindicator plot
      callModule(module = optionsModuleServer, id = "plotBioindicator", 
        data = data,
        timeRange = timeRange,
        types = types,
        typesDefault = typesDefault,
        labelTypes = switch(bioindicator,
          'onderkaaklengte' = "Type", 
          "ontweid_gewicht" = "Geslacht"),
        multipleTypes = TRUE)
      
      callModule(module = plotModuleServer, id = "plotBioindicator",
        plotFunction = "plotBioindicator", 
        bioindicator = bioindicator,
        data = data,
        preSelected = preSelected)
      
    })
  
}


#' Shiny module for creating the plot \code{\link{plotBioindicator}} - UI side
#' @inheritParams plotBioindicatorServer
#' @inheritParams optionsModuleUI
#' @inheritParams countAgeCheekUI
#' @inherit welcomeSectionUI
#' 
#' @export
plotBioindicatorUI <- function(id, bioindicator = c("onderkaaklengte", "ontweid_gewicht"), 
  regionLevels = NULL, showType = FALSE, showTime = FALSE,
  showAccuracy = FALSE, uiText, doHide = TRUE, showDataSource = c(),
  context = strsplit(id, split = "_")[[1]][1]) {
  
  # For R CMD check
  variable <- NULL
  
  bioindicator <- match.arg(bioindicator)
  
  ns <- NS(id)
  
  plotFunction <- paste0("plotBioindicatorUI-", bioindicator)
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(
    
    actionLink(inputId = ns("linkPlotBioindicator"), 
      label = h3(HTML(title))
    ),
    conditionalPanel(
      paste("input.linkPlotBioindicator % 2 ==", as.numeric(doHide)),
      ns = ns,
      
      fixedRow(
        
        column(8, 
          plotModuleUI(id = ns("plotBioindicator"))
        ),
        column(4,
          optionsModuleUI(id = ns("plotBioindicator"),
            showTime = showTime, showType = showType,
            regionLevels = regionLevels, exportData = TRUE,
            showDataSource = showDataSource
          ),
          if (showAccuracy)
            accuracyModuleUI(id = ns("plotBioindicator"), 
              title = "Accuraatheid onderkaaklengte"),
        )
      ),
      tags$p(HTML(description)),
      tags$hr()
    )
  )

}