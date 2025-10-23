
#' Get embryo data
#' @inheritParams countEmbryos
#' @return data.frame with embryo data
#' @export
getEmbryoData <- function(data, jaartallen = NULL, type){
  
  if(!is.null(jaartallen))
    data <- data[which(data$afschotjaar %in% jaartallen), ]
  
  data <- data[which(
    data$type_comp %in% type & 
    # exclude males & totally unknown
    data$geslacht_comp != "Mannelijk" & 
    !(data$geslacht_comp == "Onbekend" & data$type_comp == "Onbekend")
  ), ]

  return(data)
  
}

#' Create interactive plot for number of embryos versus year
#' 
#' Adapted version from Figure p. 30 from https://pureportal.inbo.be/portal/files/11785261/Huysentruyt_etal_2015_GrofwildjachtVlaanderen.pdf
#' @param type animal type, used to filter \code{data} ('type_comp' column)
#' default is \code{c("Smalree", "Geit")}
#' @param sourceIndicator character, which source to be used; default value is "inbo"
#' @inheritParams countYearAge
#' @inheritParams filterGrofwild
#' @return list with:
#' \itemize{
#' \item 'plot':  plotly object, for the specified specie and years 
#' \item 'data':  data displayed in the plot, as data.frame with:
#' \itemize{
#' \item 'afschotjaar':  year at which the animal was shot 
#' \item 'embryos':  aantal embryos 
#' \item 'Freq':  counts of females 
#' \item 'percent':  percentage of females with given number of embryos per year 
#'  }
#' }
#' @author mvarewyck
#' @import plotly
#' @export
countEmbryos <- function(data, type = c("Smalree", "Reegeit"), 
  jaartallen = NULL, regio = "", 
  sourceIndicator = c("inbo", "meldingsformulier", "both"),
  sourceIndicator_leeftijd = NULL,
  summarizeBy = c("count", "percent"),
  sourceIndicator_geslacht = NULL,
  width = NULL, height = NULL) {
    
    
    # to prevent warnings with R CMD check
    embryos <- NULL  
    Freq <- NULL
    type_comp <- NULL
    
    sourceIndicator <- match.arg(sourceIndicator)
    
    wildNaam <- unique(data$wildsoort)
    
    summarizeBy <- match.arg(summarizeBy)
    
    bioindicator <- c("aantal_embryos", "aantal_embryos_labo", "aantal_embryos_MF")
    bioindicatorName <- switch(summarizeBy,
      count = paste("Aantal embryo's van vrouwelijke", switch(wildNaam,
          Ree = "ree\u00EBn",
          'Wild zwijn' = "wilde zwijnen", 
          Edelhert = "edelherten",
          Damhert = "damherten")),
      percent = paste("Percentage van vrouwelijke", switch(wildNaam,
          Ree = "ree\u00EBn",
          'Wild zwijn' = "wilde zwijnen", 
          Edelhert = "edelherten",
          Damhert = "damherten"), "met gedefinieerd aantal embryo's")
    )
 
 if (is.null(jaartallen))
   jaartallen <- unique(data$afschotjaar)
 
 
 # Select data of specified years and type
  plotData <- getEmbryoData(data, 
    jaartallen = jaartallen, type = type)
  plotData <- plotData[,
   c("afschotjaar", bioindicator, "type_comp", "aantal_embryos_bron",
     "leeftijd_comp_bron", "geslacht_comp_bron", "leeftijd_comp_inbo")
  ]
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
    newLevels <- c("Onbekend", 3:0)
    plotData$embryos[is.na(plotData$embryos)] <- "Onbekend"
    plotData$embryos <- factor(plotData$embryos, levels = rev(newLevels))
  } else {
    newLevels <- c("Onbekend", ">9", "7-9", "4-6", "1-3", "0")
    plotData$embryos <- as.character(cut(plotData$embryos, breaks = c(0, 1, 4, 7, 9, 20),
      include.lowest = TRUE, right = FALSE, labels = rev(newLevels[-1])))
    plotData$embryos[is.na(plotData$embryos)] <- "Onbekend"
    plotData$embryos <- factor(plotData$embryos, levels = rev(newLevels))
  }
  
  plotData$afschotjaar <- as.factor(plotData$afschotjaar)
	
	# use table with factor to have 0 when no counts for certain year/number of embryos
	tmpSummary <- as.data.frame(with(plotData, table(afschotjaar, embryos)))
	
	# Calculate percentages
	tmpPercent <- ddply(tmpSummary, "afschotjaar", transform, 
			percent = Freq / sum(Freq) * 100)
	summaryData <- merge(tmpPercent, tmpSummary, all.y = TRUE)
	summaryData$afschotjaar <- as.numeric(as.character(summaryData$afschotjaar))
  
  # Summarize data per year
  totalCounts <- as.data.frame(table(plotData$afschotjaar))
  colnames(totalCounts) <- c("afschotjaar", "value")
  
  # Hover text
  if (summarizeBy == "count") {
    
    summaryData$text <- paste0(
      "n = ", summaryData$Freq,
      ifelse(is.na(summaryData$percent), "", paste0(" (", round(summaryData$percent), "%)")),
      "<br><em>Totaal</em> ", merge(totalCounts, summaryData)$value
    )
    
  } else {
    
    summaryData$text <- paste0(round(summaryData$percent), "%")
    
  }
	
	
	if (sum(summaryData$Freq) == 0)
		stop("Geen data beschikbaar")
	
	
	
  title <- paste0(bioindicatorName, " ",
    paste0("(", 
      switch(sourceIndicator,
        inbo = "INBO",
        meldingsformulier = "Meldingsformulier",
        both = "INBO en meldingsformulier"),
      ")\n"),
    ifelse(length(jaartallen) > 1, paste("van", min(jaartallen), "tot", max(jaartallen)), jaartallen), 
    if (!all(regio == "")) paste0("\n (", toString(regio), ")"))
  
  
  colors <- replicateColors(values = newLevels)$colors
  
  yTitle <- switch(summarizeBy,
    count = paste("Aantal vrouwelijke", switch(wildNaam,
        Ree = "ree\u00EBn",
        'Wild zwijn' = "wilde zwijnen", 
        Edelhert = "edelherten",
        Damhert = "damherten")),
    percent = paste("Percentage vrouwelijke", switch(wildNaam,
        Ree = "ree\u00EBn",
        'Wild zwijn' = "wilde zwijnen", 
        Edelhert = "edelherten",
        Damhert = "damherten"))
  )
  
  

if (summarizeBy == "count") {
  
  
  pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~Freq, color = ~embryos,
      text = ~text, textposition = "none", hoverinfo = "x+text+name",
      colors = colors, type = "bar", width = width, height = height) %>%
    
    plotly::layout(title = title,
      xaxis = list(title = "afschotjaar",
        tickvals = unique(summaryData$afschotjaar),
        ticktext = unique(summaryData$afschotjaar)), 
      yaxis = list(title = yTitle),
      margin = list(b = 120, t = 100, r = 200),
      legend = list(y = 0.8, yanchor = "top"),
      barmode = if(nrow(totalCounts) == 1) "group" else "stack") %>%
    
    add_annotations(text = "Aantal embryo's", 
      xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
      y = 0.8, yanchor = "bottom",    # Same y as legend below
      legendtitle = TRUE, showarrow = FALSE) %>%
    add_annotations(text = percentCollected(nAvailable = nCollected,
        nTotal = nRecords, text = "gekend aantal embryo's, leeftijd en geslacht van totaal"),
      xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
      y = -0.2, yanchor = "bottom", showarrow = FALSE)
  
  
} else {
  pl <- plot_ly(data = summaryData, x = ~afschotjaar, y = ~percent, color = ~embryos,
      text = ~text, textposition = "none", hoverinfo = "x+text+name",
      colors = colors, type = "scatter", mode = "lines+markers", width = width, height = height) %>%
    
    plotly::layout(title = title,
      xaxis = list(title = "afschotjaar",
        tickvals = unique(summaryData$afschotjaar),
        ticktext = unique(summaryData$afschotjaar)), 
      yaxis = list(title = yTitle),
      margin = list(b = 120, t = 100, r = 200),
      legend = list(y = 0.8, yanchor = "top"),
      barmode = if(nrow(totalCounts) == 1) "group" else "stack") %>%
    
    add_annotations(text = "Aantal embryo's", 
      xref = "paper", yref = "paper", x = 1.02, xanchor = "left",
      y = 0.8, yanchor = "bottom",    # Same y as legend below
      legendtitle = TRUE, showarrow = FALSE) %>%
    add_annotations(text = percentCollected(nAvailable = nCollected,
        nTotal = nRecords, text = "gekend aantal embryo's, leeftijd en geslacht van totaal"),
      xref = "paper", yref = "paper", x = 0.5, xanchor = "center",
      y = -0.2, yanchor = "bottom", showarrow = FALSE)
  
}

	
	# To prevent warnings in UI
	pl$elementId <- NULL
	
	summaryData$text <- NULL
	
	
	return(list(plot = pl, data = summaryData))
	
}



#' Shiny module for creating the plot \code{\link{countEmbryos}} - UI side
#' @inheritParams countAgeGenderServer 
#' @inheritParams optionsModuleServer
#' @inheritParams getOutputDescription
#' @param wildsoort character, species to be displayed. 
#' Needed to format title and description in \code{uiText}
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @export
countEmbryosServer <- function(id, data, timeRange, types, 
  uiText, wildsoort = reactive(), context = id) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$titleEmbryos <- renderUI({
          
          title <- getOutputTitle(output = "countEmbryosUI", uiText = uiText)
          h3(HTML(title))
          
        })
      
      output$descriptionEmbryos <- renderUI({
          
          req(wildsoort())   
          
          description <- getOutputDescription(output = "countEmbryosUI", 
            uiText = uiText, context = context)
            
          if (wildsoort() != "Ree")
            description <- strsplit(description, split = "Opmerking")[[1]][1]
          
          tags$p(HTML(description))
          
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
#' @inheritParams optionsModuleUI
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' @author mvarewyck
#' @export
countEmbryosUI <- function(id, regionLevels,
  uiText, context = id, specie = NULL,
  doHide = TRUE) {
  
  ns <- NS(id)
  
  if(!is.null(specie)){
    title <- getOutputTitle(output = "countEmbryosUI", specie = specie, 
      uiText = uiText)
    title <- h3(HTML(title))
    description <- getOutputDescription(output = "countEmbryosUI", 
      specie = specie, uiText = uiText, context = context)
    if (specie != "Ree")
      description <- strsplit(description, split = "Opmerking")[[1]][1]
    description <- tags$p(HTML(description))
  }else{
     title <- uiOutput(ns("titleEmbryos"))
     description <- NULL
  }

  tagList(
    
    actionLink(inputId = ns("countEmbryos"), label = title),
    conditionalPanel(
      condition = paste("input.countEmbryos % 2 ==", as.numeric(doHide)),
      ns = ns,
      
      fixedRow(
        column(8, plotModuleUI(id = ns("countEmbryos"))),
        column(4,
          optionsModuleUI(id = ns("countEmbryos"), 
            showTime = TRUE, showType = TRUE,
            summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
            regionLevels = regionLevels, exportData = TRUE,
            showDataSource = c("embryos", "leeftijd", "geslacht")),
          if(is.null(specie))  uiOutput(ns("descriptionEmbryos"))
        )
      ),
      description
    )
  )
  
}

#' Get types for females
#' @param specie character with specie
#' @inheritParams reportingGrofwild-common-args
#' @return character vector with female types 
#' (from the \code{type_comp} column) for the specific specie
#' @author lcougnaud
#' @export
getFemaleTypes <- function(ecoData, specie){
  
  types <- levels(droplevels(ecoData$type_comp))
  
  types <- if (specie == "Ree") {
    types[types %in% c("Reegeit", "Smalree")] 
  } else if (specie == "Wild zwijn"){
    types[types %in% c("Zeug", "Overloper (v)", "Frisling (v)")]      
  } else {
    types[types %in% c("Kalf (v)", "Smaldier", "Hinde")]        
  }
  
  types <- c(types, "Onbekend")
  
  return(types)
  
}