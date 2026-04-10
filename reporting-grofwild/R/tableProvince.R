# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################


#' Create summary table for specific species
#' @param data data.frame with raw data for summary table
#' @param assignedData data.frame with summary data on the number of assigned
#' animals that can be shot
#' @inheritParams percentageYearlyShotAnimals 
#' @param categorie character, defines the dependent variable in the table
#' besides "provinces" 
#' @param minForTrend numeric, the minimum number of records needed before
#' a trend is being reported
#' @param sourceIndicator_leeftijd character, source used to filter \code{data} ('leeftijd_comp_bron' column)
#' should be one of \code{c("inbo", "both")}, where \code{"both"} refers to both inbo and meldingsformulier, 
#' i.e. no filtering. Defaults to \code{"both"}
#' @return data.frame, number or percentage of observations 
#' per province and per \code{categorie}
#' @author mvarewyck
#' @importFrom reshape2 dcast
#' @importFrom plyr count join
#' @export
tableProvince <- function(data, assignedData, jaar = NULL, type,
		categorie = c("leeftijd", "typeAantal", "typePercent"), minForTrend = 50,
    sourceIndicator_leeftijd = NULL, regio = "") {
	
  # For R CMD check
  leeftijd_comp_inbo <- NULL
  
	wildNaam <- unique(data$wildsoort)  
	categorie <- match.arg(categorie)
	
	if (is.null(jaar))
		stop("Gelieve jaartal te selecteren")
	
  if (categorie == "leeftijd") {
    data <- filterGrofwild(plotData = data, 
      sourceIndicator_leeftijd = sourceIndicator_leeftijd)
    
    # Special case: inbo leeftijd_comp distinguishes frisling <6m and >6m
    if (!is.null(sourceIndicator_leeftijd) && sourceIndicator_leeftijd == "inbo")
      data <- subset(data, leeftijd_comp_inbo %in% type)
    else data <- subset(data, leeftijd_comp %in% type)
  }
  
  if (all(regio == "Vlaams Gewest")) {
    data$locatie <- factor(data$provincie)
    locatieName <- "Provincie"
  } else if (all(regio %in% c("West-Vlaanderen", "Oost-Vlaanderen", "Vlaams Brabant", "Antwerpen", "Limburg", "Voeren", "Onbekend"))) {
    data$locatie <- factor(data$provincie)
    locatieName <- "Provincie"
  } else if (all(regio %in% c(as.character(1:10), "Onbekend"))) {
    data$locatie <- data$FaunabeheerZone
    data$locatie <- factor(data$locatie, levels = levels(droplevels(factor(unique(data$locatie), 
            levels = c(1:10)))))
    locatieName <- "Faunabeheerzone"
  } else {
    data$locatie <- factor(data$gemeente_afschot_locatie)
    locatieName <- "Gemeente"
  }
  
	## General Modification of Data
	if (categorie == "leeftijd") {
		
		allData <- data[, c("locatie", "leeftijd_comp", "afschotjaar")]
		names(allData) <- c("locatie", "categorie", "jaar")
		
	} else if (categorie == "typeAantal") {
		
		allData <- data[, c("locatie", "labeltype", "afschotjaar")]
		names(allData) <- c("locatie", "categorie", "jaar")
		
	} else {
		
		allData <- data[, c("locatie", "labeltype", "afschotjaar")]
		names(allData) <- c("locatie", "categorie", "jaar")
    
		assignedData <- assignedData[, c("provincie_toek", "labeltype", "labeljaar", "toegekend")]
		names(assignedData) <- c("locatie", "categorie", "jaar", "totaal")
    # summarize per province
    assignedData <- aggregate(totaal ~ jaar + locatie + categorie, data = assignedData, sum)
		
	}
	  
	
	# Exclude records with provincie = NA, jaar = NA
#	allData <- allData[with(allData, !is.na(provincie) & !is.na(jaar)), ]
  allData <- allData[with(allData, !is.na(jaar)), ]
  
	
	# Rename categorie NA to "Onbekend"
	allData$categorie[is.na(allData$categorie) | allData$categorie == ""] <- "Onbekend"
	
  # Rename provincie NA to "Onbekend"
  allData$locatie <- factor(allData$locatie, levels = levels(addNA(allData$locatie)), 
      labels = c(levels(allData$locatie), "Onbekend"), exclude = NULL)
  
  levelsLocatie <- levels(allData$locatie)
     
  
	
	if (categorie == "typePercent") {
		
		if (!jaar %in% allData$jaar | !jaar %in% assignedData$jaar)
			stop("Niet beschikbaar: Geen data voor het gekozen jaar")
		
	} else {
		
		if (!jaar %in% allData$jaar)
			stop("Niet beschikbaar: Geen data voor het gekozen jaar")
		
	}
	
	
	
	
	
	# A. Current Year
	
	# Select data
	tableData <- allData[allData$jaar == jaar, ]
	if (categorie == "typePercent")
		tableAssignedData <- assignedData[assignedData$jaar == jaar, ]
	
	# Define names and ordering of factor levels
	if (categorie == "leeftijd") {  # wild zwijn for leeftijd
		
		levelsCategorie <- type
		
	} else if (grepl("type", categorie) & wildNaam == "Ree") {  # ree for type
		
		levelsCategorie <- c("REEGEIT", "REEBOK", "REEKITS")
		
	}
	
	
	# Summary of the data
	summaryData <- count(tableData, vars = names(tableData))
	
	if (categorie == "typePercent") {
		
		summaryData <- merge(x = summaryData, y = tableAssignedData, all = TRUE)
    summaryData$totaal[is.na(summaryData$totaal)] <- 0
		summaryData$percent <- summaryData$freq/summaryData$totaal
		
	} 
	
	
	# Add province/categorie with 0 observations
	fullData <- expand.grid(
    locatie = unique(allData$locatie),
			categorie = levelsCategorie,
			jaar = jaar)
	summaryData <- merge(summaryData, fullData, all = TRUE)
	
	if (categorie == "typePercent")
		summaryVariables <- c("freq", "totaal") else
		summaryVariables <- "freq"
	
	
	summaryTables <- lapply(summaryVariables, function(iVariable) {
				
				# Long to wide table
				summaryTable <- dcast(summaryData, locatie ~ categorie, value.var = iVariable)
				
				# Optimal displaying of the table
				summaryTable[is.na(summaryTable)] <- 0
				summaryTable <- summaryTable[, c("locatie", levelsCategorie)]
				
				# Add row and column sum
				levels(summaryTable$locatie) <- c(levels(summaryTable$locatie), "Vlaanderen")
				summaryTable <- rbind(summaryTable, 
						c(locatie = "Vlaanderen", as.list(apply(summaryTable[, levelsCategorie], 2, sum))))
				summaryTable <- cbind(summaryTable, 
						Totaal = apply(summaryTable[, levelsCategorie], 1, sum))
				
				
				return(summaryTable)
				
			})
	
	
	if (categorie == "typePercent") {
		
		percentages <- summaryTables[[1]][, -1]/summaryTables[[2]][, -1]
		summaryTable <- cbind(locatie = summaryTables[[1]][, 1], percentages)
		
	} else {
		
		summaryTable <- summaryTables[[1]]
		
	}
	
	
	# B. Calculate differences with 1, 5, 10 years ago
	
	finalTable <- summaryTable
	
	
	# Select data
	for (yearsBack in c(1, 5, 10)) {
		
		freqBack <- count(allData[allData$jaar == (jaar - yearsBack), ], 
				vars = "locatie")
		
		# Only calculate trend if relevant
		if (nrow(freqBack) > 0) {
			
			# Add provinces with 0 observations
			freqBack <- merge(x = data.frame(locatie = unique(allData$locatie)),
					y = freqBack, all = TRUE)
			freqBack$freq[is.na(freqBack$freq)] <- 0
			
			# Calculate percentages
			if (categorie == "typePercent") {
				
				freqBackAssigned <- count(assignedData[assignedData$jaar == (jaar - yearsBack), ],
						vars = "locatie", wt_var = "totaal")
				freqBackAssigned$totaal <- freqBackAssigned$freq
				freqBackAssigned$freq <- NULL
				
				freqBack <- merge(x = freqBack, y = freqBackAssigned, all = TRUE)
				freqBack$percent <- freqBack$freq/freqBack$totaal
				
			}
			
			# Add row for Vlaanderen
			levels(freqBack$locatie) <- c(levels(freqBack$locatie), "Vlaanderen")
			if (categorie == "typePercent")
				freqBack <- rbind(freqBack, list(locatie = "Vlaanderen", 
								freq = sum(freqBack$freq, na.rm = TRUE), 
								totaal = sum(freqBack$totaal, na.rm = TRUE), 
								percent = sum(freqBack$freq, na.rm = TRUE)/sum(freqBack$totaal, na.rm = TRUE))) else 
				freqBack <- rbind(freqBack, list(locatie = "Vlaanderen", 
								freq = sum(freqBack$freq, na.rm = TRUE)))
			
			freqBack$totaal <- NULL
			
			
			# Calculate trend
			finalTable <- join(x = finalTable, y = freqBack, by = "locatie")
      finalTable[, paste("Warning", yearsBack, "jaar")] <- c("zwart", "oranje", "rood")[
        ifelse(
        summaryTables[[1]]$Totaal != 0 & finalTable$freq != 0,
        (summaryTables[[1]]$Totaal < minForTrend) + (finalTable$freq < minForTrend),
        0) + 1]
			finalTable[, paste0("Verandering tov ", yearsBack, " jaar (", jaar - yearsBack, ")")] <- 
					ifelse(summaryTables[[1]]$Totaal != 0 & finalTable$freq != 0,
							{ if (categorie == "typePercent") 
									value <- round((finalTable$Totaal - finalTable$percent)*100, 1) else
									value <- round((finalTable$Totaal/finalTable$freq - 1)*100, 1)
								charValue <- sprintf("%.1f", value)
								ifelse(value > 0, paste0("+", gsub(pattern = "\\.", "\\,", charValue), "%"), 
										paste0(gsub(pattern = "\\.", "\\,", charValue), "%"))},
							"")
			
			finalTable$freq <- NULL
			finalTable$percent <- NULL
			finalTable[is.na(finalTable)] <- ""
			
		}
		
	}
	
	
	rowOrder <- match(c(levelsLocatie, "Vlaanderen"), finalTable$locatie)
	toReturn <- finalTable[rowOrder[!is.na(rowOrder)], c("locatie", levelsCategorie, "Totaal",
					names(finalTable)[grep(pattern = "Verandering", x = names(finalTable))],
          names(finalTable)[grep(pattern = "Warning", x = names(finalTable))])]
	
	# If the selected year is not relevant, return NULL
	if (all(toReturn$Totaal == "Inf"))
		return(NULL)
	
	
	if (categorie == "typePercent") {
    toReturn[, c(levelsCategorie, "Totaal")] <- 
      sapply(toReturn[, c(levelsCategorie, "Totaal")], function(x)
          paste0(round(as.numeric(x)*100), "%"))
    
    
    toReturn <- toReturn[toReturn$locatie != "Onbekend", ]
      }
	
	# Rename provincie
	names(toReturn)[names(toReturn) == "locatie"] <- locatieName
	
	
	return(list(data = toReturn))
	
}



#' Shiny module for creating the plot \code{\link{tableProvince}} - server side
#' @inheritParams countAgeGenderServer 
#' @inheritParams tableProvince
#' @inheritParams optionsModuleServer
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
tableProvinceServer <- function(id, data, categorie, timeRange, types = NULL, labelTypes = "Type", typesDefault = types,
  preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Table 1: Gerapporteerd afschot per regio en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "tableProvince", 
        data = data,
        types = types,
        labelTypes = labelTypes,
        typesDefault = typesDefault,
        multipleTypes = TRUE, 
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "tableProvince",
        plotFunction = "tableProvince", 
        data = data, 
        categorie = categorie,
        preSelected = preSelected)
      
    })
  
} 



#' Shiny module for creating the plot \code{\link{tableProvince}} - UI side
#' @inherit welcomeSectionUI
#' @inheritParams getOutputDescription
#' @inheritParams reportingGrofwild-common-args
#' 
#' @export
tableProvinceUI <- function(id, doHide = TRUE,
  uiText, context = id, specie = NULL, showDataSource = NULL, regionLevels = NULL,
  showType = FALSE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = "tableProvinceUI", 
    specie = specie, uiText = uiText)
  description <- getOutputDescription(output = "tableProvinceUI", 
    specie = specie, uiText = uiText, context = context)
  
  tagList(
    
    actionLink(inputId = ns("linkTableProvince"), 
      label = h3(HTML(title))),
    conditionalPanel(paste("input.linkTableProvince % 2 ==", as.numeric(doHide)), ns = ns,
    
      optionsModuleUI(id = ns("tableProvince"), 
        showYear = TRUE, exportData = TRUE, regionLevels = regionLevels,
        showType = showType, showDataSource = showDataSource),
      tableModuleUI(id = ns("tableProvince")),
      tags$div(class = "larger-description", HTML(description))
    )
  )
  
  
}