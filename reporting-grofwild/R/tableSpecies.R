# Summary table for species
# 
# Author: mvarewyck
###############################################################################



#' Create summary table for specific species per WBE
#' @param data data.frame with raw data for summary table
#' @inheritParams percentageYearlyShotAnimals 
#' @param minForTrend numeric, the minimum number of records needed before
#' a trend is being reported
#' @return data.frame, number or percentage of observations 
#' per province and per \code{categorie}
#' @author mvarewyck
#' @importFrom reshape2 dcast
#' @importFrom plyr count join
#' @export
tableSpecies <- function(data, jaar = NULL, minForTrend = 10) {
  
  wildNaam <- unique(data$wildsoort)  
  
  if (is.null(jaar))
    stop("Gelieve jaartal te selecteren")
  
  if (wildNaam == "Wild zwijn")
    allData <- data[, c("PartijNummer", "leeftijd_comp", "afschotjaar")] else
    allData <- data[, c("PartijNummer", "type_comp", "afschotjaar")]
  names(allData) <- c("locatie", "categorie", "jaar")
  
  
  # Exclude records with jaar = NA
  allData <- allData[with(allData, !is.na(jaar)), ]
  
  if (!jaar %in% allData$jaar)
      stop("Niet beschikbaar: Geen data voor het gekozen jaar")
  
  
  
  
  # Redefine names and ordering of factor levels
  if (wildNaam == "Wild zwijn") {  # wild zwijn for leeftijd
    
    levelsCategorie <- c("Frisling", "Overloper", "Volwassen", "Onbekend")
    
  } else if (wildNaam == "Ree") {  # ree for leeftijd
    
    levelsCategorie <- c("Geitkits", "Bokkits", "Smalree", "Jaarlingbok", "Reegeit", "Reebok", "Onbekend")
    
  }
  # Rename categorie extra levels/NA to Onbekend
  allData$categorie <- factor(allData$categorie, levels = levelsCategorie)
  allData$categorie[is.na(allData$categorie)] <- tail(levelsCategorie, n = 1)
  
  # A. Current Year
  
  tableData <- allData[allData$jaar == jaar, ]
  
  # Summary of the data
  summaryData <- count(tableData, vars = names(tableData))
  nTotal <- sum(summaryData$freq)
  summaryData$percent <- summaryData$freq / nTotal * 100
  summaryData$current <- summaryData$freq
  summaryData$freq <- NULL
  
  # Add 'totaal'
  levelsCategorie <- c(levelsCategorie, "Totaal")
  
  # Add categorie with 0 observations
  fullData <- expand.grid(
    locatie = unique(allData$locatie),
    categorie = levelsCategorie,
    jaar = jaar)
  summaryTable <- merge(summaryData, fullData, all = TRUE)
  summaryTable$percent <- ifelse(is.na(summaryTable$percent), "", 
    paste0(gsub(pattern = "\\.", "\\,", sprintf("%.1f", summaryTable$percent)), "%"))
  summaryTable[is.na(summaryTable)] <- 0
  
  # Calculate total
  summaryTable$current[summaryTable$categorie == "Totaal"] <- sum(summaryTable$current)
 
  
  
  # B. Calculate differences with 1, 5, 10 years ago
  
  finalTable <- summaryTable
  
  
  # Select data
  for (yearsBack in c(1, 5, 10)) {
    
    freqBack <- count(allData[allData$jaar == (jaar - yearsBack), ], 
      vars = "categorie")
    
    # Only calculate trend if relevant
    if (nrow(freqBack) > 0) {
      
      # Add categorie with 0 observations
      freqBack <- merge(x = fullData[, "categorie", drop = FALSE],
        y = freqBack, all = TRUE)
      freqBack$freq[is.na(freqBack$freq)] <- 0
      
      # Add row for Totaal
      freqBack$freq[freqBack$categorie == "Totaal"] <- sum(freqBack$freq, na.rm = TRUE)
      
      # Calculate trend
      finalTable <- join(x = finalTable, y = freqBack, by = "categorie")
      finalTable[, paste0("Verandering tov ", yearsBack, " jaar (", jaar - yearsBack, ")")] <- 
        ifelse(finalTable$current > minForTrend & 
            finalTable$freq > minForTrend, {
          value <- round((finalTable$current/finalTable$freq - 1)*100, 1)
            charValue <- sprintf("%.1f", value)
            ifelse(value > 0, paste0("+", gsub(pattern = "\\.", "\\,", charValue), "%"), 
              paste0(gsub(pattern = "\\.", "\\,", charValue), "%"))},
          "")
      
      finalTable[is.na(finalTable)] <- ""
      
      finalTable$freq <- NULL
      
    }
    
  }
  
  
  # Order rows and columns
  toReturn <- finalTable[match(levelsCategorie, finalTable$categorie), c("categorie", "current", "percent", 
      names(finalTable)[grep(pattern = "Verandering", x = names(finalTable))])]
  
  
  # Rename columns
  colnames(toReturn)[1:3] <- c("Categorie", "Afschot", "Afschot relatief")
  
  return(toReturn)
  
}




#' Shiny module for creating the plot \code{\link{countAgeGender}} - server side
#' @param id character, unique identifier for the module
#' @param data data.frame for the plot function
#' @param timeRange numeric vector of length 2, min and max year to subset data
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
tableSpeciesServer <- function(id, data, timeRange) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Gerapporteerd afschot per regio en per leeftijdscategorie
      callModule(module = optionsModuleServer, id = "tableSpecies", 
        data = data,
        timeRange = timeRange
      )
      callModule(module = plotModuleServer, id = "tableSpecies",
        plotFunction = "tableSpecies", 
        data = data)
           
    })
  
}


#' Shiny module for creating the plot \code{\link{countAgeGender}} - UI side
#' @template moduleUI
#' 
#' @author mvarewyck
#' @export
tableSpeciesUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    
    h3(HTML(uiText$title)),
    
    fixedRow(
      
      column(4,
        optionsModuleUI(id = ns("tableSpecies"), showYear = TRUE, exportData = TRUE),
        tags$p(HTML(uiText[, id]))
      ),
      column(8, tableModuleUI(id = ns("tableSpecies")))
    
    ),
    tags$hr()
    
  )
    
}

