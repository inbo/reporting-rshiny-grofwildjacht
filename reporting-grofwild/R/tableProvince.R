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
#' @return data.frame, number or percentage of observations 
#' per province and per \code{categorie}
#' @author mvarewyck
#' @importFrom reshape2 dcast
#' @importFrom plyr count join
#' @export
tableProvince <- function(data, assignedData, wildNaam = NULL, jaar = NULL, 
    categorie = c("leeftijd", "typeAantal", "typePercent"), minForTrend = 50) {
  
  
  categorie <- match.arg(categorie)
  
  
  ## General Modification of Data
  
  if (categorie == "leeftijd") {
    
    allData <- data[, c("provincie", "leeftijdscategorie_MF", "afschotjaar")]
    names(allData) <- c("provincie", "categorie", "jaar")
    
  } else if (categorie == "typeAantal") {
    
    allData <- data[, c("provincie", "type", "afschotjaar")]
    names(allData) <- c("provincie", "categorie", "jaar")
    
  } else {
    
    allData <- data[, c("provincie", "type", "afschotjaar")]
    names(allData) <- c("provincie", "categorie", "jaar")
    
    assignedData <- assignedData[, c("Provincie", "Labeltype", "Jaar", "Aantal")]
    names(assignedData) <- c("provincie", "categorie", "jaar", "totaal")
    
  }
  
  levelsProvincie <- levels(allData$provincie)
  
  
  # Exclude records with provincie = NA, jaar = NA
  allData <- allData[with(allData, !is.na(provincie) & !is.na(jaar)), ]
  
  # Rename categorie NA to "Onbekend"
  allData$categorie[is.na(allData$categorie) | allData$categorie == ""] <- "Onbekend"
  
  
  
  if (categorie == "typePercent") {
    
    if (is.null(jaar))
      jaar <- min(max(allData$jaar), max(assignedData$jaar)) 
    
    if (!jaar %in% allData$jaar | !jaar %in% assignedData$jaar)
      stop("Niet beschikbaar: Geen data voor het gekozen jaar")
    
  } else {
    
    if (is.null(jaar)) 
      jaar <- max(allData$jaar)
    
    if (!jaar %in% allData$jaar)
      stop("Niet beschikbaar: Geen data voor het gekozen jaar")
    
  }
  
  
  
  
  
  # A. Current Year
  
  # Select data
  tableData <- allData[allData$jaar == jaar, ]
  if (categorie == "typePercent")
    tableAssignedData <- assignedData[assignedData$jaar == jaar, ]
  
  # Define names and ordering of factor levels
  if ("Frisling" %in% tableData$categorie) {  # wild zwijn for leeftijd
    
    levelsCategorie <- c("Frisling", "Overloper", "Volwassen", "Onbekend")
    
  } else if ("Jongvolwassen" %in% tableData$categorie){  # ree for leeftijd
    
    levelsCategorie <- c("Kits", "Jongvolwassen", "Volwassen", "Onbekend")
    
  } else {  # ree for type
    
    levelsCategorie <- c("geit", "bok", "kits")
    
  }
  
  
  # Summary of the data
  summaryData <- count(tableData, vars = names(tableData))
  
  if (categorie == "typePercent") {
    
    summaryData <- merge(x = summaryData, y = tableAssignedData, all = TRUE)
    summaryData$percent <- summaryData$freq/summaryData$totaal
    
  } 
  
  
  # Add province/categorie with 0 observations
  fullData <- expand.grid(
      provincie = unique(allData$provincie),
      categorie = levelsCategorie,
      jaar = jaar)
  summaryData <- merge(summaryData, fullData, all = TRUE)
  
  if (categorie == "typePercent")
    summaryVariables <- c("freq", "totaal") else
    summaryVariables <- "freq"
  
  
  summaryTables <- lapply(summaryVariables, function(iVariable) {
        
        # Long to wide table
        summaryTable <- dcast(summaryData, provincie ~ categorie, value.var = iVariable)
        
        # Optimal displaying of the table
        summaryTable[is.na(summaryTable)] <- 0
        summaryTable <- summaryTable[, c("provincie", levelsCategorie)]
        
        # Add row and column sum
        levels(summaryTable$provincie) <- c(levels(summaryTable$provincie), "Vlaanderen")
        summaryTable <- rbind(summaryTable, 
            c(provincie = "Vlaanderen", as.list(apply(summaryTable[, levelsCategorie], 2, sum))))
        summaryTable <- cbind(summaryTable, 
            Totaal = apply(summaryTable[, levelsCategorie], 1, sum))
        
        
        return(summaryTable)
        
      })
  
  
  if (categorie == "typePercent") {
    
    percentages <- summaryTables[[1]][, -1]/summaryTables[[2]][, -1]
    summaryTable <- cbind(provincie = summaryTables[[1]][, 1], percentages)
    
  } else {
    
    summaryTable <- summaryTables[[1]]
    
  }
  
  
  # B. Calculate differences with 1, 5, 10 years ago
  
  finalTable <- summaryTable
  
  
  # Select data
  for (yearsBack in c(1, 5, 10)) {
    
    freqBack <- count(allData[allData$jaar == (jaar - yearsBack), ], 
        vars = "provincie")
    
    # Only calculate trend if relevant
    if (nrow(freqBack) > 0) {
      
      # Add provinces with 0 observations
      freqBack <- merge(x = data.frame(provincie = unique(allData$provincie)),
          y = freqBack, all = TRUE)
      freqBack$freq[is.na(freqBack$freq)] <- 0
      
      # Calculate percentages
      if (categorie == "typePercent") {
        
        freqBackAssigned <- count(assignedData[assignedData$jaar == (jaar - yearsBack), ],
            vars = "provincie", wt_var = "totaal")
        freqBackAssigned$totaal <- freqBackAssigned$freq
        freqBackAssigned$freq <- NULL
        
        freqBack <- merge(x = freqBack, y = freqBackAssigned, all = TRUE)
        freqBack$percent <- freqBack$freq/freqBack$totaal
        
      }
      
      # Add row for Vlaanderen
      levels(freqBack$provincie) <- c(levels(freqBack$provincie), "Vlaanderen")
      if (categorie == "typePercent")
        freqBack <- rbind(freqBack, list(provincie = "Vlaanderen", 
                freq = sum(freqBack$freq, na.rm = TRUE), 
                totaal = sum(freqBack$totaal, na.rm = TRUE), 
                percent = sum(freqBack$freq, na.rm = TRUE)/sum(freqBack$totaal, na.rm = TRUE))) else 
        freqBack <- rbind(freqBack, list(provincie = "Vlaanderen", 
                freq = sum(freqBack$freq, na.rm = TRUE)))
      
      freqBack$totaal <- NULL
      
      
      # Calculate trend
      finalTable <- join(x = finalTable, y = freqBack, by = "provincie")
      finalTable[, paste0("Verandering tov ", yearsBack, " jaar (", jaar - yearsBack, ")")] <- 
          ifelse(summaryTables[[1]]$Totaal > minForTrend & 
                  finalTable$freq > minForTrend,
              { if (categorie == "typePercent") 
                  value <- sprintf("%.1f", round((finalTable$Totaal - finalTable$percent)*100, 1)) else
                  value <- sprintf("%.1f", round((finalTable$Totaal/finalTable$freq - 1)*100, 1))
                ifelse(value > 0, paste0("+", gsub(pattern = "\\.", "\\,", value), "%"), 
                    paste0(gsub(pattern = "\\.", "\\,", value), "%"))},
              "")
      
      finalTable$freq <- NULL
      finalTable$percent <- NULL
      finalTable[is.na(finalTable)] <- ""
      
    }
    
  }
  
  
  rowOrder <- match(c(levelsProvincie, "Vlaanderen"), finalTable$provincie)
  toReturn <- finalTable[rowOrder[!is.na(rowOrder)], c("provincie", levelsCategorie, "Totaal",
          names(finalTable)[grep(pattern = "Verandering", x = names(finalTable))])]
  
  # If the selected year is not relevant, return NULL
  if (all(toReturn$Totaal == "Inf"))
    return(NULL)
  
  
  if (categorie == "typePercent")
    toReturn[, c(levelsCategorie, "Totaal")] <- 
        sapply(toReturn[, c(levelsCategorie, "Totaal")], function(x)
              paste0(round(x*100), "%")) 
  
  # Rename provincie
  names(toReturn)[names(toReturn) == "provincie"] <- "Provincie"
  
  
  return(toReturn)
  
}
