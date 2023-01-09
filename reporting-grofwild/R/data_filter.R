# Filters for subsetting the data in plot functions
# 
# Author: mvarewyck
###############################################################################





#' Filter \code{plotData} based on the \code{indieningType} if required
#' @param plotData data.frame, to be filtered
#' @param sourceIndicator character, source used to filter \code{data} ('indieningType' column)
#' should be subset of \code{c("E-loket", "HVV", "Natuurpunt")}.
#' @param returnStop character, should be one of \code{c("message", "data")}
#' what needs to be returned if the filtered data has no rows left
#' @return data.frame, filtered version of \code{plotData}
#' 
#' @author mvarewyck
#' @export
filterSchade <- function(plotData, sourceIndicator = NULL,
  returnStop = c("message", "data")) {
  
  returnStop <- match.arg(returnStop)
  
  if (!is.null(sourceIndicator)) {
    if ("indieningType" %in% names(plotData)) {
      
      sourcesSchade <- loadMetaSchade()$sources  
      
      sources <- paste(unlist(sourcesSchade[sourceIndicator]), collapse = "|")
      plotData <- plotData[grepl(sources, plotData$indieningType), ]
      
    } else {
      
      plotData <- plotData[plotData$dataSource %in% sourceIndicator, ]
      
    }
    
    if (nrow(plotData) == 0)
      if (returnStop == "message")
        stop("Geen data beschikbaar voor de geselecteerde bron: ", paste(sourceIndicator, collapse = ", "), ". ")
    
  }
  
  return(plotData)
  
}  



#' Filter \code{plotData} based on leeftijd and geslacht bron
#' @inheritParams filterSchade
#' @param sourceIndicator_leeftijd character, levels of \code{leeftijd_comp_bron} to select on
#' @param sourceIndicator_geslacht character, levels of \code{geslacht_comp_bron} to select on
#' @param sourceIndicator_onderkaak character, levels of \code{onderkaaklengte_comp_bron} to select on
#' @param sourceIndicator_embryos character, levels of \code{aantal_embryos_bron} to select on
#' @return data.frame, filtered version of \code{plotData}
#' 
#' @author mvarewyck
#' @export
filterGrofwild <- function(plotData, sourceIndicator_leeftijd = NULL, 
  sourceIndicator_geslacht = NULL, sourceIndicator_onderkaak = NULL,
  sourceIndicator_embryos = NULL, returnStop = c("message", "data")) {
  
  returnStop <- match.arg(returnStop)
  
  if (!is.null(sourceIndicator_leeftijd) && !"leeftijd_comp_bron" %in% colnames(plotData))
    stop("Bron voor leeftijd niet in data")
  
  if (!is.null(sourceIndicator_geslacht) && !"geslacht_comp_bron" %in% colnames(plotData))
    stop("Bron voor geslacht niet in data")
  
  if (!is.null(sourceIndicator_onderkaak) && !"onderkaaklengte_comp_bron" %in% colnames(plotData))
    stop("Bron voor onderkaaklengte niet in data")
  
  if (!is.null(sourceIndicator_embryos) && !"aantal_embryos_bron" %in% colnames(plotData))
    stop("Bron voor aantal embryos niet in data")
  
  # To prevent error with R CMD check
  leeftijd_comp_bron <- NULL
  geslacht_comp_bron <- NULL
  
  if (!is.null(sourceIndicator_leeftijd) && sourceIndicator_leeftijd == "inbo") {
  
    # Special case: inbo leeftijd_comp distinguishes frisling <6m and >6m
    plotData$leeftijd_comp <- plotData$leeftijd_comp_inbo
    
    # filters out NA and 'meldingsformulier'
    plotData <- subset(plotData, leeftijd_comp_bron == "inbo")
    
  }
  
  if (!is.null(sourceIndicator_geslacht)) {
    if (sourceIndicator_geslacht == "inbo") {
      
      # filters out NA and 'meldingsformulier' en 'onbekend'
      plotData <- subset(plotData, geslacht_comp_bron == "inbo")
      
    } else if (sourceIndicator_geslacht == "both"){
      
      # filters out NA and 'onbekend'
      plotData <- subset(plotData, !is.na(geslacht_comp_bron) & geslacht_comp_bron != "onbekend")
      
    }
  }
  
  if (!is.null(sourceIndicator_onderkaak)) {
    
    # Bioindicator 'onderkaaklengte' depends on data source
    # bron == "both" -> onderkaaklengte_comp
    # bron == "inbo" -> onderkaaklengte_mm
    # bron == "meldingsformulier" -> mean(onderkaaklengte_links, onderkaaklengte_rechts)
     if (sourceIndicator_onderkaak == "both") {
        
        plotData$onderkaaklengte <- plotData$onderkaaklengte_comp
        
        # currently not done bc for some onderkaaklengte_comp values it is unclear at the
        # moment where they were determined (mf vs inbo)
        #			data <- subset(data, !is.na(onderkaaklengte_comp_bron)) 
                
      } else {
        
        plotData$onderkaaklengte_comp_bron <- sourceIndicator_onderkaak
        
        if (sourceIndicator_onderkaak == "inbo") 
          plotData$onderkaaklengte <- plotData$onderkaaklengte_mm else
          plotData$onderkaaklengte <- rowMeans(plotData[, c("onderkaaklengte_links", "onderkaaklengte_rechts")], na.rm = TRUE)
      }
      
    }
  
  if (!is.null(sourceIndicator_embryos)) {
    
    # Exclude records with missing source - NOT for countEmbryos()
#    plotData <- plotData[!is.na(plotData$aantal_embryos_bron), ]
    
    if (sourceIndicator_embryos == "both")
      plotData$embryos <- plotData$aantal_embryos else if (sourceIndicator_embryos == "inbo") 
      plotData$embryos <- plotData$aantal_embryos_labo else
      plotData$embryos <- plotData$aantal_embryos_MF
    
  }
  
  if (nrow(plotData) == 0)
    if (returnStop == "message")
      stop("Geen data beschikbaar. ")
  
  return(plotData)
  
}





#' Filter loaded \code{allSpatialData} for selected species, regionLevel and year 
#' @param allSpatialData list with SpatialPolygonsDataFrame as loaded by 
#' \code{load(file = file.path(system.file("extdata", package = "reportingGrofwild"), "spatialData.RData"))}
#' @param species character, animal species
#' @param regionLevel character, region level. Should be one of 
#' \code{c("flanders", "provinces", "communes", "faunabeheerzones", "fbz_gemeentes", "utm5", "WBE_binnengrenzen")}
#' @param year integer, year of interest. Only relevant when \code{regionLevel}
#' is "WBE_binnengrenzen". For all other regionlevels spatial data is fixed over the years
#' @param locaties character vector, only relevant when \code{regionLevel} is 
#' "WBE_binnengrenzen"; it selects the relevant WBE only; default is NULL
#' @return single SpatialPolygonsDataFrame
#' 
#' @author mvarewyck
#' @export
filterSpatial <- function(allSpatialData, species, 
  regionLevel = c("flanders", "provinces", "communes", "faunabeheerzones", 
    "fbz_gemeentes", "utm5", "utm1", "WBE", "WBE_buitengrenzen"), 
  year, locaties = NULL) {
  
  
  regionLevel <- match.arg(regionLevel)
  
  
  # Select correct spatial data
  if ("Wild zwijn" %in% species & regionLevel == "provinces") {
    
    spatialData <- allSpatialData[["provincesVoeren"]]
    
  } else if (grepl("WBE", regionLevel)) {
    
    spatialData <- allSpatialData[[paste0(regionLevel, "_", year)]]
    
  } else {
    
    spatialData <- allSpatialData[[regionLevel]]
    
  }
  
  if (!is.null(locaties))
    spatialData <- spatialData[spatialData$NAAM %in% locaties, ]
  
  return(spatialData)
  
}


#' Filter loaded \code{allSpatialData} for selected partijNummer
#' @inheritParams filterSpatial 
#' @param partijNummer numeric, partijnummer of the WBE to filter
#' @return list with SpatialPolygonsDataFrame, each of them filtered on selected WBE
#' 
#' @author mvarewyck
#' @export
filterSpatialWbe <- function(allSpatialData, partijNummer) {
  
  lapply(allSpatialData, function(iData) {
      
      iData[iData$NAAM %in% partijNummer, ]
      
    })
  
}



#' Create empty geographical dataset
#' @param data data.frame, example data of type 'geographical'
#' @param years numeric vector, years for which to create empty data (nrow)
#' @param kbo integer, kbo number of relevant WBE to create empty data for
#' @return data.frame empty data.frame with for each year the matching WBE and name
#' 
#' @author mvarewyck
#' @export
createEmptyGeo <- function(data, years, kbo) {
  
  matchingData <- loadRawData(type = "kbo_wbe")
  
  data[!is.na(data)] <- NA
  data$KboNummer_Toek <- kbo
  data$WBE_Naam_Toek <- matchingData$WBE.officieel[match(kbo, matchingData$KboNummer_Partij)]
  data$PartijNummer <- matchingData$PartijNummer[match(kbo, matchingData$KboNummer_Partij)]
  
  do.call(rbind, lapply(years, function(iYear) {
      
      toReturn <- data
      toReturn$afschotjaar <- iYear
      toReturn
      
    }))
  
}