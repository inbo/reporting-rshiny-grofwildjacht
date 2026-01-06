#' Get title for a 'Category' tab
#' @inheritParams reportingGrofwild-common-args
#' @return string with category title
#' @author lcougnaud
#' @export
getCategoryTitle <- function(category){
  
  category <- as.character(category)
  
  title <- switch(category,
    draagvlak = "Draagvlak",
    populatie = "Populatie",
    links = "Links",
    tools::toTitleCase(sub("-", " ", category))
  )
  
  return(title)
  
}

#' Get title for the 'Subcategory' tab(s)
#' @param subcategory character with subcategory of interest,
#' to be extracted from the \code{plotFunction} column
#' of \code{uiText}
#' @inheritParams reportingGrofwild-common-args
#' @return named character vector with tab titles
#' @author lcougnaud
#' @export
getSubcategoryTitle <- function(subcategory, uiText){
  
  matchId <- which(uiText$plotFunction == subcategory)

  if (length(matchId) == 0)
    subcategory else 
    uiText[matchId, "title"]
  
}

#' Get output title
#' @inheritParams reportingGrofwild-common-args
#' @param specie (optional) character vector of length 1 with specie
#' @param type (optional) character vector of length 1 with type
#' @param n (optional) integer vector of length 1 with maximum 
#' number of characters to include
#' @inheritParams reportingGrofwild-common-args
#' @return character vector of length 1 with plot title
#' @author lcougnaud
#' @export
getOutputTitle <- function(output, 
  uiText, specie = NULL, type = NULL, n = integer(), regioSchaal = NULL){

  # check if output name formatted as 'output-[type]'
  outputInfo <- strsplit(output, split = "-")[[1]]
  
  # Try matching on whole string, otherwise take first part
  matchId <- which(uiText$plotFunction == output)
  if (length(matchId) == 0) {
    
    matchId <- which(uiText$plotFunction == outputInfo[1])
    type <- if (length(outputInfo) > 1) outputInfo[2]
      
  }
  
  title <- if (length(matchId) == 0)
    output else
    uiText[matchId, "title"] 
  
  if(!is.null(specie)){
    title <- gsub("{wildsoort}", tolower(specie), title, fixed = TRUE)
    title <- gsub("{wildsoorten}", 
      switch(specie, 
        "Ree" = "ree\u00EBn",
        "Wild zwijn" = "wilde zwijnen",
        "Damhert" = "damherten",
        "Edelhert" = "edelherten",
        specie
      ), title, fixed = TRUE
    )
  }else{
    regex <- paste0("( ", c("op", "voor", "van"), "[ [:alpha:]{1,}]*", ")*")
    regex <- paste(regex, collapse = "")
    regex <- paste0(regex, " \\{wildsoort(en)*\\},*")
    title <- sub(regex, "", title)
  }
  
  
  # Replace regio-schaal
  if (!is.null(regioSchaal))
    title <- gsub("{regio-schaal}", regioSchaal, title, fixed = TRUE)
  
  if(!is.null(type) && !is.na(type)){
    if (type == "schade")	type <- "wildschade"
    title <- gsub("{type}", type, title, fixed = TRUE)
  }
  
  if(length(n) > 0 && nchar(title) > n)
    title <- paste0(substr(x = title, start = 1, stop = n), "...")
  
  return(title)
  
}

#' Get output description
#' @param maxDate date, the last observation date to be 
#' replaced in the text
#' @param statsMap character, statistics to be printed 
#' instead of \code{'{{statsMap}}'}
#' @param context character, where the plot is shown, should
#' corresponds to a column in \code{uiText}
#' @inheritParams getOutputTitle
#' @return character vector of length 1 with output description
#' @author lcougnaud
getOutputDescription <- function(output, 
  uiText, context = "summary", 
  specie = NULL, type = NULL, statsMap = NULL,
  maxDate = NULL){
  
  text <- uiText[which(uiText$plotFunction == output), context]
  
  if(length(text) == 0)
    return(NULL)
  
  if(!is.null(specie)){
    text <- gsub("{wildsoort}", tolower(specie), text, fixed = TRUE)
    text <- gsub("{wildsoorten}", 
      switch(specie, 
        "Ree" = "ree\u00EBn",
        "Wild zwijn" = "wilde zwijnen",
        "Damhert" = "damherten",
        "Edelhert" = "edelherten",
        specie
      ), text, fixed = TRUE
    )
  }
  
  if(!is.null(type)){
    if(type == "schade")	type <- "schadegevallen"
    text <- gsub("{type}", type, text, fixed = TRUE)
  }
  
  if (grepl("\\{\\{statsMap\\}\\}", text))
    text <- gsub(
      pattern = "\\{\\{statsMap\\}\\}", 
      replacement = if (!is.null(statsMap)) paste0(statsMap, ".") else "", 
      x = text
    )
  
  # Fill in hover text if required
  if (grepl("\\{\\{\\{hover_", text)) {
    matches <- stringr::str_extract_all(text, "\\{\\{\\{hover_.*?\\}\\}\\}")[[1]]
    keys <- stringr::str_match(matches, "\\{\\{\\{(.*?)\\}\\}\\}")[,2]
    for (i in seq_along(keys)) {
      text <- gsub(
        pattern = paste0("\\{\\{\\{", keys[i], "\\}\\}\\}"), 
        replacement = paste0("<b>", stringr::str_to_title(gsub("_", " ", gsub("hover_", "", keys[i]))), "</b>: ", uiText[which(uiText$plotFunction == keys[i]), context]), 
        x = text
      )
    }
  }
  
  # Replace last date
  if (!is.null(maxDate))
    text <- gsub("\\{\\{maxDate\\}\\}", 
      format(maxDate, "%d/%m/%Y"), text)
  
  if(!is.null(specie)){
    splitText <- strsplit(text, split = "\\{")[[1]]
    text <- sapply(splitText, function(x)
        if (grepl("\\}", x)) {
          doInvert <- grepl("\\!", strsplit(x, "\\}")[[1]][1])
          doSpecies <- grepl(specie, strsplit(x, "\\}")[[1]][1])
          if (!doInvert & doSpecies)
            strsplit(x, "\\}")[[1]][2] else if (doInvert & !doSpecies)
            strsplit(x, "\\}")[[1]][2] else 
            ""
        } else {
          x
        } 
    )
  }
  
  # Handling embedded quoting
  text <- gsub("\\\\", "\"", text)

  return(text)
  
}

#' Get available outputs for a specified specie
#' 
#' @section Outputs available by specie:
#' The outputs are determined based on the available data
#' for the specific specie.
#' @param specie character with specie
#' @inheritParams reportingGrofwild-common-args
#' @return character vector with available outputs
#' @author lcougnaud
#' @export
getOutputSpecie <- function(specie,
  geoData, ecoData, openingstijdenData,
  schadeData, waarnemingenData, draagvlakData){
 
  ## filter all datasets by specie
  
  geoDataSpecie <- geoData[which(geoData$wildsoort == specie), ]
  
  ecoDataSpecie <- ecoData[which(ecoData$wildsoort == specie), ]
  
  waarnemingenDataSpecie <- waarnemingenData[
    which(waarnemingenData$wildsoort == specie), ]
  
  # only for yearlyShotAnimalsUI
  # filtering actually more complex (season for specific year)
  # assumes there are eco data available if openingstijd specified
  openingstijdenDataSpecie <- openingstijdenData[
    which(openingstijdenData$Soort == specie), ]

  # countYearShotUI-leeftijd_comp
  combinedDataSpecie <- merge(
    x = ecoDataSpecie, 
    y = geoDataSpecie[, c("ID", "FaunabeheerZone")], 
    by = "ID"
  )
  
  # for countYearProvinceUI-afschot
#  if(specie == "2008")
#    ecoDataSpecie <- ecoData[which(ecoData$afshotjaar >= afschotjaar), ]
  
  # F04_3
  drukjachtDataID <- intersect(
    ecoDataSpecie[which(ecoDataSpecie$jachtmethode_comp == "Drukjacht"), ]$ID, 
    geoDataSpecie$ID
  )
  
  # schade
  schadeDataSpecie <- schadeData[
    which(schadeData$wildsoort == specie & schadeData$afschotjaar >= 2014),
  ]  
  
  # populatie
  
  # Embryos
  typesFemale <- getFemaleTypes(
    ecoData = ecoDataSpecie, 
    specie = specie
  )
  combinedDataEmbryo <- getEmbryoData(
    data = combinedDataSpecie,
    type = typesFemale
  )
  
  # Toekomstige Verspreiding
  spreadData <- loadSpreadData()
  spreadDataNbRows <- lapply(spreadData, function(df) nrow(df[!is.na(df$wildsrt) & df$wildsrt == specie,]))

  outputs <- c(
#    "woordenlijstPlaceholder",
    if(nrow(waarnemingenDataSpecie) > 0 | nrow(geoDataSpecie) > 0)
      "F17_1",
    if(nrow(geoDataSpecie) > 0)
      c("trendYearFlandersUI", "mapFlandersUI", "kencijferUI"),
    if(nrow(combinedDataSpecie) > 0)
      c("countYearShotUI-leeftijd_comp"),
    if(any(combinedDataSpecie$afschotjaar >= 2014))
      c("countYearShotUI-jachtmethode_comp", "countYearShotUI-wettelijk_kader", "countYearShotUI-periode"),
    if (specie == "Ree")
      "afschotAanvraagReewild",
    if(nrow(ecoDataSpecie) > 0)
      c(
        # beheer
        "countYearProvinceUI-afschot", "tableProvinceUI",
        # populatie
        "countAgeGenderUI"
      ),
    if(nrow(ecoDataSpecie) > 0 & nrow(openingstijdenDataSpecie) > 0)
      c("yearlyShotAnimalsUI"),
    if(length(drukjachtDataID) > 0)
      c("F04_3"),
    if(nrow(schadeDataSpecie) > 0)
      c(
        "tableSchadeSummaryUI", "trendYearFlandersUI-schade", 
        "countYearProvinceUI-schade",
        "countYearSchadeUI-wildschade", "tableSchadeUI",
        "countYearSchadeUI-seizoen"
      ),
    if(!all(is.na(schadeDataSpecie$SoortNaam))) {
      c("countYearSchadeUI-gewas", "tableGewasUI")
    },
    if(!all(is.na(schadeDataSpecie$schadeBedrag)))
      "barCostUI",
    if(nrow(schadeDataSpecie) > 0)
      c("mapFlandersUI-schade", "mapSchadeUI"),
    if(specie == "Ree" & any(combinedDataSpecie$afschotjaar >= 2014)  ||
      nrow(combinedDataSpecie) > 0)
      c("boxAgeWeightUI"),
    if(specie == "Ree" & any(ecoDataSpecie$afschotjaar >= 2005)  ||
      nrow(ecoDataSpecie) > 0)
      c("countAgeCheekUI"),
    if(any(ecoDataSpecie$geslacht_comp == "Vrouwelijk"))
      "countAgeGroupUI",
    if(nrow(combinedDataEmbryo) > 0)
      "countEmbryosUI",
    if (any(spreadDataNbRows > 0) | specie == "Bever")
      "mapSpreadUI",
    # Maatschappelijk draagvlak
    if (specie %in% draagvlakData$aanwezigheid$Soort)
      "F14_1",
    if (specie %in% draagvlakData$aantrekkingskracht$Soort)
      "F14_2",
    if (specie %in% draagvlakData$impacts$Soort)
      "F14_3",
    if (specie %in% draagvlakData$maatregelen$Soort)
      "F14_4",
    if (specie %in% draagvlakData$beleid$Soort)
      "F14_5",
    if (!all(is.na(combinedDataSpecie$onderkaaklengte_comp)))
      "plotBioindicatorUI-onderkaaklengte",
    if (!all(is.na(combinedDataSpecie$ontweid_gewicht)))
      "plotBioindicatorUI-ontweid_gewicht",
    "biodiversiteitsportaal",
    "exotenportaal",
    if (specie == "Wolf")
      c(
        "tableWolfReproductionUI", "countGeneticWolvesUI"
      )
  )
    
  return(outputs)
  
}

#' Get information on available outputs (tables/visualizations)
#' by specie, including category and subcategory
#' 
#' Outputs specified in the file: '\code{output-info-blacklist.csv}'.
#' available in the packages are filtered.
#' @inheritSection getOutputSpecie Outputs available by specie
#' @inheritParams reportingGrofwild-common-args
#' @inheritDotParams getOutputSpecie
#' @return data.frame with specie, category, subcategory and output,
#' ordered as shown in the UI
#' @author lcougnaud
#' @export
getOutputInfo <- function(species, ...){
  
  # extract available outputs (tables/visualisations) - based on the data
  outputsBySpecie <- sapply(species, function(specie)
    getOutputSpecie(
      specie = specie,
      ...
    ),
    simplify = FALSE
  )
  
  # format as data.frame
  info <- lapply(names(outputsBySpecie), function(specie){
    
    outputs <- outputsBySpecie[[specie]]
    subcategories <- sapply(outputs, getSubcategoryOutput)
    categories <- getCategorySubcategory(subcategories)
    
    if (!is.null(outputs))
      data.frame(
        specie = specie, 
        category = categories, subcategory = subcategories, 
        output = outputs,
        stringsAsFactors = FALSE
      )
  })

  info <- do.call(rbind, info)
  rownames(info) <- NULL
  cols <- colnames(info)
  
  # filter output(s) for a specific specie from the blacklist
  blacklist <- read.csv(
    file = system.file("extdata", "output-info-blacklist.csv", 
      package = "reportingGrofwild")
  )
  
  if (nrow(blacklist) > 0 ) {
    blacklist <- do.call(rbind, lapply(1:nrow(blacklist), function(i) {
          if (is.na(blacklist$specie[i]) | !nchar(blacklist$specie[i]))
            data.frame(specie = unique(info$specie), output = blacklist$output[i]) else
            data.frame(specie = blacklist$specie[i], output = blacklist$output[i])
        }))
    blacklist$blacklist <- TRUE
    info <- merge(
      x = info, y = blacklist,
      all.x = TRUE, by = c("specie", "output"), sort = FALSE
    )
  } else {
    info$blacklist <- NA
  }
  
  info <- info[which(is.na(info$blacklist)), cols, drop = FALSE]
  
  # sort to have correct order in the UI
  info[, "specie"] <- factor(info[, "specie"], levels = species)
  info <- info[do.call(order, info[ , 1:3]), ]
  
   
  return(info)
  
}

#' Get a subcategory for an output
#' @inheritParams reportingGrofwild-common-args
#' @return string with subcategory(ies) name
#' @author lcougnaud
#' @export
getSubcategoryOutput <- function(output){
  
  # Should be unique in the entire app!
  subcategoryOutput <- list( 
      `beheer-vlaanderen` = c("trendYearFlandersUI"),
      `beheer-regio` = c("mapFlandersUI", "yearlyShotAnimalsUI", 
        "countYearProvinceUI-afschot"),
      `beheer-leeftijdcategorie` = 
          c("countYearShotUI-leeftijd_comp", "tableProvinceUI"),
      `beheer-jachtmethode` = 
          c("countYearShotUI-jachtmethode_comp", "F04_3", "countYearShotUI-wettelijk_kader", "countYearShotUI-periode"),
      `beheer-afschotplan` = 
        c("afschotAanvraagReewild"),
      
      # schade
      `schade-vlaanderen` = c(
          "tableSchadeSummaryUI", "trendYearFlandersUI-schade"
      ),
      `schade-regio` =  c("countYearProvinceUI-schade", "mapFlandersUI-schade", 
        "mapSchadeUI"),
      `schade-type-gewas` = c("countYearSchadeUI-gewas", "tableGewasUI"),
      `schade-type-schade` = c("countYearSchadeUI-wildschade", "tableSchadeUI"),
      `schade-seizoen` = c("countYearSchadeUI-seizoen"),
      `schade-kosten` = "barCostUI",
      
      # populatie
      `populatie-leeggewicht` = c("boxAgeWeightUI", "plotBioindicatorUI-ontweid_gewicht"),
      `populatie-onderkaak` = c("countAgeCheekUI", "plotBioindicatorUI-onderkaaklengte"),
      `populatie-geslacht` = "countAgeGenderUI",
      `populatie-voortplanting` = c("countAgeGroupUI", "countEmbryosUI", "tableWolfReproductionUI"),
      `populatie-genetica` = "countGeneticWolvesUI",
      
      # verspreiding
      `verspreiding-huidig` = c("F17_1", "kencijferUI"),
      `verspreiding-toekomstig` = "mapSpreadUI",
      
      # draagvlak
      `draagvlak-surveys` = c("F14_1", "F14_2", "F14_3", "F14_4", "F14_5"),
      
      #interne links
      `links-internelinks` = c("biodiversiteitsportaal", "exotenportaal")
  
#      # woordenlijst
#      `woordenlijst-placeholder` = "woordenlijstPlaceholder"
    
  )
  
  isSubcat <- sapply(subcategoryOutput, function(outputs){
    any(output %in% outputs)    
  })
  
  subcategories <- names(which(isSubcat))
  
  subcategories <- factor(subcategories, levels = names(subcategoryOutput))
  
  return(subcategories)
  
}

#' Get subcategories available in the app
#' @inheritParams reportingGrofwild-common-args
#' @author lcougnaud
#' @export
getCategorySubcategory <- function(subcategory){
  
  categories <- sapply(strsplit(as.character(subcategory), split = "-"), function(x) x[1])
  categories <- factor(categories, levels = c("beheer", "schade", "populatie", 
    "verspreiding", "draagvlak", "links"))
  
  return(categories)
  
}

#' Get available specie, categories, subcategories or outputs
#' for a (optionally) specified specie, category, subcategory
#' @param specie string with specie
#' @param subcategory string, with subcategory of interest
#' @param output string, with output of interest 
#' @param variable string with variable of interest; should be one of  
#' \code{c("specie", "category", "subcategory", "output")}
#' @param infoOutput data.frame with information on available
#' outputs, as returned by \code{\link{getOutputInfo}}
#' @param defaults (optional) named character vector with defaults
#' for the category, subcategory and output.
#' @return character vector with elements of \code{variable}
#' @author lcougnaud
#' @inheritParams reportingGrofwild-common-args
#' @export
getInfo <- function(
  specie = NULL, category = NULL, subcategory = NULL, output = NULL,
  infoOutput, defaults = NULL, 
  variable = c("specie", "category", "subcategory", "output")){

  variable <- match.arg(variable)
  
  select <- function(var, name){
    keep <- is.null(var) || 
    (!is.null(defaults[[name]]) && var == defaults[[name]])
    return(!keep)
  }

  if(select(var = specie, name = "specie"))
    infoOutput <- infoOutput[which(infoOutput$specie == specie), ]
  
  if(select(var = category, name = "category"))
    infoOutput <- infoOutput[which(infoOutput$category == category), ]
  
  if(select(var = subcategory, name = "subcategory"))
    infoOutput <- infoOutput[which(infoOutput$subcategory == subcategory), ]
  
  if(select(var = output, name = "output"))
    infoOutput <- infoOutput[which(infoOutput$output == output), ]
  
  results <- unique(as.character(infoOutput[order(infoOutput$category), variable]))
  
  return(results)
  
}


#' Group species for selectInput choices
#' @param allSpecies data.frame as returned by \code{\link{loadWildsoorten}}
#' @param selectedSpecies character vector, species to be retained;
#' if NULL all species are retained
#' @return named list with groups and selected species per group
#' 
#' @author mvarewyck
#' @export
groupSpecies <- function(allSpecies, selectedSpecies = NULL) {
  
  if (!is.null(selectedSpecies))
    subSpecies <- allSpecies[allSpecies$name %in% selectedSpecies, ] else
    subSpecies <- allSpecies
  
  toReturn <- sapply(unique(subSpecies$group), function(x) {
      temp <- subSpecies$name[subSpecies$group == x]
      if (length(temp) == 1) temp <- c("", temp)
      temp
    }, simplify = FALSE)
  
  # For single species remove group names
  if (length(toReturn) == 1) {
    
    names(toReturn) <- NULL
    toReturn <- toReturn[[1]]
    
  }
    
  toReturn
  
}



#' Replacing ids by labels for the \code{\link{getInfo}}
#' @param infoOutput data.frame as returned by \code{\link{getInfo}}
#' @inheritParams getOutputTitle 
#' @return data.frame
#' 
#' @author mvarewyck
#' @export
getInfoList <- function(infoOutput, uiText) {
  
  subInfo <- infoOutput
  
  # Ignore species choice
  subInfo$specie <- NULL
  subInfo <- subInfo[!duplicated(subInfo), ]
  
  # Category level 
  categoryInfo <- subInfo[!duplicated(subInfo[, c("category")]), ]
  categoryInfo$subcategory <- ""
  categoryInfo$output <- ""
  categoryInfo$id <- categoryInfo$category
  
  # Subcategory level 
  subcategoryInfo <- subInfo[!duplicated(subInfo[, c("category", "subcategory")]), ]
  subcategoryInfo$output <- ""
  subcategoryInfo$id <- subcategoryInfo$subcategory
  
  # Output level
  subInfo$id <- subInfo$output
  fullInfo <- do.call(rbind, list(categoryInfo, subcategoryInfo, subInfo))
  # Replace id by label
  fullInfo$category <- sapply(fullInfo$category, getCategoryTitle)
  fullInfo$subcategory <- sapply(as.character(fullInfo$subcategory), function(x)
      getSubcategoryTitle(x, uiText = uiText))
  fullInfo$output <- sapply(fullInfo$output, function(x) getOutputTitle(x, uiText = uiText))
  
  fullInfo$label <- ifelse(fullInfo$output == "", ifelse(fullInfo$subcategory == "", 
      fullInfo$category, fullInfo$subcategory), fullInfo$output)
  fullInfo$id <- as.character(fullInfo$id)
  
  fullInfo
  
}


#' Restrict choices in navbar menu to currently relevant choices
#' @param allChoices character vector, all available choices
#' @param currentChoices character vector, all relevant choices. Only these should be shown
#' @return no return value; manipulation of UI
#' 
#' @author mvarewyck
#' @export
resetNavbarChoices <- function(allChoices, currentChoices) {
  
  for (iChoice in allChoices)
    if (iChoice %in% currentChoices)
      # Show
      bslib::nav_show(id = "navbarID", target = iChoice) else
      # Hide
      bslib::nav_hide(id = "navbarID", target = iChoice)
  
}
