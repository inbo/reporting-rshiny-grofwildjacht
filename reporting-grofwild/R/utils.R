# Helper functions to format the data
# 
# Author: mvarewyck
###############################################################################


#' List user-friendly full names to replace R coding names
#' 
#' Will not return NA, but rather the R coding name in case
#' no match could be found.
#' 
#' @param x character, what to transform
#' @param type character, defines how to transform short to full names
#' @param rev boolean, whether to switch names and values
#' @return character vector
#' 
#' @author mvarewyck
#' @export
fullNames <- function(x, type = c("wildschade", "maanden"), rev = FALSE) {
  
  type <- match.arg(type)
  
  new <- switch(type,
    wildschade = c(
      # SchadeBasisCode
      "Gewas" 	          = "GEWAS",
      "Voertuig" 	        = "VRTG",
      "Andere" 	          = "ANDERE",
      # SchadeCode
      "Woelschade"				= "WLSCHD",
      "Vraatschade" 			= "VRTSCHD",
      "Graafschade"       = "GRFSCHD",
      "Gewas - andere"    = "GEWASANDR", 
      "Veegschade"        = "VGSCHD",
      "Verkeersongeluk zonder letsel" 	  = "GNPERSLTSL",
      "Verkeersongeluk met letsel" 		    = "PERSLTSL",
      "Verkeersongeluk onbekend" 					= "ONBEKEND",
      "Valwild"					                  = "VALWILD"
    ),
    maanden = c(
      "januari" = "January",
      "februari" = "February",
      "maart" = "March",
      "april" = "April",
      "mei" = "May",
      "juni" = "June",
      "juli" = "July",
      "augustus" = "August",
      "september" = "September",
      "oktober" = "October",
      "november" = "November",
      "december" = "December" 
    )
  )
  
  
  
  if (rev) {
    toReturn <- names(new)
    names(toReturn) <- new
    
  } else {
    toReturn <- new
  }
  
  
  result <- toReturn[match(x, toReturn)]
  
  ## in case there was no match (i.e. because new schade code(s))
  ## keep the raw name anyway instead of NA
  if(any(is.na(result))){
    
    naPosition <- which(is.na(result))
    result[naPosition] <- x[naPosition]
    names(result)[naPosition] <- x[naPosition]
  }
  
  return(result)
  
}


#' Replace english months detected in vector elements with Dutch month names
#' 
#' Will not return NA, but rather the original content if
#' no match could be found. In case a factor is supplied, 
#' a factor will be returned keeping the original order of the levels, but 
#' with adjusted months. 
#' 
#' @param x character or factor, what to transform
#' @return character or factor vector, depending on what was supplied
#' 
#' @author eadriaensen
#' @export
setMonthsInDutch <- function(x) {
  
  class <- class(x)
  
  if (class == "factor") {
    levels <- levels(x)
    x <- as.character(x)
  } else if (class != "character") {
    stop("This function can only work on vectors of class 'factor' or 'character'.")
  }
  
  dictionary <- data.frame("english" = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
    "nederlands" = c("januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus", "september", "oktoboer", "november", "december"),
    stringsAsFactors = FALSE)
  
  result <- sapply(x, function(element) {
      
      matched <- sapply(dictionary[["english"]], function(englishElement) {
          grepl(englishElement, element)
        })
      if (!any(matched)) {
        
        return(element)
        message("Warning: the following element could not be translated and is kept as is: ", element ," Please make sure there are no typos in the supplied vector.")
      } else {
        ind <- which(matched)
        gsub(dictionary[ind, "english"], dictionary[ind, "nederlands"], element)
      }
      
    })
  
  if (class == "factor") {
    
    levelsAdjusted <- sapply(levels, function(element) {
        
        matched <- sapply(dictionary[["english"]], function(englishElement) {
            grepl(englishElement, element)
          })
        if (!any(matched)) {
          
          return(element)
          message("Warning: the following element could not be translated and is kept as is: ", element ," Please make sure there are no typos in the supplied vector.")
        } else {
          ind <- which(matched)
          gsub(dictionary[ind, "english"], dictionary[ind, "nederlands"], element)
        }
        
      })
    
    result <- factor(result, levels = levelsAdjusted)
    
  }
  
  return(result)
  
}


#' Name file given content information
#' @param species character, species of the file content
#' @param year numeric vector, year span of the file content 
#' @param extraInfo character, optional extra info to add e.g. type; default is NULL
#' @param content character, more information on the file
#' @param fileExt character, extension of the file
#' @return character, suggested file name pasting together \code{species},
#' \code{year}, \code{content}, \code{fileExt}
#' @author mvarewyck
#' @export
nameFile <- function(species, year, extraInfo = NULL, content, fileExt) {
  
  paste0(
    if (length(species) > 0)
      paste0(paste(gsub(pattern = " ", replacement = "_", x = species), collapse = "-"), "_"),
    if (length(year) > 1) paste(year, collapse = "-") else year,
    if (!is.null(extraInfo)) {paste0("_", paste(extraInfo, collapse = "-"))}, 
    "_", content, 
    ".", fileExt
  )
  
}




#' Capitalize first letter
#' @param names character vector, names to be capitalized (e.g. countries)
#' @return character vector, capitalized version of \code{names}
#' @author mvarewyck
#' @export
simpleCap <- function(names) {
  
  sapply(names, function(x) {
      
      if (is.na(x))
        return(x)
      
      s <- tolower(as.character(x))
      paste0 (toupper(substring(s, 1, 1)), substring(s, 2))
      
    })
  
}



#' Paste elements of vector into string vector (for testing)
#' @param x vector
#' @return string of the form "c(<elements of x>)"
pasteToVector <- function(x) {
  
  if (is.character(x))
    elements <- paste(paste0("'", x, "'"), collapse = ", ")
  else elements <- paste(x, collapse = ", ")
  
  paste0("c(", elements, ")")
  
}


#' Define season from date
#' @param dates date vector, format \code{"d/m/Y"}
#' @return character vector with respective season 
#' @author mvarewyck
#' @export
getSeason <- function(dates) {
  
  winterStart <- as.Date("21/12/2012", format = "%d/%m/%Y")
  lenteStart <- as.Date("21/3/2012", format = "%d/%m/%Y")
  zomerStart <- as.Date("21/6/2012", format = "%d/%m/%Y")
  herfstStart <- as.Date("21/9/2012", format = "%d/%m/%Y")
  
  # Convert dates from any year to 2012 dates - leap year
  d <- as.Date(strftime(dates, format="%d/%m/2012"), format = "%d/%m/%Y")
  
  season <- ifelse (d >= winterStart | d < lenteStart, "winter",
    ifelse (d >= lenteStart & d < zomerStart, "lente",
      ifelse (d >= zomerStart & d < herfstStart, "zomer", "herfst")))
  
  factor(season, levels = c("winter", "lente", "zomer", "herfst"))
  
}


#' Replicate inbo colors if more than 9 needed
#' @param nColors integer, number of colors needed
#' @return list with
#' colors = character vector with (repeated) inbo colors
#' warning = character, not NULL if colors are repeated
#' 
#' @author mvarewyck
#' @importFrom INBOtheme inbo_palette
#' @export
replicateColors <- function(nColors) {
  
  rest <- nColors%%9
  times <- floor(nColors/9)
  
  colors <- c()
  
  if (times > 0)
    colors <- rep(inbo_palette(n = 9), times)
  if(rest > 0)
    colors <- c(colors, inbo_palette(n = rest))
  
  colors <- rev(colors)
  
  # warning if noLocaties exceeds 9 colours
  warningText <- NULL
  if(nColors > 9) {
    warningText <- "Door de ruime selectie werden de kleuren van deze grafiek hergebruikt. 
      Hierdoor is verwarring mogelijk. Verklein de selectie om dit te voorkomen."
  }
  
  return(
    list(
      colors = colors, 
      warning = warningText
    )
  )

  
}
#' get path of report available in the package
#' @return string with path of report
#' @author Laure Cougnaud and Kirsten Van Hoorde
#' @export
getPathReport <- function(){
  
  basePathReport <- system.file("report", package = "reportingGrofwild")
  pathReport <- dir(basePathReport , pattern = "grofWild_results.Rmd", full.names = TRUE)
  
  return(pathReport)
  
}

#' get path of css file
#' @return string with path of css file
#' @author Laure Cougnaud
#' @export
getPathCss <- function(){
  
  basePath <- system.file("report", package = "reportingGrofwild")
  pathFile <- dir(basePath, pattern = "custom.css", full.names = TRUE)
  
  return(pathFile)
  
}

#' get path of file contained in the 'figure' folder of the report
#' @param figureName string name of the figure file
#' ('graph[1-3].png')
#' @return string with path of figure
#' @author Laure Cougnaud
#' @export
getPathFigure <- function(figureName){
  
  basePath <- system.file("report/figure", package = "reportingGrofwild")
  pathFile <- dir(basePath, pattern = figureName, full.names = TRUE)
  
  return(pathFile)
  
}