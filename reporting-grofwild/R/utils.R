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
fullNames <- function(x, type = "maanden", rev = FALSE) {
  
  type <- match.arg(type)
  
  new <- switch(type,
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
nameFile <- function(species, year = NULL, extraInfo = NULL, content, fileExt) {
  
  paste0(
    if (length(species) > 0)
      paste(gsub(pattern = " ", replacement = "_", x = species), collapse = "-"),
    if (!is.null(year)) paste0("_", if (length(year) > 1) paste(year, collapse = "-") else year),
    if (!is.null(extraInfo)) {paste0("_", paste(extraInfo, collapse = "-"))}, 
    "_", content, 
    ".", fileExt
  )
  
}




#' Capitalize first letter
#' @param names character vector, names to be capitalized (e.g. countries)
#' @param keepNames boolean, whether to keep the names for the returned vector;
#' default is TRUE
#' @return character vector, capitalized version of \code{names}
#' @author mvarewyck
#' @export
simpleCap <- function(names, keepNames = TRUE) {
  
  sapply(names, function(x) {
      
      if (is.na(x))
        return(x)
      
      s <- tolower(as.character(x))
      paste0 (toupper(substring(s, 1, 1)), substring(s, 2))
      
    }, USE.NAMES = keepNames)
  
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


#' Generate text with percentage collected as annotation for plots
#' @param nAvailable integer, number of subjects for which data is available (non-missing)
#' @param nTotal integer, total number of subjects
#' @param text character, explains how the filtering for \code{nAvailable} is done
#' @return character, annotation text for the plot
#' 
#' @author mvarewyck
#' @export
percentCollected <- function(nAvailable, nTotal, text) {
  
  percentage <- round(nAvailable / nTotal * 100, 1)
  
  paste0(percentage, "% met ", text, " (", format(nAvailable, big.mark = " "), 
    " / ", format(nTotal, big.mark = " "), ")")
  
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

#' get path of INBO logo file
#' 
#' @return character, path of logo file
#' @author mvarewyck
#' @export
getPathLogo <- function() {
  
  system.file("ui/www", "logo.png", package = "reportingGrofwild")
  
}



#' Disclaimer text when limited information is available
#' 
#' @param doHTML boolean, whether to format the text with html tags
#' @return character
#' 
#' @author mvarewyck
#' @importFrom htmltools tags
#' @export
getDisclaimerLimited <- function(doHTML = TRUE) {
  
  myText <- "\U002A Van deze indicator zijn slechts gedeeltelijke gegevens beschikbaar voor de gekozen schaal"

  if (doHTML)
    tags$p(tags$em(myText)) else
    myText

}


#' Style plotly object for Rmd report
#' 
#' @param myPlot plotly object
#' @return plotly object
#' 
#' @author mvarewyck
#' @import plotly
#' @export
plotlyReport <- function(myPlot) {
  
  myPlot <- myPlot %>% config(displayModeBar = FALSE)
  
  # remove gridlines
  if (is.null(myPlot$x$layoutAttrs[[1]]$xaxis))
    myPlot$x$layoutAttrs[[1]]$xaxis <- list(showgrid = FALSE) else
    myPlot$x$layoutAttrs[[1]]$xaxis$showgrid <- FALSE
#  myPlot$x$layoutAttrs[[1]]$xaxis$ticks <- "outside"
  
  if (is.null(myPlot$x$layoutAttrs[[1]]$yaxis))
    myPlot$x$layoutAttrs[[1]]$yaxis <- list(showgrid = FALSE) else
    myPlot$x$layoutAttrs[[1]]$yaxis$showgrid <- FALSE
#  myPlot$x$layoutAttrs[[1]]$yaxis$ticks <- "outside"
  
  myPlot
  
}