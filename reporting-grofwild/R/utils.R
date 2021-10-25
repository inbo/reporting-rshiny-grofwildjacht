# Helper functions
# 
# Author: mvarewyck
###############################################################################




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
    paste(gsub(pattern = " ", replacement = "_", x = species), collapse = "-"), "_",
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
    colors <- rep(rev(inbo_palette(n = 9)), times)
  if(rest > 0)
    colors <- c(colors, rev(inbo_palette(n = rest)))
  
  # warning if noLocaties exceeds 9 colours
  if(nColors > 9) {
    warning <- "Door het hoog aantal gekozen regio's werden de kleuren van deze grafiek hergebruikt. 
      Hierdoor is verwarring mogelijk. Selecteer minder regio's om dit te voorkomen."
  } else {
    warning <- NULL
  }
  
  return(
    list(
      colors = colors, 
      warning = warning
    )
  )

  
}
