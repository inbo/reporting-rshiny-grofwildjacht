#' Get title for a specific tab of the 'Category' page.
#' 
#' This is used for the title of the tab and the path.
#' @param value character of length 1 with 
#' \code{\link[shiny]{tabPanel}} value
#' @param category character of length 1 with the name of the
#' category page.
#' @return character of length 1 with 
#' \code{\link[shiny]{tabPanel}} title
#' @author lcougnaud
getTabTitle <- function(value, category){
  
  title <- switch(category,
     afschot = 
      switch(value,
        vlaanderen = "Afschot in Vlaanderen",
        regio =  "Afschot per regio",
        leeftijdcategorie = "Afschot per leeftijdscategorie",
        jachtmethode = "Afschot per jachtmethode"
      )
  )
  
  return(title)
  
}

#' Get plot title
#' @param plot character vector of length 1 with plot title
#' @param specie character vector of length 1 with specie
#' @param n (optional) integer vector of length 1 with maximum 
#' number of characters to include
#' @param uiText data.frame with plot titles
#' @return character vector of length 1 with plot title
#' @author lcougnaud
getPlotTitle <- function(plot, specie = NULL, n = integer(), uiText){
  
  title <- uiText[uiText$plotFunction == plot, "title"]
  
  if(!is.null(specie))
    title <- gsub("{wildsoort}", specie, title, fixed = TRUE)
  
  if(length(n) > 0)
    title <- paste0(substr(x = title, start = 1, stop = n), "...")
  
  return(title)
  
}

#' Get plot description
#' @inheritParams getPlotTitle
#' @return character vector of length 1 with plot description
#' @author lcougnaud
getPlotDescription <- function(plot, specie = NULL, uiText){
  
  title <- uiText[uiText$plotFunction == plot, "fauna"]
  
  if(!is.null(specie))
    title <- gsub("{wildsoort}", specie, title, fixed = TRUE)
  
  return(title)
  
}