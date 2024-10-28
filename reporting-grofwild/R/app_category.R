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