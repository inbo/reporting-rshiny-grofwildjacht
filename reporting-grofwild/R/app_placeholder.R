# Placeholder functions for empty tiles
# 
# Author: mvarewyck
###############################################################################


#' Empty function - placeholder
#' @param id character, unique identifier
#' @return no return value
#' 
#' @author mvarewyck
#' @export
woordenlijstPlaceholderUI <- function(id) {
  
  
}
  
#' Empty function - placeholder
#' @inheritParams beheerCardServer
#' @return reactive specie
#' 
#' @author mvarewyck
#' @export
woordenlijstCardServer <- function(id, specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText) {
  
  moduleServer(id, function(input, output, session){
      
      return(specie)
      
    })
  
}  


#' Empty function - placeholder
#' @inheritParams reportingGrofwild-common-args
#' @return reactive specie
#' 
#' @author mvarewyck
#' @export
woordenlijstOutputServer <- function(id, specie = reactiveVal(), 
  plot = reactiveVal(), outputs = character(), uiText) {
  
  moduleServer(id, function(input, output, session){
      
      return(specie)
      
    })
  
}
