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
draagvlakPlaceholderUI <- function(id) {
  
  
}

#' Empty function - placeholder
#' @inheritParams beheerCardServer
#' @return no return value
#' 
#' @author mvarewyck
#' @export
draagvlakCardServer <- function(id, specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText) {
  
  moduleServer(id, function(input, output, session){
      
    })
  
}

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
#' @return no return value
#' 
#' @author mvarewyck
#' @export
woordenlijstCardServer <- function(id, specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText) {
  
  moduleServer(id, function(input, output, session){
      
    })
  
}  
