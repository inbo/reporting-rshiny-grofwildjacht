# TODO: Add comment
# 
# Author: mvarewyck
###############################################################################



#' Section for welcoming (top of the page)
#' @param id character, from which page this function is called
#' e.g. 'wbe'
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
welcomeSection <- function(id, uiText) {
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tags$p(class = "lead", HTML(uiText[, id]))
  
}





#' Section title and text for bio-indicator
#' 
#' @inheritParams welcomeSection
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
bioindicatorSection <- function(id, uiText) {
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    h2(HTML(uiText$title)),
    tags$p(HTML(uiText[, id]))
  )

}
