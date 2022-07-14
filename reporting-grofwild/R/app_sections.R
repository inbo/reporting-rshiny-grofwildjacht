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



#' Section title and text for bio-indicator - server side
#' @inheritParams bioindicatorSection
#' @param wildsoort reactive, selected wildsoort in the app
#' @return no return value
#' 
#' @author mvarewyck
#' @import shiny
#' @export
bioindicatorSectionServer <- function(id, uiText, wildsoort) {
  
  moduleServer(id,
    function(input, output, session) {
      
      oldText <- uiText[uiText$plotFunction == "bioindicatorSection", id]
      
      output$textBioindicator <- renderUI({
          
          newText <- strsplit(oldText, split = "\\{")[[1]]
          toRetain <- sapply(newText, function(x)
              if (grepl("\\}", x)) {
                if (grepl(wildsoort(), strsplit(x, "\\}")[[1]][1]))
                  strsplit(x, "\\}")[[1]][2] else
                  ""
              } else {
                x
              } 
            )
           
          HTML(trimws(paste(toRetain, collapse = "")))
          
        })
      
    })
}
  

#' Section title and text for bio-indicator
#' 
#' @template moduleUI 
#' 
#' @author mvarewyck
#' @export
bioindicatorSection <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == as.character(match.call())[1], ]
  
  tagList(
    h2(HTML(uiText$title)),
    tags$p(uiOutput(ns("textBioindicator")))
  )

}
