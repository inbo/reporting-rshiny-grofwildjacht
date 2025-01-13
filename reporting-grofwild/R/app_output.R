#' UI for a category page
#' @param id character, module id
#' @param category character, categroy
#' @param ... any parameters passed to \code{\link{specieSidebarUI}}
#' /\code{\link{specieSidebarUI}}
#' @inherit shiny::verticalLayout return
#' @author lcougnaud
#' @import shiny
#' @export
outputUI <- function(id, category, ...){
  
  ns <- NS(namespace = id)
  
  verticalLayout(
          
    # image
    fluidRow(
      column(width = 12, 
       img(src = paste0("www/category-", category, "-header.png"), 
       width = "100%")
      )
    ),
          
    br(),
          
    # Specie and options
    sidebarLayout(    
      position = "left", 
      sidebarPanel = if(category == "schade"){
        schadeSidebarUI(id = ns("sidebar"), ...)
      }else{
        specieSidebarUI(id = ns("sidebar"), ...)
      },
      mainPanel = mainPanel(width = 9, 
       uiOutput(outputId = ns("output"))
      )
    )
  
  )
  
}