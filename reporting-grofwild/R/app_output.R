#' UI for a category page
#' @inheritParams reportingGrofwild-common-args
#' @param whiteWell boolean whether to draw a white well panel around the output;
#' default is FALSE 
#' @param ... any parameters passed to \code{\link{specieSidebarUI}}
#' /\code{\link{specieSidebarUI}}
#' @inherit shiny::verticalLayout return
#' @author lcougnaud
#' @import shiny
#' @export
outputUI <- function(id, category, whiteWell = FALSE, uiText = NULL, ...){
  
  ns <- NS(namespace = id)
  
  description <- if (!is.null(uiText))
      getOutputDescription(output = id, uiText = uiText,
        context = "description") else
      NULL
  
  imgFile <- file.path("www", paste("category", category, "header.png", sep = "-"))
  
  verticalLayout(
          
    # image
    fluidRow(img(src = imgFile, width = "100%")),
           
    # Specie and options
    tags$div(style = "margin-left: 15px; margin-top: 15px; margin-right: 15px", 
      sidebarLayout(    
        position = "left", 
        sidebarPanel = if(category == "schade"){
            schadeSidebarUI(id = ns("sidebar"), ...)
          }else{
            specieSidebarUI(id = ns("sidebar"), ...)
          },
        mainPanel = mainPanel(
          width = 9, 
          style = "overflow-y: hidden", # avoid 2 scrolling bar
          if (whiteWell)
              wellPanel(class = "well-white", uiOutput(outputId = ns("output"))) else
              uiOutput(outputId = ns("output")),
          tags$div(style = "margin-top: 25px;margin-bottom: 25px;", HTML(description))
        )
      )
    )
  
  )
  
}