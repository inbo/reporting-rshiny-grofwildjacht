#' UI for the specie sidebar panel
#' @param topExtra (optional) extra elements to include at 
#' the top of the sidebar
#' @param bottomExtra (optional) extra elements to include at 
#' the bottom of the sidebar
#' @param name logical (TRUE if select is FALSE and opposite)
#' should the specie name be displayed as text?
#' @param select logical (FALSE by default), should a button
#' be included to select the specie?
#' @inheritParams reportingGrofwild-common-args
#' @return shiny::sidebarPanel return
#' @author lcougnaud
#' @export
specieSidebarUI <- function(id, 
  topExtra = NULL, bottomExtra = NULL,
  category = TRUE, 
  name = !select, select = FALSE,
  speciesList){
  
  ns <- NS(namespace = id)
  
  sidebarPanel(
    width = 3, 
    id = ns("sidebar"), 
    topExtra,
    if(select)
      selectInput(
        inputId = ns("specie"), 
        label = "Selecteer een diersoort:",
        choices = c("", speciesList)
      ),
    if(name)
      htmlOutput(outputId = ns("specie-name")),
    imageOutput(outputId = ns("specie-image"), height = "auto"),
    textOutput(outputId = ns("specie-latin-name")),
    bottomExtra
  )
  
}

#' Server function for the specie sidebar panel
#' @inheritParams reportingGrofwild-common-args
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
specieSidebarServer <- function(id, specie = reactiveVal()){
  
  moduleServer(id, function(input, output, session){
    
    # Specie name
    output$`specie-name` <- renderUI(h4(specie()))
    
    # Specie image	
    output$`specie-image` <- renderImage(
      list(src = getSpecieImage(specie = specie()), width = "100%")
      , deleteFile = FALSE)
    
    # Specie latin name
    output$`specie-latin-name` <- renderText(
      paste("Latijn:", getLatinName(specie = specie()))
    )
        
  })
}