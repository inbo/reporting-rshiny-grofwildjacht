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
    class = "well-white",
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
      uiOutput(outputId = ns("specie-latin-name")),
      bottomExtra,
      div(style = "text-align: right; margin-top: 10px;", actionButton(inputId=ns("return"), label = "Vorige pagina", style = "background-color: #F5F5F5;", icon = icon("arrow-left")))
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
    output$`specie-name` <- renderUI(tags$p(style = "text-align:center", tags$b(specie())))
    
    # Specie image	
    output$`specie-image` <- renderImage(
      list(src = getSpecieImage(specie = specie()), width = "100%")
      , deleteFile = FALSE)
    
    # Specie latin name
    output$`specie-latin-name` <- renderUI(
      tags$em(paste("Latijn:", getLatinName(specie = specie())))
    )
    
    observe(updateSelectInput(session, inputId = "specie", selected = specie()))
    
    observeEvent(input$specie, { 
        req(input$specie != "")
        req(session$clientData$url_search)
        
        currentString <- session$clientData$url_search
        
        query <- parseQueryString(session$clientData$url_search)
        query[["specie"]] <- input$specie
        speciesInfo <- read.csv(file.path(system.file("extdata", package = "reportingGrofwild"), "species-info.csv"))
        query[["gbifkey"]] <- speciesInfo[match(input$specie, speciesInfo$species.name), "gbifkey"]

        newString <- paste0("?", paste0(names(query), "=", query, collapse = "&"))
        
        if (!identical(parseQueryString(currentString), parseQueryString(newString)))
          updateQueryString(queryString = newString, mode = "push", session)
          
      }, priority = -2)
    
    
    observeEvent(input$return, {
        
        req(session$clientData$url_search)
        
        currentString <- parseQueryString(session$clientData$url_search)
        
        newSelection <- currentString
        if (!is.null(currentString$plot)) {
          newSelection$plot <- NULL
        } else if (!is.null(currentString$subcategory)) {
          newSelection$subcategory <- NULL
        } else if (!is.null(currentString$category)) {
          newSelection$category <- NULL
        } else if (!is.null(currentString$specie)) {
          newSelection$specie <- NULL
          newSelection$gbifkey <- NULL
        } else {
          newSelection <- list()
        }
        
        newString <- paste0("?", paste0(names(newSelection), "=", newSelection, collapse = "&"))
        
        if (!identical(currentString, newSelection))
          updateQueryString(queryString = newString, mode = "push", session)
      })
    
        
  })
}