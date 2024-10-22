#' UI for the 'afschot' Category page
#' @param id id character, module id/specie
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @importFrom bslib card card_header card_image card_body card_footer
#' @export
afschotUI <- function(id){
  
  ns <- NS(namespace = id)
      
  verticalLayout(
          
    # header
    headerUI(
      path = c("home", "specie", "category", "plot"), 
      id = id, specie = id, category = "Afschot"
    ),
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-afschot-header.png", width = "100%")
      )
    ),
    br(),
           
    # navigation page with plots and specie sidebar panel
    navbarPage(
        
      title = "",
      
      id = ns("afschot-plots"),
      
      tabPanel(
        title = "Afschot in Vlaanderen", value = "vlaanderen",
        afschotPanel(
            id = id,
            bslib::layout_column_wrap(
                width = 1/3,
                bslib::card(
                    bslib::card_header("Jaarlijks gerapporteerd afschot van wild zwijn in Vlaanderen"), 
#                    bslib::card_image(),
                    bslib::card_body("Een lijngrafiek van het jaarlijks afschot van wild zwijn  in Vlaanderen."),
                    bslib::card_footer(
                        shiny::actionButton(inputId = ns("afschot-plot-vlaanderen"), label = "Bekijk grafiek")
                    )
                ),
                bslib::card(bslib::card_header("Jaarlijks gerapporteerd afschot van wild zwijn per provincie")), 
                bslib::card(bslib::card_header("Percentage afschot van wild zwijn in Vlaanderen t.o.v. een referentieperiode"))
            )
        )
      ),
      tabPanel(title = "Afschot per regio", value = "regio"),
      tabPanel(title = "Afschot per leeftijdscategorie", value = "leeftijdcategorie"),
      tabPanel(title = "Afschot per jachtmethode", value = "jachtmethode")
    )

  )
  
}

#' Server function for the 'afschot' Category page
#' @param id id character, module id/specie
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
afschotServer <- function(id){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- NS(namespace = id)
        
    ## Header
        
    # Update specie in path
    output$pathSpecie <- renderText(id)
    
    # Update plot in path
    output$pathPlot <- renderText(input$afschot)

  })
  
}

#' Wrapper for the sidebar of the 'afschot' Category page
#' @param id id character, module id/specie
#' @param ... Elements for the \code{\link[shiny]{mainPanel}}
#' @inherit shiny::sidebarLayout return
#' @author lcougnaud
afschotPanel <- function(id, ...){
  
  ns <- NS(namespace = id)
  
  sidebarLayout(
    position = "left", 
      
    sidebarPanel = sidebarPanel(
      width = 3,
      img(src = getSpecieImage(specie = id, relative = TRUE), width = "100%", height = "auto"),
      paste("Latijn:", getLatinName(specie = id))
    ),
      
    mainPanel = mainPanel(width = 9, ...)

  )
  
}