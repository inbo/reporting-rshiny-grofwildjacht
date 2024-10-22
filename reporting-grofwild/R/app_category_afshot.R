#' UI for the 'afshot' Category page
#' @param id id character, module id/specie
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
afshotUI <- function(id){
      
  verticalLayout(
          
    # header
    headerUI(path = c("home", "specie", "category", "plot"), id = id),
            
    # image
     fluidRow(
      column(width = 12, 
        img(src = "www/category-afshot-header.png", width = "100%")
      )
    ),
    br(),
           
    # navigation page with plots and specie sidebar panel
    navbarPage(
        
      title = "afshot",
      
      id = NS("afshot", id),
      
      tabPanel(
        title = "Afshot in Vlaanderen", value = "vlaanderen",
        uiOutput(outputId = NS(c("afshot", "vlaanderen"), id = id))
      ),
      tabPanel(title = "Afschot per regio", value = "regio"),
      tabPanel(title = "Afschot per leeftijdscategorie", value = "leeftijdcategorie"),
      tabPanel(title = "Afschot per jachtmethode", value = "jachtmethode")
    )

  )
  
}

#' Server function for the 'afshot' Category page
#' @param id id character, module id/specie
#' @return Shiny module function
#' @import shiny
#' @importFrom bslib card card_header card_image card_body card_footer
#' @author lcougnaud
afshotServer <- function(id){
  
  moduleServer(id, function(input, output, session){   

    observeEvent(input$`category-afshot`, {
          
      switch(input$category-afshot,
          
        vlaanderen = {
          output$`afshot-vlaanderen` <- afshotPanel(
            id = id,
            bslib::layout_column_wrap(
              width = 1/3,
              bslib::card(
                bslib::card_header("Jaarlijks gerapporteerd afschot van wild zwijn in Vlaanderen"), 
                bslib::card_image(),
                bslib::card_body("Een lijngrafiek van het jaarlijks afschot van wild zwijn  in Vlaanderen."),
                bslib::card_footer(
                  shiny::actionButton(inputId = NS(c("afshot", "plot", "vlaanderen"), id = id))
                )
              ),
              bslib::card(bslib::card_header("Jaarlijks gerapporteerd afschot van wild zwijn per provincie")), 
              bslib::card(bslib::card_header("Percentage afschot van wild zwijn in Vlaanderen t.o.v. een referentieperiode"))
          )
        )
      })

    })

  })
  
}

#' Wrapper for the sidebar of the 'afshot' Category page
#' @param id id character, module id/specie
#' @param ... Elements for the \code{\link[shiny]{mainPanel}}
#' @inherit shiny::sidebarLayout return
#' @author lcougnaud
afshotPanel <- function(id, ...){
  
  sidebarLayout(
    position = "left", 
      
    sidebarPanel = sidebarPanel(
      width = 3,
      imageOutput(outputId = NS(c("afshot", "image"), id = id), height = "auto"),
      textOutput(outputId = NS(c("afshot", "name"), id = id))
    ),
      
    mainPanel = mainPanel(width = 9, ...)

  )
  
}