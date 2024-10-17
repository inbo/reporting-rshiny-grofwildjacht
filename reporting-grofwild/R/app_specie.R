
#' UI for the 'specie' page
#' @inherit shiny::fluidPage return
#' @author lcougnaud
#' @import shiny
#' @export
specieUI <- function(input){
  
  specie <- input$wildsoort
  
  verticalLayout(
      
    title = specie,
      
    # header
    fluidRow(
      column(width = 10, uiOutput(outputId = "path")),
      headerUI()
    ),
    
    # image
    fluidRow(
      img(
        system.file("ui", "www", "specie-header.png", 
          package = "reportingGrofwild"
        )
      )
    ),

    # Choice of specie and options
    sidebarLayout(
        
      position = "left",
        
      sidebarPanel = sidebarPanel(
            
        selectInput(
          inputId = "specieWildsoort", 
          label = "Selecteer een diersoort:",
          choices = schadeWildsoorten,
          selected = specie
        ),
         
        imageOutput(outputId = "wildsoortImage"),
        textOutput(outputId = "wildsoortName")
      
      ),
      
      mainPanel = mainPanel(
        uiOutput(outputId = "specieItems")
      )

   )

  )

}

#' Server function for the 'specie' page
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @return No returned value, output are updated in the page.
#' @author lcougnaud
#' @import shiny
#' @export
specieServer <- function(input, output, session){
  
  # Specie image
  observe({
    if(isTruthy(input$specieWildsoort)){
      imgFile <- system.file("ui", "www", 
        paste0("specie-", 
          gsub("[[:blank:]]", "-", input$specieWildsoort), 
          ".png"), 
        package = "reportingGrofwild"
      )
      if(file.exists(imgFile))
        output$wildsoortImage <- renderImage(
          list(src = imgFile, width = "100%")
        )
    }
  })

  # Specie latin name
  output$wildsoortName <- renderText({
    specieInfo <- read.csv(
      file = system.file("extdata", "specie-info.csv", 
      package = "reportingGrofwild")
    )
    latinName <- subset(specieInfo, `specie name` == input$specieWildsoort)$`latin name`
    if(length(latinName) == 1)    
      paste("Latijn:", latinName)      
  })

  # Specie - available items/pages
  output$speciesItem <- renderUI({
    cards <- c(
      if(input$specieWildsoort %in% c("Wild zwijn", "Ree", "Damhert", "Edelhert"))
        specieCardUI(type = "afschot"),
      if(input$specieWildsoort %in% schadeWildsoorten)
        specieCardUI(type = "schade"),
      if(input$specieWildsoort %in% c("Wild zwijn", "Ree"))
        specieCardUI(type = "populatie indicatoren"),
      if(input$specieWildsoort %in% c("Wild zwijn", "Ree", "Damhert", "Edelhert"))
        c(
          specieCardUI(type = "verspreiding"),
          specieCardUI(type = "maatschappelijk draagvlak")
        ),
        specieCardUI(type = "woordenlijst")
     )
     do.call(layout_column_wrap, cards)
   })

}

#' UI element for a 
#' @param type type (title) of the card
#' @inherit bslib::card return
#' @author lcougnaud
#' @importFrom shiny img
#' @importFrom bslib card card_header card_body
#' @export
specieCardUI <- function(type){
  file <- system.file("ui", "www", 
    paste0("specie-", gsub("[[:blank:]]", "-", type), ".png")
  )
  card(
    card_header(class = "specie-card", toupper(type)),
    card_body(img(file))
  )
}
