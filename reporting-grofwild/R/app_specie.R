
#' UI for the 'specie' page
#' @param id character, module id/specie
#' @inherit shiny::fluidPage return
#' @author lcougnaud
#' @import shiny
#' @export
specieUI <- function(id){
  
  tagList(
      
    #tags$head(tags$style("body{overflow-y:hidden;}")),
    
    verticalLayout(
      
      # header
      headerUI(
        column(width = 9, 
          fluidRow(
            column(width = 1, actionLink(inputId = NS(id, "pathHome"), label = "Home")),
            column(width = 1, "/"),
            column(width = 2, textOutput(outputId = NS(id, "pathSpecie")))        
          )
        ),
        offset = 0
      ),
      
      # image
      fluidRow(
       column(width = 12, 
          img(src = "www/specie-header.png", width = "100%")
        )
      ),
      
      br(),
  
      # Choice of specie and options
      sidebarLayout(
          
        position = "left", 
          
        sidebarPanel = sidebarPanel(
            
          width = 3,
              
          selectInput(
            inputId = NS(id, "wildsoort"), 
            label = "Selecteer een diersoort:",
            choices = schadeWildsoorten,
            selected = id
          ),
           
          imageOutput(outputId = NS(id, "image"), height = "auto"),
          textOutput(outputId = NS(id, "name"))
        
        ),
        
        mainPanel = mainPanel(
          width = 9,
          # style = "overflow-y: auto;max-height: 100vh;", # scrolling bar?
          uiOutput(outputId = NS(id, "items"))
        )
  
     )
  
    )

  )

}

#' Server function for the 'specie' page
#' @inheritParams specieUI
#' @return Logical, if TRUE (FALSE by default), the 'Home' page
#' is requested.
#' @author lcougnaud
#' @import shiny
#' @importFrom bslib layout_column_wrap
#' @export
specieServer <- function(id){
  
  moduleServer(id, function(input, output, session){
       
    ## Header
    
    # Update specie in path
    output$pathSpecie <- renderText(input$wildsoort)
    
    ## Sidebar panel
 
    # Specie image	
    output$image <- renderImage({
      imgFile <- system.file("ui", "www", paste0("specie-", 
        gsub("[[:blank:]]", "-", tolower(input$wildsoort)), 
        ".png"), package = "reportingGrofwild")
      validate(
        need(
          expr = file.exists(imgFile), 
          message = paste("No image available for:", input$wildsoort)
        )
      )
      list(src = imgFile, width = "100%")
    }, deleteFile = FALSE)

    # Specie name
    output$name <- renderText({
      specieInfo <- read.csv(
        file = system.file("extdata", "specie-info.csv", 
          package = "reportingGrofwild"),
        check.names = FALSE
      )
      latinName <- specieInfo[
        which(specieInfo$`specie name` == input$wildsoort),
        "latin name"
      ]
      
      validate(
        need(
          expr = (length(latinName) == 1 && nchar(latinName) > 0), 
          message = paste("No latin name available for:", input$wildsoort)
        )
      )
      paste("Latijn:", latinName)
    })

    ## Main panel

    # Specie - available items/pages
    output$items <- renderUI({
      baseApp <- (input$wildsoort %in% c("Wild zwijn", "Ree", "Damhert", "Edelhert"))
      cards <- tagList(
        if(baseApp)
          specieCardUI(type = "afschot"),
        if(input$wildsoort %in% unlist(schadeWildsoorten))
          specieCardUI(type = "schade"),
        if(input$wildsoort %in% c("Wild zwijn", "Ree"))
          specieCardUI(type = "populatie indicatoren"),
        if(baseApp)
          specieCardUI(type = "verspreiding"),
        if(baseApp)
          specieCardUI(type = "maatschappelijk draagvlak"),
         specieCardUI(type = "woordenlijst")
       )
       cards <- cards[!sapply(cards, is.null)]
       args <- c(cards, list(width = 1/3, fixed_width = TRUE))
       do.call(bslib::layout_column_wrap, args)
     })
 
     goHome <- reactiveVal(isTruthy(input$pathHome))
     return(goHome)
 
    })

}

#' UI element for a card for a specific specie
#' @param type type (title) of the card
#' @inherit bslib::card return
#' @author lcougnaud
#' @importFrom bslib card card_header card_body
#' @export
specieCardUI <- function(type){
  file <- system.file("ui", "www", 
    paste0("specie-", gsub("[[:blank:]]", "-", type), ".png"),
    package = "reportingGrofwild"
  )
  validate(
    need(
      expr = file.exists(file),
      message = paste("The image for", type, "is not available.")
    )
  )
  bslib::card(
    bslib::card_header(class = "specie-card-header", toupper(type)),
    bslib::card_image(file = file, fill = TRUE)
  )
}
