
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
#' @export
specieServer <- function(id){
  
  moduleServer(id, function(input, output, session){   
        
    # initialization
    specie <- reactiveVal(value = id)
    
    # update value
    observeEvent(input$wildsoort, 
      if(!is.null(input$wildsoort))  specie(input$wildsoort)
    )
        
    ## Header
    
    # Update specie in path
    output$pathSpecie <- renderText(specie())
    
    ## Sidebar panel
 
    # Specie image	
    output$image <- renderImage(
      list(src = getSpecieImage(specie = specie()), width = "100%")
    , deleteFile = FALSE)

    # Specie latin name
    output$name <- renderText(
      paste("Latijn:", getLatinName(specie = specie()))
    )

    ## Main panel

    # Specie - available items/pages
    output$items <- renderUI(getSpecieCards(specie = specie()))
 
    goHome <- reactiveVal(isTruthy(input$pathHome))
    return(goHome)
 
  })

}

#' Get specie latin name
#' @inheritParams getSpecieImage
#' @return Character of length 1, latin name.
#' @author lcougnaud
getLatinName <- function(specie){
  
  specieInfo <- read.csv(
    file = system.file("extdata", "specie-info.csv", 
    package = "reportingGrofwild"),
    check.names = FALSE
  )	
  latinName <- subset(specieInfo, `specie name` == specie)[, "latin name"]
  
  validate(
    need(
      expr = (length(latinName) == 1 && nchar(latinName) > 0), 
      message = paste("No latin name available for:", specie)
    )
  )
  
  return(latinName)
  
}

#' Get specie image
#' @param specie Character of length 1, specie.
#' @return Character of length 1, path to image.
#' @author lcougnaud
getSpecieImage <- function(specie){
  
  imgFile <- system.file("ui", "www", paste0("specie-", 
    gsub("[[:blank:]]", "-", tolower(specie)), 
    ".png"), package = "reportingGrofwild")

  validate(
    need(
      expr = file.exists(imgFile), 
      message = paste("No image available for:", specie)
    )
  )

  return(imgFile)
  
}

#' Get all cards UI element for a specific specie
#' @inherit bslib::layout_column_wrap return
#' @author lcougnaud
#' @importFrom bslib layout_column_wrap
#' @inheritParams getSpecieImage
getSpecieCards <- function(specie){
  
  baseApp <- (specie %in% c("Wild zwijn", "Ree", "Damhert", "Edelhert"))
  cards <- tagList(
      if(baseApp)
        specieCardUI(type = "afschot"),
      if(specie %in% unlist(schadeWildsoorten))
        specieCardUI(type = "schade"),
      if(specie %in% c("Wild zwijn", "Ree"))
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
  
}

#' Get a UI card element
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
