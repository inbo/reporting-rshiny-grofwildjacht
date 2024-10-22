
#' UI for the 'specie' page
#' @param id character, module id/specie
#' @inherit shiny::verticalLayout return
#' @author lcougnaud
#' @import shiny
#' @export
specieUI <- function(id){
  
  tagList(
      
    #tags$head(tags$style("body{overflow-y:hidden;}")),
    
    verticalLayout(
      
      # header
      headerUI(path = c("home", "specie"), id = id),
      
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
#' @return \link[shiny]{reactiveVal} with text of 
#' redirected page: 'home' or one of the 'category' pages
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
    
    ## Output
    
    nextPage <- reactive(
      if(isTruthy(input$pathHome))
        "home"
      else if(isTruthy(input$cards))
        input$cards
    )

    return(nextPage)
 
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
#' @inherit shiny::radioButtons return
#' @author lcougnaud
#' @inheritParams getSpecieImage
getSpecieCards <- function(specie){
  
  baseApp <- (specie %in% c("Wild zwijn", "Ree", "Damhert", "Edelhert"))
  
  values <- c(
      if(baseApp)  "afschot",
      if(specie %in% unlist(schadeWildsoorten))
        "schade",
      if(specie %in% c("Wild zwijn", "Ree"))
        "populatie indicatoren",
      if(baseApp)
        c("verspreiding", "maatschappelijk draagvlak"),
      "woordenlijst"
  )
  
  names <- lapply(values, function(type){
    foto <- paste0("specie-", gsub("[[:blank:]]", "-", type), ".png")
    HTML(paste0(
      "<div class='specie-card-title'>", toupper(type), "</div>",
      "<div>", img(src = paste0("www/", foto), width = "100%"), "</div>"
    ))
  })
  
  div(
      
    radioButtons(
      inputId = NS(specie, "cards"), label = "", inline = TRUE,
      choiceValues = values, choiceNames = names,
      selected = character(0)
     ),
    tags$script("$('.radio-inline').addClass('col-3');"),
    tags$head(tags$style(HTML(".col-3 {width: 32%;} .radio-inline{margin-left:10px;}")) )
  )
}