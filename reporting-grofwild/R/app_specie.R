#' UI for the 'specie' page
#' @inheritParams reportingGrofwild-common-args
#' @inherit shiny::verticalLayout return
#' @author lcougnaud
#' @import shiny
#' @export
specieUI <- function(id, speciesList){
  
  ns <- NS(namespace = id)
  
  tagList(
    
    verticalLayout(
      
      # image
      fluidRow(
       column(width = 12, 
          img(src = "www/specie-header.png", width = "100%")
        )
      ),
      
      # Choice of specie and options
      tags$div(style = "margin-left: 15px; margin-top: 15px", sidebarLayout(    
          
        position = "left", 
          
        sidebarPanel = specieSidebarUI(
          id = ns("sidebar"), 
          category = FALSE,
          speciesList = speciesList
        ),
        
        mainPanel = mainPanel(
          width = 9,
          style = "overflow-y: auto;max-height: 100vh;", # scrolling bar
          uiOutput(outputId = ns("items"))
        )
  
     )
   )
  
    )

  )

}

#' Server function for the 'specie' page
#' @inheritParams reportingGrofwild-common-args
#' @return no returned value
#' @author lcougnaud
#' @import shiny
#' @export
specieServer <- function(id, specie = reactiveVal()){
  
  moduleServer(id, function(input, output, session){  
        
    category <- reactiveVal("Categorie")
    
    ## Sidebar panel
    specieSidebarServer(id = "sidebar", specie = specie)
      
    ## Main panel
    
    # Specie - available items/pages
    output$items <- renderUI(getSpecieCards(id = id, specie = specie()))
    
    observeEvent(input$cards, category(input$cards))
    
    observe(print(paste("Specie tab: category updated to:", category())))
    
    return(category)

  })

}

#' Get specie latin name
#' @inheritParams getSpecieImage
#' @return Character of length 1, latin name.
#' @author lcougnaud
getLatinName <- function(specie){
  
  specieInfo <- read.csv(
    file = system.file("extdata", "species-info.csv", 
    package = "reportingGrofwild"),
    check.names = FALSE
  )	
  latinName <- specieInfo[
    which(specieInfo$`species name` == specie), 
    "latin name"
  ]
  
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
#' @param relative Logical, if TRUE (FALSE by default) a path
#' relative to the app is returned.
#' @return Character of length 1, path to image.
#' @author lcougnaud
getSpecieImage <- function(specie, relative = FALSE){
  
  imgFile <- system.file("ui", "www", paste0("specie-", 
    gsub("[[:blank:]]", "-", tolower(specie)), 
    ".png"), package = "reportingGrofwild")

  validate(
    need(
      expr = file.exists(imgFile), 
      message = paste("No image available for:", specie)
    )
  )

  if(relative)
    imgFile <- file.path(basename(dirname(imgFile)), basename(imgFile))
 
  return(imgFile)
  
}

#' Get all cards UI element for a specific specie
#' @inheritParams reportingGrofwild-common-args
#' @inherit shiny::radioButtons return
#' @author lcougnaud
#' @inheritParams getSpecieImage
getSpecieCards <- function(id, specie){
  
  values <- getCategories(specie = specie)
  
  names <- lapply(values, function(type){
    foto <- paste0("specie-", gsub("[[:blank:]]", "-", type), ".png")
    title <- ifelse(type == "afschot", "beheer", type)
    title <- toupper(title)
    HTML(paste0(
      "<div class='radio-tiles-title'>", title, "</div>",
      "<div>", img(src = paste0("www/", foto), width = "100%"), "</div>"
    ))
  })
  
  tags$div(style = "margin-top: -20px;",
    radioButtons(
      inputId = NS(id, "cards"), label = "", inline = TRUE,
      choiceValues = values, choiceNames = names,
      selected = character(0)
     ),
    tags$script("$('.radio-inline').addClass('radio-tiles');")#,
#    tags$head(tags$style(HTML(".radio-inline{margin-left:10px;}")) )
  )
}