#' UI function for the cards of the Category page
#' @inheritParams reportingGrofwild-common-args
#' @return \code{\link[shiny]{verticalLayout}}
#' @author lcougnaud
#' @export
categoryUI <- function(
 id, category, 
 ecoData, schadeData,
 uiText, speciesList){
  
  ns <- NS(namespace = id)
  
  dataDate <- if(category == "schade"){
    schadeData
  }else{ecoData}
  maxDate <- max(dataDate$afschot_datum, na.rm = TRUE)
  
  infoText <- welcomeSectionUI(
    id = category, uiText = uiText,
    category = category,
    context = "description",
    maxDate = maxDate,
    split = TRUE
  )
  
  img <- file.path("www", paste("category", category, "header.png", sep = "-"))
  
  verticalLayout(
      
    # image
    fluidRow(img(src = img, width = "100%")),
    
    # Specie
    tags$div(style = "margin-left: 15px; margin-top: 15px", sidebarLayout(    
      position = "left", 
      sidebarPanel = specieSidebarUI(
        id = ns("sidebar"), 
        speciesList = speciesList,
        select = TRUE
      ),
      mainPanel = mainPanel(
        width = 9, 
        style = "overflow-y: hidden;", # single scrolling bar
        
        infoText[["title"]], infoText[["summary"]],
        uiOutput(outputId = ns("cards")),
        tags$div(style = "margin-top: 25px;", infoText[["description"]])
      )
    )
  )
  
  )
  
}

#' Server function for the Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of subcategory (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
categoryServer <- function(id, 
  specie = reactiveVal(), category = character(),
  subcategories = character(),
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Main panel
    
    # get subcategories
        
    cards <- lapply(subcategories, function(subcategory)
      categoryCard(
        id = id, 
        uiText = uiText,
        category = category, subcategory = subcategory
      )
    )
    args <- c(list(width = 1/3, gap = "2em"), cards)
    output$cards <- renderUI(do.call(bslib::layout_column_wrap, args))
    
    # save subcategory if corresponding tile is clicked on
    subcategoryUI <- reactiveVal("Subcategorie")
    lapply(subcategories, function(subcategory){
      observeEvent(
        input[[paste0(subcategory, "-button")]], 
        subcategoryUI(subcategory), 
        ignoreInit = TRUE
      )
    })
    
    return(subcategoryUI)
    
  })
  
}

#' Get category card for a specific output or category
#' @param outputFunction character, named of output function used
#' to extract title, by default: \code{output}
#' @inheritParams reportingGrofwild-common-args
#' @inheritParams getOutputTitle
#' @inherit bslib::card return
#' @author lcougnaud
#' @importFrom bslib card card_header card_image card_body card_footer
#' @importFrom shiny actionButton
categoryCard <- function(id, 
  uiText, 
  output, subcategory,
  outputFunction = output, 
  category, 
  specie = NULL, type = category){
  
  ns <- NS(id)
  
  if(!missing(subcategory)){
    
    title <- getSubcategoryTitle(subcategory, uiText = uiText)
    description <- NULL
    filename <- subcategory
    idCard <- subcategory; btnLabel <- "Lijst grafieken"
    
  }else if(!missing(output)){
    
    title <- getOutputTitle(
      output = outputFunction, specie = specie, 
      uiText = uiText, type = type
    )
    description <- getOutputDescription(
      output = outputFunction, specie = specie,
      uiText = uiText, type = type)
    filename <- paste0(category, "-", output)
    idCard <- output; btnLabel <- "Bekijk grafiek"
    
  }else stop("'output' or 'subcategory' should be specified.")
  
  file <- system.file("ui", "www", 
    paste0("category-", filename, ".png"), 
    package = "reportingGrofwild"
  )
  
  card <- bslib::card(
    id = ns(paste0(idCard, "-card")),
    class = "category-card",
    bslib::card_header(title, class = "category-card-header"), 
    br(),
    bslib::card_image(file = file, 
      class = "category-card-image"
    ),
    br(),
    bslib::card_body(description),
    br(), 
    bslib::card_footer(
      align = "center",
      tags$div(
        style = "margin-bottom: 10px;margin-top: 10px", 
          shiny::actionButton(
          inputId = ns(paste0(idCard, "-button")), 
          label = btnLabel, 
          class = "category-card-action-button"
        )
      )
    )
  )
  
  return(card)
  
}