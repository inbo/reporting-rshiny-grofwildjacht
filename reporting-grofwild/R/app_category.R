#' UI function for the cards of the Category page
#' @param id character, module id
#' @param category string, category
#' @return \code{\link[shiny]{verticalLayout}}
#' @author lcougnaud
#' @export
categoryUI <- function(id, category,
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
  
  subcategories <- getSubcategories(category = category)
  cards <- lapply(subcategories, function(subcategory)
    categoryCard(
      id = id, 
      uiText = uiText,
      category = category, subcategory = subcategory
    )
  )
  args <- c(list(width = 1/3, gap = "2em"), cards)
  cards <- do.call(bslib::layout_column_wrap, args)
  
  verticalLayout(
      
    # image
    fluidRow(img(src = img, width = "100%")),
      
    br(),
      
    # Specie
    sidebarLayout(    
      position = "left", 
      sidebarPanel = specieSidebarUI(
        id = ns("sidebar"), 
        speciesList = speciesList
      ),
      mainPanel = mainPanel(
        width = 9, 
        style = "overflow-y: auto;max-height: 100vh;", # scrolling bar
        infoText[["title"]], infoText[["summary"]],
        cards,
        infoText[["description"]]
      )
    )
  
  )
  
}

#' Server function for the Category page
#' @param id character, module id
#' @param category character, category
#' @param specie reactive value with specie
#' @return reactive value with name of subcategory (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
categoryServer <- function(id, 
  specie = reactiveVal(), category = character()){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Main panel
    
    # if a subcategory is selected based on the subcategory cards
    subcategoryUI <- reactiveVal("Subcategorie")
    subcategories <- getSubcategories(category = category)
    lapply(subcategories, function(subcategory){
      observeEvent(input[[paste0(subcategory, "-button")]], {
        subcategoryUI(subcategory)
      })
    })
    
    return(subcategoryUI)
    
  })
  
}

#' Get category card for a specific output or category
#' @param id id character, module id/specie
#' @param output character, output name, e.g. 'trendYearRegionUI'
#' @param category character, category for the card 
#' (e.g.'afschot' or 'schade'), used to extract
#' the picture for the card
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
    btnName <- subcategory; btnLabel <- "Lijst grafieken"
    
  }else if(!missing(output)){
    
    title <- getOutputTitle(
      output = outputFunction, specie = specie, 
      uiText = uiText, type = type
    )
    description <- getOutputDescription(
      output = outputFunction, specie = specie,
      uiText = uiText, type = type)
    filename <- paste0(category, "-", output)
    btnName <- output; btnLabel <- "Bekijk grafiek"
    
  }else stop("'output' or 'subcategory' should be specified.")
  
  file <- system.file("ui", "www", 
    paste0("category-", filename, ".png"), 
    package = "reportingGrofwild"
  )
  
  card <- bslib::card(
    class = "category-card",
    bslib::card_header(title, class = "category-card-header"), 
    br(),
    bslib::card_image(file = file, class = "category-card-image"),
    br(),
    bslib::card_body(description),
    br(), br(),
    bslib::card_footer(
      align = "center",
      shiny::actionButton(
        inputId = ns(paste0(btnName, "-button")), 
        label = btnLabel, 
        class = "category-card-action-button"
      )
    )
  )
  
  return(card)
  
}