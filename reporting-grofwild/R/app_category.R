#' Get title for a specific tab of the 'Category' page.
#' 
#' This is used for the title of the tab and the path.
#' @param value character of length 1 with 
#' \code{\link[shiny]{tabPanel}} value
#' @param category character of length 1 with the name of the
#' category page.
#' @return character of length 1 with 
#' \code{\link[shiny]{tabPanel}} title
#' @author lcougnaud
getTabTitle <- function(value, category){
  
  title <- switch(category,
    afschot = 
      switch(value,
        vlaanderen = "Afschot in Vlaanderen",
        regio =  "Afschot per regio",
        leeftijdcategorie = "Afschot per leeftijdscategorie",
        jachtmethode = "Afschot per jachtmethode"
      ),
    schade = 
      switch(value,
        informatie = "Informatie over schadegevallen",
        vlaanderen = "Schadegevallen in Vlaanderen",
        regio =  "Schadegevallen per regio",
        type = "Schadegevallen per type schade",
        seizoen = "Schadegevallen per seizoen",
        kosten = "Inschatting kosten"
    ),
    populatie = 
      switch(value,
        leeggewicht = "Leeggewicht",
        onderkaak = "Onderkaak gegevens",
        geslacht = "Geslacht",
        voortplanting = "Voortplanting"
      ),
    verspreiding = 
      switch(value,
        huidig = "Huidige verspreiding",
        toekomstig = "Toekomstige verspreiding"
      )
  )
  
  return(title)
  
}

#' Get output title
#' @param output character vector of length 1 with output name
#' @param uiText data.frame with plot title and description
#' @param specie (optional) character vector of length 1 with specie
#' @param type (optional) character vector of length 1 with type
#' @param n (optional) integer vector of length 1 with maximum 
#' number of characters to include
#' @return character vector of length 1 with plot title
#' @author lcougnaud
getOutputTitle <- function(output, 
  uiText, specie = NULL, type = NULL, n = integer()){
  
  title <- uiText[which(uiText$plotFunction == output), "title"]
  
  if(!is.null(specie)){
    title <- gsub("{wildsoort}", tolower(specie), title, fixed = TRUE)
    title <- gsub("{wildsoorten}", 
      switch(specie, 
        "Ree" = "ree\u00EBn",
        "Wild zwijn" = "wilde zwijnen",
        "Damhert" = "damherten",
        "Edelhert" = "edelherten",
        specie
      ), title, fixed = TRUE
    )
  }
  
  if(!is.null(type)){
    if(type == "schade")	type <- "schadegevallen"
    title <- gsub("{type}", type, title, fixed = TRUE)
  }
  
  if(length(n) > 0)
    title <- paste0(substr(x = title, start = 1, stop = n), "...")
  
  return(title)
  
}

#' Get output description
#' @inheritParams getOutputTitle
#' @return character vector of length 1 with output description
#' @author lcougnaud
getOutputDescription <- function(output, 
  uiText, context = "fauna", 
  specie = NULL, type = NULL, statsMap = NULL){
  
  text <- uiText[which(uiText$plotFunction == output), context]
  
  if(length(text) == 0)
    return(NULL)
  
  if(!is.null(specie)){
    text <- gsub("{wildsoort}", tolower(specie), text, fixed = TRUE)
    text <- gsub("{wildsoorten}", 
      switch(specie, 
        "Ree" = "ree\u00EBn",
        "Wild zwijn" = "wilde zwijnen",
        "Damhert" = "damherten",
        "Edelhert" = "edelherten",
        specie
      ), text, fixed = TRUE
    )
  }
  
  if(!is.null(type)){
    if(type == "schade")	type <- "schadegevallen"
    text <- gsub("{type}", type, text, fixed = TRUE)
  }
  
  if (grepl("\\{\\{statsMap\\}\\}", text))
    text <- gsub(
      pattern = "\\{\\{statsMap\\}\\}", 
      replacement = if (!is.null(statsMap)) paste0(statsMap, ".") else "", 
      x = text
    )
  
  return(text)
  
}

#' Get category card for a specific output
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
  uiText, output, outputFunction = output, 
  category, specie, type = category){
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = outputFunction, specie = specie, 
    uiText = uiText, type = type)
  
  description <- getOutputDescription(
    output = outputFunction, specie = specie,
    uiText = uiText, type = type)
  
  file <- system.file("ui", "www", 
    paste0("category-", category, "-", output, ".png"), 
    package = "reportingGrofwild"
  )
  
  outputCard <- bslib::card(
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
              inputId = ns(paste0(output, "-button")), 
              label = "Bekijk grafiek", class = "category-card-action-button"
          )
      )
  )
  
  return(outputCard)
  
}

#' Wrapper for the sidebar layout of the Category pages
#' @inheritParams categorySidebarPanel
#' @param ... Elements for the \code{\link[shiny]{mainPanel}}
#' @inherit shiny::sidebarLayout return
#' @author lcougnaud
categoryPanel <- function(id, specie, ...){
  
  ns <- NS(namespace = id)
  
  sidebarLayout(
    position = "left", 
      
    sidebarPanel = categorySidebarPanel(
      id = id, specie = specie
    ),
      
    mainPanel = mainPanel(width = 9, ...)
  
  )
  
}

#' Wrapper for the sidebar of the Category pages
#' @param id character, module id
#' @param specie character, specie
#' @param topExtra (optional) extra elements to include at 
#' the top of the sidebar
#' @param bottomExtra (optional) extra elements to include at 
#' the bottom of the sidebar
#' @return shiny::sidebarPanel return
#' @author lcougnaud
categorySidebarPanel <- function(id, specie, 
  topExtra = NULL, bottomExtra = NULL){
  
  ns <- NS(namespace = id)
  
  sidebarPanel(
    width = 3, 
    id = ns("category-sidebar"), 
    topExtra,
    h4(specie, align = "center"),
    img(src = getSpecieImage(specie = specie, relative = TRUE), width = "100%", height = "auto"),
    br(),
    div(strong(paste("Latijn:", getLatinName(specie = specie))), align = "center"),
    bottomExtra
  )
  
}