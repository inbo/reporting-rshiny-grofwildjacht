#' Get categories available for a specie
#' @param specie string with specie. If not specified,
#' all categories are returned.
#' @return character vector with available categories
#' @author lcougnaud
#' @export
getCategories <- function(specie = NULL){
  
  baseApp <- 
    is.null(specie) ||
    (specie %in% c("Wild zwijn", "Ree", "Damhert", "Edelhert"))
  
  categories <- c(
      if(baseApp)  "beheer",
      "schade",
      if(is.null(specie) || specie %in% c("Wild zwijn", "Ree"))
        "populatie",
      if(baseApp)
        c("verspreiding", "draagvlak"),
      "woordenlijst"
  )
  
  return(categories)
  
}

#' Get title for a 'Category' tab
#' @param category string with category
#' @return string with category title
#' @author lcougnaud
#' @export
getCategoryTitle <- function(category){
  
  title <- switch(category,
    draagvlaak = "Maatschappelijk draagvlak",
    populatie = "Populatie indicatoren",
    tools::toTitleCase(sub("-", " ", category))
  )
  
  return(title)
  
}

#' Get subcategories available in the app
#' @inheritParams getSubcategoryTitle
#' @author lcougnaud
#' @export
getSubcategories <- function(...){
  
  titles <- getSubcategoryTitle(...)
  subcategories <- names(titles)
  
  return(subcategories)
  
}

#' Get title for the 'Subcategory' tab(s)
#' @param category character of length 1 with the name of the
#' category page.
#' @return named character vector with tab titles
#' @author lcougnaud
#' @export
getSubcategoryTitle <- function(
  category = getCategories(), 
  subcategory = NULL){
 
  category <- match.arg(category, several.ok = TRUE)

  titles <- c(
    if("beheer" %in% category)
      c(
        `beheer-vlaanderen` = "Afschot in Vlaanderen",
        `beheer-regio` =  "Afschot per regio",
        `beheer-leeftijdcategorie` = "Afschot per leeftijdscategorie",
        `beheer-jachtmethode` = "Afschot per jachtmethode"
      ),
    if("schade" %in% category)
      c(
        `schade-vlaanderen` = "Schadegevallen in Vlaanderen",
        `schade-regio` =  "Schadegevallen per regio",
        `schade-type` = "Schadegevallen per type schade",
        `schade-seizoen` = "Schadegevallen per seizoen",
        `schade-kosten` = "Inschatting kosten"
      ),
    if("populatie" %in% category)
      c(
        `populatie-leeggewicht` = "Leeggewicht",
        `populatie-onderkaak` = "Onderkaak gegevens",
        `populatie-geslacht` = "Geslacht",
        `populatie-voortplanting` = "Voortplanting"
      ),
    if("verspreiding" %in% category)
      c(
        `verspreiding-huidig` = "Huidige verspreiding",
        `verspreiding-toekomstig` = "Toekomstige verspreiding"
      )
  )
  
  if(!is.null(subcategory))
    titles <- titles[subcategory]
  
  return(titles)
  
}

#' Get output title
#' @param output character vector of length 1 with output name
#' @param uiText data.frame, HTML formatted text to be displayed 
#' in the UI
#' @param specie (optional) character vector of length 1 with specie
#' @param type (optional) character vector of length 1 with type
#' @param n (optional) integer vector of length 1 with maximum 
#' number of characters to include
#' @return character vector of length 1 with plot title
#' @author lcougnaud
getOutputTitle <- function(output, 
  uiText, specie = NULL, type = NULL, n = integer()){

  # check if output name formatted as 'output-[type]'
  outputInfo <- strsplit(output, split = "-")[[1]]
  if(
    !is.null(type) && type == "schade" && 
    length(outputInfo) == 2 && outputInfo[2] != type){
    outputFunction <- outputInfo[1]
    type <- outputInfo[2]
  }else{
    outputFunction <- output
  }
  
  title <- uiText[which(uiText$plotFunction == outputFunction), "title"]
  
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
#' @param maxDate date, the last observation date to be 
#' replaced in the text
#' @param statsMap character, statistics to be printed 
#' instead of \code{'{{statsMap}}'}
#' @inheritParams getOutputTitle
#' @return character vector of length 1 with output description
#' @author lcougnaud
getOutputDescription <- function(output, 
  uiText, context = "summary", 
  specie = NULL, type = NULL, statsMap = NULL,
  maxDate = NULL){
  
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
  
  # Replace last date
  if (!is.null(maxDate))
    text <- gsub("\\{\\{maxDate\\}\\}", 
      format(maxDate, "%d/%m/%Y"), text)
  
  # Handling embedded quoting
  text <- gsub("\\\\", "\"", text)

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

#' Get 'Informatie' tab panel for a 'Category' page
#' @inheritParams welcomeSectionUI
#' @param ... Extra parameters passed to \code{\link{welcomeSectionUI}}
#' @return \code{\link[shiny]{tabPanel}}
#' @importFrom shiny tabPanel fluidRow icon
#' @author lcougnaud
tabPanelInformatie <- function(
  id, uiText, 
  category = getCategories(),
  ...){

  category <- match.arg(category)
  
  tabPanel(
    title = "", #getTabTitle(value = "informatie", category = "schade"), 
    value = "informatie", 
    class = "tab-informatie",
    icon = shiny::icon(name = NULL, class = "info_icon"), #name = "circle-info"
    fluidRow(
      welcomeSectionUI(
        id = id, uiText = uiText,
        category = category,
        context = "description",
        ...
      )
    )
  )
  
}

#' Get 'All plots/tables' tab panel for a 'Category' page
#' @inheritParams welcomeSectionUI 
#' @param plots Character vector with all plots to include.
#' The name should match the name in \code{uiText}
#' @inherit shiny::navbarMenu return
#' @importFrom shiny navbarMenu NS
#' @author lcougnaud
tabPanelAll <- function(category, outputs, uiText, id){
  
  ns <- NS(namespace = id)
  
  args <- lapply(outputs, function(output){

    title <- getOutputTitle(
      output = output, #specie = specie(), 
      uiText = uiText, type = category
     )
     # shorten title
     title <- sub(" (van )*(op )*(voor )*\\{wildsoort\\}(,)*", "", title)
     
     tabPanel(
      title = title, 
      value = output, 
      uiOutput(outputId = ns(paste0("plots-", output)))
    )
        
  })

  args[["title"]] <- getTabTitle(value = "all", category = category)
  args[["menuName"]] <- "all"
  
  do.call(shiny::navbarMenu, args)
  
}