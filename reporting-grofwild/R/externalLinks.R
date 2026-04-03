# Summary table for species
# 
# Author: sjunius
###############################################################################


#' Shiny module for creating the plot \code{\link{countAgeGender}} - server side
#' @inheritParams mapFlandersServer
#' @param portal character portal to visualize
#' @inheritParams reportingGrofwild-common-args
#' @return no return value
#' @author mvarewyck
#' @import shiny
#' @export
externalLinksServer <- function(id, specie, portal = c("biodiversiteitsportaal", 
                                                       "exotenportaal", 
                                                       "wolf_info", 
                                                       "roofdiernieuws_link"), uiText) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$link <- renderUI({
          
          title <- getOutputTitle(output = portal, uiText = uiText)
          
          speciesInfo <- read.csv(file.path(system.file("extdata", package = "reportingGrofwild"), "species-info.csv"))
          gbifkey <- speciesInfo[match(specie(), speciesInfo$species.name), "gbifkey"]
          url <- switch(portal,
            "biodiversiteitsportaal" = paste0("https://natuurdata.inbo.be/bie-hub/species/", gbifkey),
            "exotenportaal" = paste0("https://alienspecies.inbo.be/app/01_exotenportaal/?tab=species_observations&language=nl&page=species_information&taxonkey=", gbifkey),
            "wolf_info" = "https://natuurenbos.be/dossiers/de-wolf-vlaanderen",
            "roofdiernieuws_link" = "https://www.vlaanderen.be/inbo/publicaties?search-term=&themas=undefined&organisation-unit=-1&authors=&authorName=Selecteer+een+auteur&from=-1&until=-1&pageNumber=1&sorting=undefined&language-filter=-1&theme-filter=-1&type=17542635&searchPdf=true"
          )
          
          tagList(tags$div(class = "larger-description", tags$em("URL link:", a(title, href=url, target="_blank"))))
        
        })
    })
  
}


#' Shiny module for creating the plot \code{\link{countAgeGender}} - UI side
#' 
#' @param portal character portal to visualize
#' @inherit mapFlandersUI
#' @inheritParams reportingGrofwild-common-args
#' 
#' @export
externalLinksUI <- function(id, uiText, portal, doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = portal, uiText = uiText)
  description <- getOutputDescription(output = portal, uiText = uiText, context = "description")
  
  
  tagList(
    
    actionLink(inputId = ns("linkPortal"),
      label = title, class = "action-h3"),
    conditionalPanel(
      condition = paste("input.linkPortal % 2 ==", as.numeric(doHide)),
      ns = ns,
      
      tags$div(class = "larger-description", HTML(description)),
      uiOutput(ns("link"))
    
    )
  )
}

