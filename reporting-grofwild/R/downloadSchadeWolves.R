#' Shiny module for downloading the full wolf schade data - server side
#' @inheritParams reportingGrofwild-common-args
#' @inheritParams optionsModuleServer 
#' @return no return value
#' @author sjunius
#' @import shiny
#' @export
downloadSchadeWolvesServer <- function(id, data, preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
     
      # Download data
      output$downloadData <- downloadHandler(
        filename = function()
          nameFile(species = "Wolf",
            content = "schadeData", fileExt = "csv"),
        content = function(file) {
          
          subData <- data() %>% 
            mutate(gemeente = gemeente_afschot_locatie) %>% 
            select(ID, Datum_vaststelling, gemeente, provincie, Schadegeval,
            Aantal_dood, Aantal_gewond, Aantal_vermist, Diersoort, DNA_resultaat,
            Status_permanentie, Preventie)
          
          ## write data to exported file
          write.table(x = subData, file = file, quote = FALSE, row.names = FALSE,
            sep = ";", dec = ".")
          
        })
      
    })
  
} 



#' Shiny module for downloading the full wolf schade data - UI side
#' @inherit welcomeSectionUI
#' @param plotFunction character, for matching file with plot titles
#' @inheritParams reportingGrofwild-common-args
#' @export
downloadSchadeWolvesUI <- function(id, uiText, plotFunction = "downloadSchadeWolvesUI", 
  context = "description", doHide = TRUE) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = plotFunction, uiText = uiText)
  description <- getOutputDescription(output = plotFunction, 
    uiText = uiText, context = context)
  
  tagList(
    
    actionLink(inputId = ns("linkDownloadSchadeWolves"), label = h3(HTML(title))),
    conditionalPanel(paste("input.linkDownloadSchadeWolves % 2 ==", as.numeric(doHide)), ns = ns,
      
      tags$div(class = "larger-description", HTML(description)),
      tags$br(),
      fixedRow(
        column(12, downloadButton(ns("downloadData"), label = "Download data", class = "downloadButton"))
      ),
      tags$hr()
    )
  )

}