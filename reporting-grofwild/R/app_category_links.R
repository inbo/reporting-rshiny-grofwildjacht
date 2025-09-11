
#' Server function for an output (internal link) of the 'links' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author sjunius
#' @export
linksOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal(),
  outputs = character(), uiText){
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## Sidebar panel
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## Main panel
      
      # Tab content with selected plot/table
      outputServer <- reactiveVal(NULL)
      
      # Create plot - UI side
      observe({
          
          if (plot() %in% outputs) {
            
            outputName <- plot()
         
            # include plot/table in UI
            output[["output"]] <- renderUI(externalLinksUI(
                id = ns("links"), 
                uiText = uiText,
                portal = outputName,
                doHide = FALSE
              ))
            
            # activate server-side update
            outputServer(outputName)
            
          }
          
        })
      
      # Create plot - server side
      observeEvent(outputServer(), ignoreNULL = TRUE, {
          
          outputName <- outputServer()
          
          externalLinksServer(
            id = "links",
            specie = specie,
            portal = outputName,
            uiText = uiText
          )
          
          # re-set in case plot selected via tab after/before category card
          outputServer(NULL)
        })
      
      return(list(
          specie = specie
        ))
      
    })
  
}