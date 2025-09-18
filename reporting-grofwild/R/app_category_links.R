
#' Server function for an output (internal link) of the 'links' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author sjunius
#' @export
linksOutputServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(), plot = reactiveVal(),
  subcategories = character(),
  outputs = character(), defaultTabs = NULL, uiText){
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## Sidebar panel
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## General selection - Links
      observe({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = ns("links_topbar")
              ),
              switch(as.character(subcategory()), 
                "links-internelinks" =list(
                    hideGeneralFilters = TRUE
                 )
              )
            )
            
            # include plot/table in UI
            output[["topbar_filtering"]] <- renderUI(do.call(generalSelectionUI, args))
            
          }
          
        })
      
      linksSelection <- reactive({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = "links_topbar"
              ),
              switch(as.character(subcategory()), 
                "links-internelinks" = list(),
              )
            )
            
            do.call(generalSelectionServer, args)
            
          }
          
        })
      
      
      ## Main panel
      
      # Tab content with selected plot/table
      outputServer <- reactiveVal(NULL)
      
      # Create plot - UI side
      observe({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {
            
            ui <- switch(as.character(subcategory()), 
              "links-internelinks" = {
                
                links <- lapply(outputs, function(output) {
                    
                      wellPanel(class = "well-white", externalLinksUI(
                          id = ns(paste0("links_", output)), 
                          uiText = uiText,
                          portal = output,
                          doHide = !(plot() == defaultTabs$plot || output %in% plot())
                        ))
                    
                  })
                do.call(tagList, links)
              }
            
            )
            
            # include plot/table in UI
            output[["output"]] <- renderUI(ui)
            
            # activate server-side update
            outputServer(subcategory())
            
          }
          
        })
      
      # Create plot - server side
      observeEvent(outputServer(), ignoreNULL = TRUE, {
          
          switch(as.character(outputServer()), 
            "links-internelinks" = {
              sapply(outputs, function(output) {
                  
                  externalLinksServer(
                    id = paste0("links_", output),
                    specie = specie,
                    portal = output,
                    uiText = uiText
                  )
                  
                }
              )
            }
          )
          
          # re-set in case plot selected via tab after/before category card
          outputServer(NULL)
        })
      
      return(list(
          specie = specie
        ))
      
    })
  
}