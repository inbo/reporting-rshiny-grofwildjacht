
#' Server function for an output (plot/table) of the 'verspreiding' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author mvarewyck
#' @export
draagvlakOutputServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(), plot = reactiveVal(),
  subcategories = character(), 
  outputs = character(), defaultTabs = NULL,
  draagvlakData, uiText){
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## Sidebar panel
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## Main panel
      
      # Tab content with selected plot/table
      
      outputServer <- reactiveVal(NULL)

      
      allSectorChoices <- list(
        "Breed publiek" = c("Binnen everzwijngebied", "Buiten everzwijngebied"),
        "Stakeholders" = c("Jachtsector", "Landbouwsector", "Natuursector")
      )
      
      # Create plot - UI side
      observe({
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories){
            
            ui <- switch(subcategory(), 
              "draagvlak-surveys" = {
                
                surveys <- lapply(outputs, function(output) {
                    
                    subData <- switch(output, 
                      "F14_1" = draagvlakData$aanwezigheid,
                      "F14_2" = draagvlakData$aantrekkingskracht,
                      "F14_3" = draagvlakData$impacts,
                      "F14_4" = draagvlakData$maatregelen,
                      "F14_5" = draagvlakData$beleid
                    )
                    subData[subData$Soort == specie(), ]
                    
                    if (nrow(subData) > 0)
                    wellPanel(class = "well-white", pciDraagvlakUI(
                        id = ns(paste0("pciDraagvlak_", output)), 
                        uiText = uiText, 
                        outputFunction = output,
                        doHide = !(plot() == defaultTabs$plot || output %in% plot()),
                        yearChoices = levels(subData$Year),
                        sectorChoices = if (output %in% c("F14_1", "F14_2")) 
                            allSectorChoices[1] else 
                            allSectorChoices,
                        groupChoices = if (output != "F14_1") levels(subData$vraag_label),
                        groupLabel = switch(output,
                          "F14_3" = "Impacts",
                          "F14_4" = "Maatregelen",
                          "F14_5" = "Belang in beheer",
                          "")
                      )) else helpText("Geen visualisatie beschikbaar voor deze diersoort")
                    
                  }
                )
                
                do.call(tagList, surveys)
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
          
          switch(outputServer(), 
            "draagvlak-surveys" = {
              sapply(outputs, function(output) {
                  
                  subData <- switch(output, 
                    "F14_1" = draagvlakData$aanwezigheid,
                    "F14_2" = draagvlakData$aantrekkingskracht,
                    "F14_3" = draagvlakData$impacts,
                    "F14_4" = draagvlakData$maatregelen,
                    "F14_5" = draagvlakData$beleid
                  )
                  subData[subData$Soort == specie(), ]
                  
                  pciDraagvlakServer(
                    id = paste0("pciDraagvlak_", output),
                    data = reactive(subData),
                    yVar = if (output == "F14_1") "Year" else "vraag_label",
                    plotFunction = if (output == "F14_2") "barDraagkracht" else "pciDraagvlak"
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