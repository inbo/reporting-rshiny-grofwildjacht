
#' Server function for an output (plot/table) of the 'verspreiding' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author mvarewyck
#' @export
draagvlakOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal(),
  outputs = character(),
  draagvlakData, uiText){
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## Sidebar panel
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## Main panel
      
      # Tab content with selected plot/table
      
      outputServer <- reactiveVal(NULL)
      
      draagvlakSubdata <- reactive({
          
          subData <- switch(plot(), 
            "F14_1" = draagvlakData$aanwezigheid,
            "F14_2" = draagvlakData$aantrekkingskracht,
            "F14_3" = draagvlakData$impacts,
            "F14_4" = draagvlakData$maatregelen,
            "F14_5" = draagvlakData$beleid
          )
          subData[subData$Soort == specie(), ]
          
        })
      
      allSectorChoices <- list(
        "Breed publiek" = c("Binnen everzwijngebied", "Buiten everzwijngebied"),
        "Stakeholders" = c("Jachtsector", "Landbouwsector", "Natuursector")
      )
      
      # Create plot - UI side
      observe({
          
          if (plot() %in% outputs){
            
            outputName <- plot()
            
            # create the plot/table
            ui <- if (nrow(draagvlakSubdata()) > 0) {
              pciDraagvlakUI(
                  id = ns("pciDraagvlak"), 
                  uiText = uiText, 
                  outputFunction = outputName,
                  yearChoices = levels(draagvlakSubdata()$Year),
                  sectorChoices = if (outputName %in% c("F14_1", "F14_2")) 
                      allSectorChoices[1] else 
                      allSectorChoices,
                  groupChoices = if (outputName != "F14_1") levels(draagvlakSubdata()$vraag_label),
                  groupLabel = switch(outputName,
                    "F14_3" = "Impacts",
                    "F14_4" = "Maatregelen",
                    "F14_5" = "Belang in beheer",
                    "")
                )
              } else helpText("Geen visualisatie beschikbaar voor deze diersoort")
            
            # include plot/table in UI
            output[["output"]] <- renderUI(ui)
            
            # activate server-side update
            outputServer(outputName)
            
          }
          
        })
      
      # Create plot - server side
      observeEvent(outputServer(), ignoreNULL = TRUE, {
          
          outputName <- outputServer()
          
          pciDraagvlakServer(
            id = "pciDraagvlak",
            data = draagvlakSubdata,
            yVar = if (outputName == "F14_1") "Year" else "vraag_label",
            plotFunction = if (outputName == "F14_2") "barDraagkracht" else "pciDraagvlak"
          )
          
          # re-set in case plot selected via tab after/before category card
          outputServer(NULL)
        })
      
      return(list(
          specie = specie
        ))
      
    })
  
}