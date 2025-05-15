

#' Server function for the cards of the 'verspreiding' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return named list with plot, reactive with name of output plot/table (if selected)
#' @import shiny
#' @author mvarewyck
#' @export
draagvlakCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText){
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## Sidebar panel
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## Main panel
      
      # Create tab
      observe({
          
          if(subcategory() %in% subcategories){
            
            categoryCards <- lapply(outputs, function(output){
                
                args <- list(
                  output = output,
                  id = id, 
                  uiText = uiText,
                  specie = specie(), 
                  category = "draagvlak"
                )
                                
                do.call(categoryCard, args)
              })
            
            args <- c(categoryCards, list(width = 1/3, gap = "2em"))
            cards <- do.call(bslib::layout_column_wrap, args)
            
            output[["output"]] <- renderUI(cards)
            
          }
        })
      
      # if plot is selected based on the category cards
      outputUI <- reactiveVal()
      lapply(outputs, function(output){
          btn <- paste0(output, "-button")
          observeEvent(
            input[[btn]], 
            outputUI(output), 
            ignoreInit = TRUE
          )
        })
      
      return(list(
          plot = reactive(outputUI())
        ))
      
    })
}


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
                  sectorChoices = if (outputName == "F14_2") allSectorChoices[1] else allSectorChoices,
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