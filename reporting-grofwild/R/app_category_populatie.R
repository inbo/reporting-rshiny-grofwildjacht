#' Server function for the cards of the 'populatie indicatoren' Category page
#' @inheritParams categoryCard
#' @inheritParams reportingGrofwild-common-args
#' @return named list with plot, reactive with name of output plot/table (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
populatieCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories = character(), outputs = character(),
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    # Create tab
    observe({    
              
      if(subcategory() %in% subcategories){
            
        categoryCards <- lapply(outputs, function(output)
          categoryCard(
            output = output,
            id = id, 
            uiText = uiText,
            specie = specie(), 
            category = "populatie"
          )
        )
        
        args <- c(categoryCards, list(width = 1/3, gap = "2em"))
        cards <- do.call(bslib::layout_column_wrap, args)
        output[["output"]] <- renderUI(cards)
        
      }
    })

    # if plot is selected based on the category cards
    outputUI <- reactiveVal()
    lapply(outputs, function(output){
      observeEvent(
        input[[paste0(output, "-button")]], 
        outputUI(output), 
        ignoreInit = TRUE
      )
    })
    
  return(list(
      plot = reactive(outputUI())
    ))

  })  
}

#' Server function for an output (plot/table) of the 'populatie' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export
populatieOutputServer <- function(id, 
  specie = reactiveVal(), plot = reactiveVal(),
  outputs = character(),
  ecoData, geoData,
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
        
    ## input
    results <- reactiveValues()
    
    # Create data upon user choices
    results$ecoData <- reactive(
      ecoData[which(ecoData$wildsoort == specie()), ]
    )
    
    results$geoData <- reactive({
      req(geoData)
      geoData[which(geoData$wildsoort == specie()), ]
    })
    
    # Enrich data with FBZ
    results$combinedData <- reactive(
      merge(
        x = results$ecoData(), 
        y = results$geoData()[, c("ID", "FaunabeheerZone")], 
        by = "ID"
      )
    )

    results$timeRange <- reactive(
      range(results$ecoData()$afschotjaar)
    ) 
    
    # Plot 6: Leeggewicht per leeftijdscategorie (INBO of Meldingsformulier) en geslacht
    results$leeftijdtypes <- reactive(
      c(
        loadMetaEco(species = specie())$leeftijd_comp_inbo, 
        "Onbekend"
      )
    )
    
    # Plot 10: Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar
    results$typesFemale <- reactive({
      getFemaleTypes(
        ecoData = results$ecoData(), 
        specie = specie()
      )
    })
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = specie)
    
    ## Main panel

    # Tab content with selected plot/table

    outputServer <- reactiveVal(NULL)
  
    # Create plot - UI side
    observe({
          
      if(plot() %in% outputs){   
        
        outputName <- plot()
        
        # create the plot/table
        ui <- switch(outputName,  
          "boxAgeWeightUI" = {
            boxAgeWeightUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countAgeCheekUI" = {
            countAgeCheekUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countAgeGenderUI" = {
            countAgeGenderUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countEmbryosUI" = {
            countEmbryosUI(
              id = ns(outputName), 
              regionLevels = c(1:2, 4),
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countAgeGroupUI" = {# dash plot F16_1
            countAgeGroupUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          }
        )
        
        # include plot/table in UI
        output[["output"]] <- renderUI(ui)
        
        # activate server-side update
        outputServer(outputName)
      
      }

    })

    # Create plot - server side
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      outputName <- outputServer()
      
      switch(outputName,
        "boxAgeWeightUI" = boxAgeWeightServer(
          id = outputName,
          data = results$combinedData,
          type = results$leeftijdtypes,
          timeRange = reactive(if (specie() == "Ree")
            c(2014, max(results$timeRange())) else 
              results$timeRange())
        ),
        "countAgeCheekUI" = countAgeCheekServer(
          id = outputName,
          data = results$ecoData,
          timeRange = reactive(if (specie() == "Ree")
            c(2005, max(results$timeRange())) else 
            results$timeRange())
        ),
        "countAgeGenderUI" = countAgeGenderServer(
          id = outputName,
          data = results$ecoData,
          timeRange = results$timeRange
        ),
        "countEmbryosUI" = countEmbryosServer(
          id = outputName,
          data = results$combinedData,
          timeRange = results$timeRange,
          types = results$typesFemale,
          uiText = uiText
        ),
        "countAgeGroupUI" = countAgeGroupServer(
          id = outputName,
          data = reactive({
            plotData <- results$ecoData()[
              results$ecoData()$geslacht_comp == "Vrouwelijk", 
            ]
            validate(need(nrow(plotData) > 0, "Geen data beschikbaar"))
            plotData$reproductiestatus <- ifelse(
              is.na(plotData$aantal_embryos), "Onbekend",
              ifelse(plotData$aantal_embryos != 0, "Drachtig", "Niet drachtig")
            )
            plotData
          }),
          timeRange = results$timeRange,
          groupVariable = "reproductiestatus"
        )
      )
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
      
    })
    
    return(list(
        specie = specie
      ))
    
  })
  
}