#' Server function for the cards of the 'populatie indicatoren' Category page
#' @inheritParams categoryCard
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of output plot/table (if selected)
#' @import shiny
#' @author lcougnaud
#' @export
populatieCardServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(),
  subcategories, uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## input
    results <- reactiveValues(renderedTabs = "Populatie")
    results$specie <- reactive(specie())
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = results$specie)
    
    # Create tab
    observe({    
              
      if(subcategory() %in% subcategories){
            
        categoryCardPopulatie <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "populatie", 
            ...
          )
        }
            
        group <- strsplit(subcategory(), split = "-")[[1]][2]
              
        cards <- switch(group, 
          leeggewicht =        
            bslib::layout_column_wrap(
              width = 1/3, gap = "2em",
              categoryCardPopulatie(output = "boxAgeWeightUI")
            ),
           onderkaak =          
             bslib::layout_column_wrap(
               width = 1/3, gap = "2em",
               categoryCardPopulatie(output = "countAgeCheekUI")
            ),
            geslacht =        
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countAgeGenderUI")
              ),
            voortplanting =
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countEmbryosUI"),
                categoryCardPopulatie(output = "countAgeGroupUI")
              )
          )
          output[["output"]] <- renderUI(cards)
        }
    })

    # if plot is selected based on the category cards
    outputUI <- reactiveVal("Visualisatie/Tabel")
    observeEvent(input$`boxAgeWeightUI-button`, outputUI("boxAgeWeightUI"), ignoreInit = TRUE)
    observeEvent(input$`countAgeCheekUI-button`, outputUI("countAgeCheekUI"), ignoreInit = TRUE)
    observeEvent(input$`countAgeGenderUI-button`, outputUI("countAgeGenderUI"), ignoreInit = TRUE)
    observeEvent(input$`countEmbryosUI-button`, outputUI("countEmbryosUI"), ignoreInit = TRUE)
    observeEvent(input$`countAgeGroupUI-button`, outputUI("countAgeGroupUI"), ignoreInit = TRUE)
    
    return(outputUI)

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
  ecoData, geoData,
  uiText){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    populatieOutputs <- getOutputs(category = "populatie")
        
    ## input
    results <- reactiveValues(renderedTabs = "Grofwild")
        
    results$specie <- reactive(specie())
  
    # Create data upon user choices
    results$ecoData <- reactive(
      ecoData[which(ecoData$wildsoort == results$specie()), ]
    )
    
    results$geoData <- reactive({
      req(geoData)
      geoData[which(geoData$wildsoort == results$specie()), ]
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
        loadMetaEco(species = results$specie())$leeftijd_comp_inbo, 
        "Onbekend"
      )
    )
    
    # Plot 10: Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar
    results$typesFemale <- reactive({
          
      types <- levels(droplevels(results$ecoData()$type_comp))
          
      types <- if (results$specie() == "Ree") {
        types[types %in% c("Reegeit", "Smalree")] 
      } else if (results$specie() == "Wild zwijn"){
        types[types %in% c("Zeug", "Overloper (v)", "Frisling (v)")]      
      } else {
        types[types %in% c("Kalf (v)", "Smaldier", "Hinde")]        
      }
      c(types, "Onbekend")
    })
    
    ## Sidebar panel
    
    specieSidebarServer(id = "sidebar", specie = results$specie)
    
    # specie is updated in this page
    observe( 
      updateSelectInput(session, inputId = "sidebar-specie", 
        selected = specie())
    )
    
    observeEvent(input$`sidebar-specie`, 
      results$specie <- reactive(input$`sidebar-specie`))
    
    ## Main panel

    # Tab content with selected plot/table

    outputServer <- reactiveVal(NULL)
  
    # Create plot - UI side
    observe({
          
      if(plot() %in% populatieOutputs){   
        
        outputName <- plot()
        
        # create the plot/table
        ui <- switch(outputName,  
          "boxAgeWeightUI" = {
            boxAgeWeightUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
              doHide = FALSE
            )
          },
          "countAgeCheekUI" = {
            countAgeCheekUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
              doHide = FALSE
            )
          },
          "countAgeGenderUI" = {
            countAgeGenderUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
              doHide = FALSE
            )
          },
          "countEmbryosUI" = {
            countEmbryosUI(
              id = ns(outputName), 
              regionLevels = c(1:2, 4),
              uiText = uiText, context = "description",
              specie = results$specie(),
              doHide = FALSE
            )
          },
          "countAgeGroupUI" = {# dash plot F16_1
            countAgeGroupUI(
              id = ns(outputName), 
              uiText = uiText, context = "description",
              specie = results$specie(),
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
          timeRange = reactive(if (results$specie() == "Ree")
            c(2014, max(results$timeRange())) else 
              results$timeRange())
        ),
        "countAgeCheekUI" = countAgeCheekServer(
          id = outputName,
          data = results$ecoData,
          timeRange = reactive(if (results$specie() == "Ree")
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
    
    return(reactive(results$specie()))
    
  })
  
}