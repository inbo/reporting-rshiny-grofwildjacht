
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
    
    # Plot 8: Onderkaaklengte per jaar
    results$typesGender <- reactive({
        
        loadMetaEco(species = specie())$type_comp
        
      })
    
    results$typesDefaultGender <- reactive({
        
        grep("kits", results$typesGender(), value = TRUE)
        
      })
    
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
        
        # create the plot/table
        ui <- switch(plot(),  
          "boxAgeWeightUI" = {
            boxAgeWeightUI(
              id = ns("plot"), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countAgeCheekUI" = {
            countAgeCheekUI(
              id = ns("plot"), 
              uiText = uiText, context = "description",
              specie = specie(), regionLevels = c(1:2), 
              doHide = FALSE
            )
          },
          "countAgeGenderUI" = {
            countAgeGenderUI(
              id = ns("plot"), 
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countEmbryosUI" = {
            countEmbryosUI(
              id = ns("plot"), 
              regionLevels = c(1:2, 4),
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "countAgeGroupUI" = {# dash plot F16_1
            countAgeGroupUI(
              id = ns("plot"), 
              regionLevels = c(1:2, 4),
              uiText = uiText, context = "description",
              specie = specie(),
              doHide = FALSE
            )
          },
          "plotBioindicatorUI-onderkaaklengte" = {
            plotBioindicatorUI(
              id = ns("plot"),
              bioindicator = "onderkaaklengte",
              regionLevels = c(1:2, 4), showAccuracy = TRUE, 
              uiText = uiText, context = "description",
              doHide = FALSE
            )
          },
          "plotBioindicatorUI-ontweid_gewicht" = {
            plotBioindicatorUI(
              id = ns("plot"), 
              bioindicator = "ontweid_gewicht", 
              regionLevels = c(1:2, 4), 
              uiText = uiText, context = "description",
              doHide = FALSE
            )
          }
        )
        
        # include plot/table in UI
        output[["output"]] <- renderUI(ui)
        
        # activate server-side update
        outputServer(plot())
      
      }

    })

    # Create plot - server side
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      
      switch(outputServer(),
        "boxAgeWeightUI" = boxAgeWeightServer(
          id = "plot",
          data = results$combinedData,
          type = results$leeftijdtypes,
          timeRange = reactive(if (specie() == "Ree")
            c(2014, max(results$timeRange())) else 
              results$timeRange())
        ),
        "countAgeCheekUI" = countAgeCheekServer(
          id = "plot",
          data = results$ecoData,
          timeRange = reactive(if (specie() == "Ree")
            c(2005, max(results$timeRange())) else 
            results$timeRange())
        ),
        "countAgeGenderUI" = countAgeGenderServer(
          id = "plot",
          data = results$ecoData,
          timeRange = results$timeRange
        ),
        "countEmbryosUI" = countEmbryosServer(
          id = "plot",
          data = results$combinedData,
          timeRange = results$timeRange,
          types = results$typesFemale,
          uiText = uiText
        ),
        "countAgeGroupUI" = countAgeGroupServer(
          id = "plot",
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
        ),
        "plotBioindicatorUI-onderkaaklengte" = plotBioindicatorServer(
          id = "plot",
          data = results$combinedData,
          timeRange = results$timeRange,
          types = results$typesGender,
          typesDefault = results$typesDefaultGender,
          bioindicator = "onderkaaklengte"
        ),
        "plotBioindicatorUI-ontweid_gewicht" = plotBioindicatorServer(
          id = "plot",
          data = results$combinedData,
          timeRange = results$timeRange,
          types = results$typesGender,
          typesDefault = results$typesDefaultGender,
          bioindicator = "ontweid_gewicht"
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