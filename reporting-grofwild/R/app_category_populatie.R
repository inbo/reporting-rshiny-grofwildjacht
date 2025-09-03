
#' Server function for an output (plot/table) of the 'populatie' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export
populatieOutputServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(), plot = reactiveVal(),
  subcategories = character(),
  outputs = character(), defaultTabs = NULL,
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
        req(subcategory())
        req(plot())
        
      if(subcategory() %in% subcategories){   
        
        ui <- switch(subcategory(), 
          "populatie-leeggewicht" = {
            tagList(
              if ("boxAgeWeightUI" %in% outputs)
                wellPanel(class = "well-white", boxAgeWeightUI(
                    id = ns("plot"), 
                    uiText = uiText, context = "description",
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "boxAgeWeightUI" %in% plot())
                  )),
              if ("plotBioindicatorUI-ontweid_gewicht" %in% outputs)
                wellPanel(class = "well-white", plotBioindicatorUI(
                    id = ns("plot"), 
                    bioindicator = "ontweid_gewicht", 
                    regionLevels = c(1:2, 4), 
                    uiText = uiText, context = "description",
                    doHide = !(plot() == defaultTabs$plot || "plotBioindicatorUI-ontweid_gewicht" %in% plot())
                  ))
            )
          },
          "populatie-onderkaak" = {
            tagList(
              if ("countAgeCheekUI" %in% outputs)
                wellPanel(class = "well-white", countAgeCheekUI(
                    id = ns("plot"), 
                    uiText = uiText, context = "description",
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "countAgeCheekUI" %in% plot())
                  )),
              if ("plotBioindicatorUI-onderkaaklengte" %in% outputs)
                wellPanel(class = "well-white", plotBioindicatorUI(
                    id = ns("plot"),
                    bioindicator = "onderkaaklengte",
                    regionLevels = c(1:2, 4), showAccuracy = TRUE, 
                    uiText = uiText, context = "description",
                    doHide = !(plot() == defaultTabs$plot || "plotBioindicatorUI-onderkaaklengte" %in% plot())
                  ))
            )
          },
          "populatie-geslacht" = {
            tagList(
              if ("countAgeGenderUI" %in% outputs)
                wellPanel(class = "well-white", countAgeGenderUI(
                    id = ns("plot"), 
                    uiText = uiText, context = "description",
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "countAgeGenderUI" %in% plot())
                  ))
            )
          },
          "populatie-voortplanting" = {
            tagList(
              if ("countAgeGroupUI" %in% outputs)
                wellPanel(class = "well-white", countAgeGroupUI(
                    id = ns("plot"), 
                    uiText = uiText, context = "description",
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "countAgeGroupUI" %in% plot())
                  )),
              if ("countEmbryosUI" %in% outputs)
                wellPanel(class = "well-white", countEmbryosUI(
                    id = ns("plot"), 
                    regionLevels = c(1:2, 4),
                    uiText = uiText, context = "description",
                    specie = specie(),
                    doHide = !(plot() == defaultTabs$plot || "countEmbryosUI" %in% plot())
                  ))
            )
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
        "populatie-leeggewicht" = {
          c(
            if ("boxAgeWeightUI" %in% outputs)
              boxAgeWeightServer(
                id = "plot",
                data = results$combinedData,
                type = results$leeftijdtypes,
                timeRange = reactive(if (specie() == "Ree")
                      c(2014, max(results$timeRange())) else 
                      results$timeRange())
              ),
            if ("plotBioindicatorUI-ontweid_gewicht" %in% outputs)
              plotBioindicatorServer(
                id = "plot",
                data = results$combinedData,
                timeRange = results$timeRange,
                types = results$typesGender,
                typesDefault = results$typesDefaultGender,
                bioindicator = "ontweid_gewicht"
              )
          )
        },
        "populatie-onderkaak" = {
          c(
            if ("countAgeCheekUI" %in% outputs)
              countAgeCheekServer(
                id = "plot",
                data = results$ecoData,
                timeRange = reactive(if (specie() == "Ree")
                      c(2005, max(results$timeRange())) else 
                      results$timeRange())
              ),
            if ("plotBioindicatorUI-onderkaaklengte" %in% outputs)
              plotBioindicatorServer(
                id = "plot",
                data = results$combinedData,
                timeRange = results$timeRange,
                types = results$typesGender,
                typesDefault = results$typesDefaultGender,
                bioindicator = "onderkaaklengte"
              )
          )
        },
        "populatie-geslacht" = {
          c(
            if ("countAgeGenderUI" %in% outputs)
              countAgeGenderServer(
                id = "plot",
                data = results$ecoData,
                timeRange = results$timeRange
              )
          )
        },
        "populatie-voortplanting" = {
          c(
            if ("countEmbryosUI" %in% outputs)
              countEmbryosServer(
                id = "plot",
                data = results$combinedData,
                timeRange = results$timeRange,
                types = results$typesFemale,
                uiText = uiText
              ),
            if ("countAgeGroupUI" %in% outputs)
              countAgeGroupServer(
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
              )
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