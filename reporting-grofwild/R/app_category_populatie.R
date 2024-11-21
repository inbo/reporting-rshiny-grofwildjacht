populatieOutputs <- c(
  "boxAgeWeightUI", 
  "countAgeCheekUI", "countAgeGenderUI", "countEmbryosUI", 
  "countAgeGroupUI"
)

#' UI for the 'populatie indicatoren' Category page
#' @param id id character, module id
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @export
populatieUI <- function(id, specie){
  
  ns <- NS(namespace = id)
      
  verticalLayout(
          
    # header
    headerUI(
      path = c("home", "specie", "category", "subcategory", "plot"), 
      id = id, #specie = specie, 
      category = "Populatie indicatoren",
      subcategory = getTabTitle(value = "leeggewicht", category = "populatie")
    ),
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-populatie-header.png", width = "100%")
      )
    ),
           
    # navigation page with plots and specie sidebar panel
    sidebarLayout(
          
      sidebarPanel = categorySidebarPanel(id = id, specie = specie),
          
      mainPanel = mainPanel(
          
        navbarPage(
            
          title = "",
          
          id = ns("subcategory"),
          
          selected = "informatie",
                
          tabPanel(
            title = getTabTitle(
              value = "leeggewicht", category = "populatie"
            ), 
            value = "leeggewicht",
            uiOutput(outputId = ns("output-leeggewicht"))
          ),
          tabPanel(
            title = getTabTitle(
             value = "onderkaak", category = "populatie"
            ), 
            value = "onderkaak",
            uiOutput(outputId = ns("output-onderkaak"))
          ),
          tabPanel(
            title = getTabTitle(
              value = "geslacht", category = "populatie"
            ), 
            value = "geslacht",
            uiOutput(outputId = ns("output-geslacht"))
          ),
          tabPanel(
            title = getTabTitle(
              value = "voortplanting", category = "populatie"
            ), 
            value = "voortplanting",
            uiOutput(outputId = ns("output-voortplanting"))
          ),
          # menu with all plots/tables
          tabPanelAll(
            category = "populatie", id = id,
            outputs = populatieOutputs, 
            uiText = uiText
          ),
          tabPanelInformatie(
            category = "populatie", id = id, 
            uiText = uiText,
            maxDate = max(ecoData$afschot_datum, na.rm = TRUE),
            specie = specie
          )
        )
      )
    )
  )
  
}

#' Server function for the 'populatie indicatoren' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
populatieServer <- function(id){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    outputCreated <- reactiveVal(value = NULL)
    nextPage <- reactiveVal(value = NULL)
    
    ## input
    results <- reactiveValues(renderedTabs = "Populatie")
    
    results$specie <- reactive(input$specie)
    
    # Create data upon user choices
    results$ecoData <- reactive(
      subset(ecoData, wildsoort == results$specie())
    )
    
    results$geoData <- reactive({
      req(geoData)
      subset(geoData, wildsoort == results$specie())
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
    
    ## Header
    
    # Update specie in path
    output$pathSpecie <- renderUI(
      actionLink(
        inputId = ns("pathSpecie-button"), 
        label = results$specie()
      )    
    )
    
    # Update subcategory in path
    output$pathSubcategory <- renderUI(
      actionLink(
        inputId = ns("pathSubcategory-button"), 
        label = getTabTitle(
          value = input$subcategory, 
          category = "populatie"
        )
      )
    )
    
    # Update plot in path
    observeEvent(outputName(), ignoreNULL = TRUE,
      output$pathPlot <- renderText(
        if(outputName() == ""){
          ""
        }else{
          getOutputTitle(
            output = outputName(), 
            uiText = uiText, specie = results$specie(), 
            type = "populatie",
            n = 55
          )
        }
      )
    )
    
    ## Sidebar with input parameters
    
    # Specie image	
    output$`specie-image` <- renderImage(
      list(src = getSpecieImage(specie = results$specie()), width = "100%")
      , deleteFile = FALSE)
    
    # Specie latin name
    output$`specie-name` <- renderText(
      paste("Latijn:", getLatinName(specie = results$specie()))
    )
    
    ## Tab with available plots
    
    initTab <- reactiveVal(TRUE)
    # Go back to page if subcategory is clicked on in the path
    observeEvent(input$`pathSubcategory-button`, initTab(TRUE))
    observeEvent(input$subcategory, initTab(TRUE))
    
    outputName <- reactiveVal(NULL)

    # Create tab
    observe(if(initTab()){

      if(isTruthy(input$subcategory)){
        
        categoryCardPopulatie <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "populatie", 
            ...
          )
        }
        
        switch(input$subcategory, 
            
          leeggewicht = {
            output$`output-leeggewicht` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "boxAgeWeightUI")
              )
            )
            outputName("")
          },
          onderkaak = {
            output$`output-onderkaak` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countAgeCheekUI")
              )
            )
            outputName("")
          },
          geslacht = {
            output$`output-geslacht` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countAgeGenderUI")
              )
            )
            outputName("")
          },
          voortplanting = {
            output$`output-voortplanting` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countEmbryosUI"),
                categoryCardPopulatie(output = "countAgeGroupUI")
              )
            )
            outputName("")
          }
        )
      }
      initTab(FALSE)
    })
  
    ## Tab content with selected plot/table
    
    outputUI <- reactiveVal(NULL)
    outputServer <- reactiveVal(NULL)
    
    # if plot is selected in the 'all' tab
    observeEvent(input$subcategory, 
      if(input$subcategory %in% populatieOutputs)
        outputUI(input$subcategory)
    )
    
    # if plot is selected based on the category cards
    observeEvent(input$`boxAgeWeightUI-button`, outputUI("boxAgeWeightUI"))
    observeEvent(input$`countAgeCheekUI-button`, outputUI("countAgeCheekUI"))
    observeEvent(input$`countAgeGenderUI-button`, outputUI("countAgeGenderUI"))
    observeEvent(input$`countEmbryosUI-button`, outputUI("countEmbryosUI"))
    observeEvent(input$`countAgeGroupUI-button`, outputUI("countAgeGroupUI"))

    # Create plot - UI side
    observeEvent(outputUI(), ignoreNULL = TRUE, {
      plotName <- outputUI()
      
      # create the plot/table
      switch(plotName, 
          
        "boxAgeWeightUI" = {
          plot <- boxAgeWeightUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-leeggewicht"
        },
        "countAgeCheekUI" = {
          plot <- countAgeCheekUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-onderkaak"
        },
        "countAgeGenderUI" = {
          plot <- countAgeGenderUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-geslacht"
        },
        "countEmbryosUI" = {
          plot <- countEmbryosUI(
            id = ns(plotName), 
            regionLevels = c(1:2, 4),
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-voortplanting"
        },
        "countAgeGroupUI" = {# dash plot F16_1
          plot <- countAgeGroupUI(
            id = ns(plotName), 
            uiText = uiText, context = "description",
            specie = results$specie(),
            doHide = FALSE
          )
          card <- "output-voortplanting"
        }
      )
      
      # include plot/table in UI
      cnt <- ifelse(
        input$subcategory %in% populatieOutputs,
        paste0("plots-", plotName),
        card
      )
      output[[cnt]] <- renderUI(plot)
      
      # re-set in case plot selected via tab after/before category card
      outputUI(NULL)
      
      # activate server-side update
      outputServer(plotName)

    })

    # Create plot - server side
    observeEvent(outputServer(), ignoreNULL = TRUE, {
      plotName <- outputServer()
      
      switch(plotName,
        "boxAgeWeightUI" = boxAgeWeightServer(
          id = plotName,
          data = results$combinedData,
          type = results$leeftijdtypes,
          timeRange = reactive(if (results$specie() == "Ree")
            c(2014, max(results$timeRange())) else 
              results$timeRange())
        ),
        "countAgeCheekUI" = countAgeCheekServer(
          id = plotName,
          data = results$ecoData,
          timeRange = reactive(if (results$specie() == "Ree")
            c(2005, max(results$timeRange())) else 
            results$timeRange())
        ),
        "countAgeGenderUI" = countAgeGenderServer(
          id = plotName,
          data = results$ecoData,
          timeRange = results$timeRange
        ),
        "countEmbryosUI" = countEmbryosServer(
          id = plotName,
          data = results$combinedData,
          timeRange = results$timeRange,
          types = results$typesFemale,
          uiText = uiText
        ),
        "countAgeGroupUI" = countAgeGroupServer(
          id = plotName,
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
      outputName(plotName)
      
      # re-set in case plot selected via tab after/before category card
      outputServer(NULL)
    })
      
    ## Output
      
    # Redirection:

    observeEvent(input$`pathHome`, {
      print("populatie: Go to home page")
      nextPage("home")
    })
  
    observeEvent(input$`pathSpecie-button`, {
      print("populatie: Go to specie page")
      nextPage(structure("specie", specie = results$specie()))
    })
      
    return(nextPage)

  })
  
}