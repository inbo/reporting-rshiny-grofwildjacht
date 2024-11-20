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
          },
          onderkaak = {
            output$`output-onderkaak` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countAgeCheekUI")
              )
            )
          },
          geslacht = {
            output$`output-geslacht` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countAgeGenderUI")
              )
            )
          },
          voortplanting = {
            output$`output-voortplanting` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardPopulatie(output = "countEmbryosUI"),
                categoryCardPopulatie(output = "countAgeGroupUI")
              )
            )
          }
        )
        
      }
      
      outputCreated("")# reset path
      initTab(FALSE)
      
    })
  
    ## Tab content with selected plot/table

    # UI
    observeEvent(input$`boxAgeWeightUI-button`, {
      output$`output-leeggewicht` <- renderUI(
        boxAgeWeightUI(
          id = ns(id), 
          uiText = uiText, context = "description",
          specie = results$specie(),
          doHide = FALSE
        )
      )
       outputCreated("boxAgeWeightUI")
    })

    observeEvent(input$`countAgeCheekUI-button`, {
      output$`output-onderkaak` <- renderUI(
        countAgeCheekUI(
          id = ns(id), 
          uiText = uiText, context = "description",
          specie = results$specie(),
          doHide = FALSE
        )
      )
      outputCreated("countAgeCheekUI")
    })

    observeEvent(input$`countAgeGenderUI-button`, {
      output$`output-geslacht` <- renderUI(
        countAgeGenderUI(
          id = ns(id), 
          uiText = uiText, context = "description",
          specie = results$specie(),
          doHide = FALSE
        )
      )
      outputCreated("countAgeGenderUI")
    })

    observeEvent(input$`countEmbryosUI-button`, {
      output$`output-voortplanting` <- renderUI(
        countEmbryosUI(
          id = ns(id), 
          regionLevels = c(1:2, 4),
          uiText = uiText, context = "description",
          specie = results$specie(),
          doHide = FALSE
        )
      )
      outputCreated("countEmbryosUI")
    })

    # dash plot F16_1
    observeEvent(input$`countAgeGroupUI-button`, {
      output$`output-voortplanting` <- renderUI(
        countAgeGroupUI(
          id = ns(id), 
          uiText = uiText, context = "description",
          specie = results$specie(),
          doHide = FALSE
        )
      )
      outputCreated("countAgeGroupUI")
    })

    observe(print(outputCreated()))
    
    # Update plot in path
    output$pathPlot <- renderText({
      if(isTruthy(outputCreated()))
        getOutputTitle(
          output = outputCreated(), 
          uiText = uiText, specie = results$specie(), 
            type = "populatie",
          n = 55
        )
      else ""
    })

    # Server
    observe(
      switch(outputCreated(),
        boxAgeWeightUI = boxAgeWeightServer(
          id = id,
          data = results$combinedData,
          type = results$leeftijdtypes,
          timeRange = reactive(if (results$specie() == "Ree")
            c(2014, max(results$timeRange())) else 
              results$timeRange())
        ),
        countAgeCheekUI = countAgeCheekServer(
          id = id,
          data = results$ecoData,
          timeRange = reactive(if (results$specie() == "Ree")
            c(2005, max(results$timeRange())) else 
            results$timeRange())
        ),
        countAgeGenderUI = countAgeGenderServer(
          id = id,
          data = results$ecoData,
          timeRange = results$timeRange
        ),
        countEmbryosUI = countEmbryosServer(
          id = id,
          data = results$combinedData,
          timeRange = results$timeRange,
          types = results$typesFemale,
          uiText = uiText
        ),
        countAgeGroupUI = countAgeGroupServer(
          id = id,
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
    )
      
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