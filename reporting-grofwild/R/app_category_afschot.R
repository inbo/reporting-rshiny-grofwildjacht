#' UI for the 'afschot' Category page
#' @param id id character, module id
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @export
afschotUI <- function(id, specie){
  
  ns <- NS(namespace = id)
      
  verticalLayout(
          
    # header
    headerUI(
      path = c("home", "specie", "category", "subcategory", "plot"), 
      id = id, #specie = id, 
      category = "Afschot",
      subcategory = getTabTitle(value = "vlaanderen", category = "afschot")
    ),
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-afschot-header.png", width = "100%")
      )
    ),
    
    sidebarLayout(
        
      sidebarPanel = categorySidebarPanel(id = id, specie = specie),
        
      mainPanel = mainPanel(

        navbarPage(
            
          title = "",
          
          id = ns("afschot-subcategory"),
          
          selected = "informatie",
                
          tabPanel(
            title = getTabTitle(value = "vlaanderen", category = "afschot"), 
            value = "vlaanderen",
            uiOutput(outputId = ns("afschot-plots-vlaanderen"))
          ),
          tabPanel(
            title = getTabTitle(value = "regio", category = "afschot"), 
            value = "regio",
            uiOutput(outputId = ns("afschot-plots-regio"))
          ),
          tabPanel(
            title = getTabTitle(value = "leeftijdcategorie", category = "afschot"), 
            value = "leeftijdcategorie",
            uiOutput(outputId = ns("afschot-plots-leeftijdcategorie"))
          ),
          tabPanel(
            title = getTabTitle(value = "jachtmethode", category = "afschot"),
            value = "jachtmethode",
            uiOutput(outputId = ns("afschot-plots-jachtmethode"))
          ),
          tabPanelInformatie(
            category = "afschot", id = id, 
            uiText = uiText, 
            maxDate = max(ecoData$afschot_datum, na.rm = TRUE),
            specie = specie
          )
        )

      )
  
    )

  )
  
}

#' Server function for the 'afschot' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
afschotServer <- function(id){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    nextPage <- reactiveVal(value = NULL)
    
    ## input
    results <- reactiveValues(renderedTabs = "Grofwild")
    
    results$specie <- reactive(input$specie)
    
    results$ecoData <- reactive(
      ecoData[ecoData$wildsoort == results$specie(), ]
    )
    results$geoData <- reactive({
      req(geoData)
      geoData[geoData$wildsoort == results$specie(), ]
    })
    
    results$timeRange <- reactive(range(results$ecoData()$afschotjaar))
    results$openingstijdenData <- reactive(
      openingstijdenData[openingstijdenData$Soort == results$specie(), ]
    )
    results$openingstijd <- reactive({
      # for Ree: openingseason contains more year than in the data
      # for Wildboar: openingseason contains less year than in the data
          
      # so retains the years when data and opening season specified
      # and doesn't retain the last year (because not full)
              
      if (results$specie() %in% c("Ree", "Wild zwijn")) {
        openingstijd <- c(
          max(
            min(results$ecoData()$afschotjaar), 
            min(results$openingstijdenData()$Jaar)
          ),
          min(
            max(results$ecoData()$afschotjaar), 
            max(results$openingstijdenData()$Jaar)
          )
        )
      }
    })

    # Enrich data with FBZ
    results$combinedData <- reactive(
      merge(
        x = results$ecoData(), 
        y = results$geoData()[, c("ID", "FaunabeheerZone")], 
        by = "ID"
      )
    ) 

    # Plot: Percentage jaarlijkse afschot
    results$labeltypes <- reactive({
      req(results$openingstijdenData())
      types <- loadMetaEco(species = results$specie())$labeltype
      if (length(types) == 1 && results$specie() == types)
        return(c("alle" = "all")) else 
        return(types)
    })
    
    results$leeftijdtypes <- reactive(
      c(loadMetaEco(species = results$specie())$leeftijd_comp_inbo, "Onbekend")
    )
    
    results$jachttypes <- reactive({
      choices <- unique(results$combinedData()$jachtmethode_comp)
      if (any(is.na(choices)))
        choices[is.na(choices)] <- "onbekend"
      sort(choices)
    })

    results$drukjachtData <- reactive({
      colsGeo <- c("WBE_Naam_Toek", "postcode_afschot_locatie", 
        "FaunabeheerZone", "gemeente_afschot_locatie"
      )
      drukjachtData <- merge(
        x = results$ecoData()[
          results$ecoData()$jachtmethode_comp == "Drukjacht", 
          c("ID", "afschot_datum", "afschotjaar", "provincie", "wildsoort")
        ], 
        y = results$geoData()[, c("ID", colsGeo)], 
        by = "ID", all.x = TRUE
      )
      cols <- c("afschot_datum", "afschotjaar", colsGeo, 
        "provincie", "wildsoort")
      drukjachtData <- drukjachtData[, cols]
      # Keep unique records per WBE & date
      drukjachtData <- drukjachtData[!duplicated(drukjachtData), ]
      validate(need(nrow(drukjachtData) > 0, "Geen data beschikbaar"))
      return(drukjachtData)
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
          value = input$`afschot-subcategory`, 
          category = "afschot"
        )
      )
    )
    
    ## Sidebar panel
    
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
    observeEvent(input$`afschot-subcategory`, initTab(TRUE))

    # Create tab
    observe(if(initTab()){

      if(isTruthy(input$`afschot-subcategory`)){
        
        categoryCardAfschot <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "afschot",
            ...
          )
        }
        
        switch(input$`afschot-subcategory`, 
          vlaanderen = {
            output$`afschot-plots-vlaanderen` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardAfschot(output = "trendYearRegionUI"),
                categoryCardAfschot(output = "countYearProvinceUI", outputFunction = "countYearProvinceUI-afschot"),
                categoryCardAfschot(output = "yearlyShotAnimalsUI")
              )
            )
          },
          regio = {
            output$`afschot-plots-regio` <- renderUI(          
              bslib::layout_column_wrap(
                  width = 1/3, gap = "2em",
                  categoryCardAfschot(output = "mapFlandersUI")
              )
            )
          },
          leeftijdcategorie = {
            output$`afschot-plots-leeftijdcategorie` <- renderUI(          
              bslib::layout_column_wrap(
                 width = 1/3, gap = "2em",
                 categoryCardAfschot(output = "tableProvinceUI"),
                 categoryCardAfschot(output = "countYearShotUI-leeftijd_comp")
              )
            )
          },
          jachtmethode = {
            output$`afschot-plots-jachtmethode` = renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardAfschot(output = "countYearShotUI-jachtmethode_comp"),
                categoryCardAfschot(output = "F04_3")
              )
            )
          }
        )
       }
      initTab(FALSE)
    })
    
    ## Tab content with selected plot
    plotCreated <- reactiveVal("")

    # UI
    observeEvent(input$`trendYearRegionUI-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        trendYearRegionUI(
          id = ns("dash"),
          uiText = uiText, context = "description", specie = results$specie(),
          showCombinatie = TRUE,
          doHide = FALSE
        )
       );
       plotCreated("trendYearRegionUI")
    })

    observeEvent(input$`countYearProvinceUI-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        countYearProvinceUI(
          id = ns("dash"), 
          uiText = uiText, context = "description", type = "afschot",
          specie = results$specie(),
          doHide = FALSE,
          plotFunction = "countYearProvinceUI-afschot"
        )
      );
      plotCreated("countYearProvinceUI")
    })

    observeEvent(input$`yearlyShotAnimalsUI-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        yearlyShotAnimalsUI(
          id = ns("dash"), 
          uiText = uiText, context = "description",
          specie = results$specie(),
          doHide = FALSE
        )
       );
      plotCreated("yearlyShotAnimalsUI")
    })

    observeEvent(input$`mapFlandersUI-button`, {
      output$`afschot-plots-regio` <- renderUI(
        mapFlandersUI(
          id = ns("wild"), 
          type = "grofwild", plotDetails = "region",
          uiText = uiText, specie = results$specie(), 
          typeTitle = "afschot"
        )
      );
      plotCreated("mapFlandersUI")
    })

    observeEvent(input$`tableProvinceUI-button`, {
      output$`afschot-plots-leeftijdcategorie` <- renderUI(
        tableProvinceUI(
          id = ns("wild"), doHide = FALSE,
          uiText = uiText, context = "description", specie = results$specie()
        )
      );
      plotCreated("tableProvinceUI")
    })

    observeEvent(input$`countYearShotUI-leeftijd_comp-button`, {
      output$`afschot-plots-leeftijdcategorie` <- renderUI(
        countYearShotUI(
          id = ns("wild_leeftijd"), groupVariable = "leeftijd_comp",
          regionLevels = c(1:2, 4), 
          uiText = uiText, context = "description", specie = results$specie(),
          doHide = FALSE
        )
      );
      plotCreated("countYearShotUI-leeftijd_comp")
    })

    observeEvent(input$`countYearShotUI-jachtmethode_comp-button`, {
      output$`afschot-plots-jachtmethode` <- renderUI(
        countYearShotUI(
          id = ns("wild_jachtmethode"), groupVariable = "jachtmethode_comp",
          regionLevels = c(1:2, 4), 
          uiText = uiText, context = "description", specie = results$specie(),
          doHide = FALSE
        )
      );
      plotCreated("countYearShotUI-jachtmethode_comp")
    })

    observeEvent(input$`F04_3-button`, {
      output$`afschot-plots-jachtmethode` <- renderUI(
        countYearProvinceUI(
          id = ns("dash"), 
          uiText = uiText, context = "description", specie = results$specie(),
          plotFunction = "F04_3", 
          doHide = FALSE,
          regionLevels = 1:4
        )    
      );
      plotCreated("F04_3")
    })

    observe(print(plotCreated()))
    
    # Update plot in path
    output$pathPlot <- renderText({
      if(isTruthy(plotCreated()))
        getOutputTitle(
          output = plotCreated(), 
          uiText = uiText, specie = results$specie(), type = "afschot",
          n = 55
        )
      else ""
    })

    # Server
    observe(
      switch(plotCreated(),
        trendYearRegionUI = trendYearRegionServer(
            id = "dash", 
            data = results$ecoData, 
            species = results$specie,
            timeRange = results$timeRange,
            regionLevel = reactive("flanders"),
            locaties = reactive("Vlaams Gewest"),
            geoData = results$geoData,
            allSpatialData = spatialData,
            biotoopData = reactive(biotoopData[["flanders"]])
          ),
          countYearProvinceUI = countYearProvinceServer(
            id = "dash",
            data = results$ecoData,
            timeRange = if (id == "Edelhert")
              reactive(c(2008, max(results$ecoData()$afschotjaar))) else 
              results$timeRange
           ),
           yearlyShotAnimalsUI = yearlyShotAnimalsServer(
             id = "dash", 
             data = results$ecoData, 
             timeRange = results$openingstijd, 
             type = results$labeltypes, 
             openingstijdenData = results$openingstijdenData
           ),
           mapFlandersUI = mapFlandersServer(
            id = "wild",
            uiText = uiText,
            defaultYear = defaultYear,
            species = results$specie,
            type = "grofwild",
            geoData = results$geoData,
            biotoopData = biotoopData,
            allSpatialData = spatialData
          ),
          tableProvinceUI = tableProvinceServer(
            id = "wild",
            data = results$ecoData,
            categorie = "leeftijd",
            timeRange = results$timeRange
          ),
          `countYearShotUI-leeftijd_comp` = countYearShotServer(
            id = "wild_leeftijd",
            data = results$combinedData,
            timeRange = results$timeRange,
            groupVariable = "leeftijd_comp",
            types = results$leeftijdtypes
          ),
          `countYearShotUI-jachtmethode_comp` = countYearShotServer(
            id = "wild_jachtmethode",
            data = results$combinedData,
            timeRange = reactive(c(2014, results$timeRange()[2])),
            groupVariable = "jachtmethode_comp",
            types = results$jachttypes
          ),
          `F04_3` = countYearProvinceServer(
            id = "dash", 
            data = results$drukjachtData,
            timeRange = reactive(range(results$drukjachtData()$afschotjaar))
          )
             
        )
      )
      
    ## Output
      
    # Redirection:

    observeEvent(input$`pathHome`, {
      print("afschot: Go to home page")
      nextPage("home")
    })
  
    observeEvent(input$`pathSpecie-button`, {
      print("afschot: Go to specie page")
      nextPage(structure("specie", specie = results$specie()))
    })
      
    return(nextPage)

  })
  
}