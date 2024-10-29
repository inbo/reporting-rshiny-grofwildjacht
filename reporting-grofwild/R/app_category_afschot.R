#' UI for the 'afschot' Category page
#' @param id id character, module id/specie
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @export
afschotUI <- function(id){
  
  ns <- NS(namespace = id)
      
  verticalLayout(
          
    # header
    headerUI(
      path = c("home", "specie", "category", "subcategory", "plot"), 
      id = id, specie = id, 
      category = "Afschot",
      subcategory = getTabTitle(value = "vlaanderen", category = "afschot")
    ),
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-afschot-header.png", width = "100%")
      )
    ),
    br(),
           
    # navigation page with plots and specie sidebar panel
    navbarPage(
        
      title = "",
      
      id = ns("afschot-subcategory"),
            
      tabPanel(
        title = getTabTitle(value = "vlaanderen", category = "afschot"), 
        value = "vlaanderen",
        afschotPanel(
          id = id,
          uiOutput(outputId = ns("afschot-plots-vlaanderen"))
        )
      ),
      tabPanel(
        title = getTabTitle(value = "regio", category = "afschot"), 
        value = "regio",
        afschotPanel(
          id = id,
          uiOutput(outputId = ns("afschot-plots-regio"))
        )
      ),
      tabPanel(
        title = getTabTitle(value = "leeftijdcategorie", category = "afschot"), 
        value = "leeftijdcategorie",
        afschotPanel(
          id = id,
           uiOutput(outputId = ns("afschot-plots-leeftijdcategorie"))
        )
      ),
      tabPanel(
        title = getTabTitle(value = "jachtmethode", category = "afschot"),
        value = "jachtmethode"
      )
    )

  )
  
}

#' Server function for the 'afschot' Category page
#' @param id id character, module id/specie
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
afschotServer <- function(id){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## input
    results <- reactiveValues(renderedTabs = "Grofwild")
    
    specieEcoData <- ecoData[ecoData$wildsoort == id, ] # specieEcoData
    specieGeoData <- geoData[geoData$wildsoort == id, ] # specieGeoData
    results$ecoData <- reactive(specieEcoData)
    results$geoData <- reactive({
      req(geoData)
      specieGeoData
    })
    results$specie <- reactive(id)
    results$timeRange <- reactive(range(results$ecoData()$afschotjaar))
    results$openingstijdenData <- reactive(
      openingstijdenData[openingstijdenData$Soort == id, ]
    )
    results$openingstijd <- reactive({
      # for Ree: openingseason contains more year than in the data
      # for Wildboar: openingseason contains less year than in the data
          
      # so retains the years when data and opening season specified
      # and doesn't retain the last year (because not full)
              
      if (id %in% c("Ree", "Wild zwijn")) {
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
      types <- loadMetaEco(species = id)$labeltype
      if (length(types) == 1 && id == types)
        return(c("alle" = "all")) else 
        return(types)
    })
    
    jachtChoices <- c("F04_3", "F05_1", "F05_2")
    dash_titlesJacht <- reactive(
      namedChoices(jachtChoices, uiText = uiText, regionLevel = "flanders")
    )
    
    results$leeftijdtypes <- reactive(
      c(loadMetaEco(species = id)$leeftijd_comp_inbo, "Onbekend")
    )
    
    ## Header
        
    # Update specie in path
    output$pathSpecie <- renderText(id)
    
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
    
    ## Tab with available plots
    
    initTab <- reactiveVal(TRUE)
    # Go back to page if subcategory is clicked on in the path
    observeEvent(input$`pathSubcategory-button`, initTab(TRUE))
    observeEvent(input$`afschot-subcategory`, initTab(TRUE))

    # Create tab
    observe(if(initTab()){
      print("update tab")
      if(isTruthy(input$`afschot-subcategory`))
        switch(input$`afschot-subcategory`, 
          vlaanderen = {
            output$`afschot-plots-vlaanderen` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCard(id = id, plot = "trendYearRegionUI", uiText = uiText),
                categoryCard(id = id, plot = "countYearProvinceUI", uiText = uiText),
                categoryCard(id = id, plot = "yearlyShotAnimalsUI", uiText = uiText)
              )
            )
          },
          regio = {
            output$`afschot-plots-regio` <- renderUI(          
              bslib::layout_column_wrap(
                  width = 1/3, gap = "2em",
                  categoryCard(id = id, plot = "mapFlandersUI", uiText = uiText)
              )
            )
          },
          leeftijdcategorie = {
            output$`afschot-plots-leeftijdcategorie` <- renderUI(          
              bslib::layout_column_wrap(
                 width = 1/3, gap = "2em",
                 categoryCard(id = id, plot = "tableProvinceUI", uiText = uiText),
                 categoryCard(id = id, plot = "countYearShotUI-leeftijd_comp", uiText = uiText),
              )
            )
          }
        )
      initTab(FALSE)
    })
    
    ## Tab content with selected plot
    plotCreated <- reactiveVal("")

    # UI
    observeEvent(input$`trendYearRegionUI-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        trendYearRegionUI(
          id = ns("dash"),
          uiText = uiText, context = "fauna", specie = id,
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
          uiText = uiText, context = "wild",
          specie = id,
          doHide = FALSE
        )
      );
      plotCreated("countYearProvinceUI")
    })

    observeEvent(input$`yearlyShotAnimalsUI-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        yearlyShotAnimalsUI(
          id = ns("dash"), 
          uiText = uiText, context = "wild",
          specie = id,
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
          uiText = uiText, specie = id, typeTitle = "afschot"
        )
      );
      plotCreated("mapFlandersUI")
    })

    observeEvent(input$`tableProvinceUI-button`, {
      output$`afschot-plots-leeftijdcategorie` <- renderUI(
        tableProvinceUI(
          id = ns("wild"), doHide = FALSE,
          uiText = uiText, context = "wild", specie = id
        )
      );
      plotCreated("tableProvinceUI")
    })

    observeEvent(input$`countYearShotUI-leeftijd_comp-button`, {
      output$`afschot-plots-leeftijdcategorie` <- renderUI(
        countYearShotUI(
          id = ns("wild_leeftijd"), groupVariable = "leeftijd_comp",
          regionLevels = c(1:2, 4), 
          uiText = uiText, context = "wild", specie = id,
          doHide = FALSE
        )
      );
      plotCreated("countYearShotUI-leeftijd_comp")
    })

    observe(print(plotCreated()))
    
    # Update plot in path
    output$pathPlot <- renderText({
      if(isTruthy(plotCreated()))
        getPlotTitle(
          plot = plotCreated(), 
          uiText = uiText, specie = id, type = "afschot",
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
              reactive(c(2008, max(specieEcoData$afschotjaar))) else 
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
            species = reactive(id),
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
          )
             
        )
      )

  })
  
}

#' Wrapper for the sidebar of the 'afschot' Category page
#' @param id id character, module id/specie
#' @param ... Elements for the \code{\link[shiny]{mainPanel}}
#' @inherit shiny::sidebarLayout return
#' @author lcougnaud
afschotPanel <- function(id, ...){
  
  ns <- NS(namespace = id)
  
  sidebarLayout(
    position = "left", 
      
    sidebarPanel = sidebarPanel(
      width = 3, 
      id = "category-sidebar", 
      h4(id, align = "center"),
      img(src = getSpecieImage(specie = id, relative = TRUE), width = "100%", height = "auto"),
      br(),
      div(strong(paste("Latijn:", getLatinName(specie = id))), align = "center")
    ),
      
    mainPanel = mainPanel(width = 9, ...)

  )
  
}

#' Get category card for a specific plot
#' @param id id character, module id/specie
#' @param plot character, plot name, e.g. 'trendYearRegionUI'
#' @inherit bslib::card return
#' @author lcougnaud
#' @importFrom bslib card card_header card_image card_body card_footer
#' @importFrom shiny actionButton
categoryCard <- function(id, plot, uiText){
  
  ns <- NS(id)
  
  title <- getPlotTitle(plot = plot, specie = id, uiText = uiText)

  description <- getPlotDescription(plot = plot, specie = id, uiText = uiText)
  
  file <- system.file("ui", "www", paste0("category-", plot, ".png"), package = "reportingGrofwild")
  
  output <- bslib::card(
    class = "category-card",
    bslib::card_header(title, class = "category-card-header"), 
    br(),
    bslib::card_image(file = file, class = "category-card-image"),
    br(),
    bslib::card_body(description),
    br(), br(),
    bslib::card_footer(
      align = "center",
      shiny::actionButton(
        inputId = ns(paste0(plot, "-button")), 
        label = "Bekijk grafiek", class = "category-card-action-button"
      )
    )
  )
  
  return(output)
  
}