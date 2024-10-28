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
      id = id, specie = id, category = "Afschot"
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
        value = "regio"
      ),
      tabPanel(
        title = getTabTitle(value = "leeftijdcategorie", category = "afschot"), 
        value = "leeftijdcategorie"
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
    results$specie <- reactive(id)
    results$timeRange <- reactive(range(specieEcoData$afschotjaar))
    
    jachtChoices <- c("F04_3", "F05_1", "F05_2")
    dash_titlesJacht <- reactive(
      namedChoices(jachtChoices, uiText = uiText, regionLevel = "flanders")
    )
    
        
    ## Header
        
    # Update specie in path
    output$pathSpecie <- renderText(id)
    
    # Update subcategory in path
    output$pathSubcategory <- renderText(
      getTabTitle(value = input$`afschot-subcategory`, category = "afschot")
    )
    
    # Initiate tabs
    output$`afschot-plots-vlaanderen` <- renderUI(          
      bslib::layout_column_wrap(
        width = 1/3, gap = "2em",
        categoryCard(id = id, plot = "trendYearRegionUI", uiText = uiText),
        categoryCard(id = id, plot = "countYearProvinceUI", uiText = uiText),
        categoryCard(id = id, plot = "yearlyShotAnimalsUI", uiText = uiText)
      )
    )
    
    # Render the plots
    plotCreated <- reactiveVal("")

    # UI
    observeEvent(input$`trendYearRegionUI-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        trendYearRegionUI(
          id = ns("dash"),
          uiText = uiText, context = "fauna",
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
          uiText = uiText, context = "fauna",
          doHide = FALSE
        )
      );
      plotCreated("countYearProvinceUI")
    })

    observe(print(plotCreated()))
    
    # Update plot in path
    output$pathPlot <- renderText({
      req(plotCreated())
      paste0(
        substr(
          x = uiText[uiText$plotFunction == plotCreated(), "title"],
          1, 30
        ), 
        "..."
      )
    })

    # Server
    observe(
      switch(plotCreated(),
        `trendYearRegionUI` = trendYearRegionServer(
            id = "dash", 
            data = results$ecoData, 
            species = results$specie,
            timeRange = results$timeRange,
            regionLevel = reactive("flanders"),
            locaties = reactive("Vlaams Gewest"),
            geoData = reactive(specieGeoData),
            allSpatialData = spatialData,
            biotoopData = reactive(biotoopData[["flanders"]])
          ),
          `countYearProvinceUI` = {
            countYearProvinceServer(
              id = "dash",
              data = results$ecoData,
              timeRange = if (id == "Edelhert")
                  reactive(c(2008, max(specieEcoData$afschotjaar))) else 
                  results$timeRange
            ) 
          }
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
  
  title <- uiText[match(plot, uiText$plotFunction), "title"]
  description <- uiText[match(plot, uiText$plotFunction), "fauna"]
  
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