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
      path = c("home", "specie", "category", "plot"), 
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
      
      id = ns("afschot-plots"),
            
      tabPanel(
        title = "Afschot in Vlaanderen", value = "vlaanderen",
        afschotPanel(
          id = id,
          uiOutput(outputId = ns("afschot-plots-vlaanderen"))
        )
      ),
      tabPanel(title = "Afschot per regio", value = "regio"),
      tabPanel(title = "Afschot per leeftijdscategorie", value = "leeftijdcategorie"),
      tabPanel(title = "Afschot per jachtmethode", value = "jachtmethode")
    )

  )
  
}

#' Get category card for a specific plot
#' @param id id character, module id/specie
#' @param plot character, plot name, e.g. 'afschot-vlaanderen'
#' @param title character, plot title
#' @param description character, plot description
#' @inherit bslib::card return
#' @author lcougnaud
#' @importFrom bslib card card_header card_image card_body card_footer
#' @importFrom shiny actionButton
categoryCard <- function(id, plot, title, description){
  
  ns <- NS(id)
  
  file <- file.path("www", paste0("category-", plot, ".png"))
  output <- bslib::card(
    class = "category-card",
    bslib::card_header(title, class = "category-card-header"), 
    br(),
    bslib::card_image(src = file, class = "category-card-image"),
    br(),
    bslib::card_body(description),
    br(), br(),
    bslib::card_footer(
      align = "center",
      shiny::actionButton(
        inputId = ns(paste0(plot, "-button")), 
        label = "Bekijk grafiek", class = "category-card-action-button")
    )
  )
  
  return(output)
  
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
    
    # Update plot in path
    output$pathPlot <- renderText(input$`afschot-plots`)
    
    # Initiate tabs
    output$`afschot-plots-vlaanderen` <- renderUI(          
      bslib::layout_column_wrap(
        width = 1/3, gap = "2em",
        categoryCard(
          id = id,
          plot = "afschot-vlaanderen",
          title = sprintf("Jaarlijks gerapporteerd afschot van %s in Vlaanderen", tolower(id)),
          description = sprintf("Een lijngrafiek van het jaarlijks afschot van %s in Vlaanderen.", tolower(id))
        ),
        categoryCard(
          id = id,
          plot = "afschot-provincie",
          title = sprintf("Jaarlijks gerapporteerd afschot van %s per provincie", tolower(id)),
          description = "Een barplot van het aantal geschoten dieren per jaar in de verschillende provincies."
        ),
        categoryCard(
          id = id,
          plot = "afschot-vlaanderen-percentage",
          title = sprintf("Percentage afschot van %s in Vlaanderen t.o.v. een referentieperiode", tolower(id)),
          description = sprintf("De procentuele verdeling van het afschot van %s gedurende het geselecteerde jaar t.o.v de verdeling tijdens een referentieperiode.", tolower(id))
       )
    ))
    
    # Render the plots
    plotCreated <- reactiveVal("")

    # UI
    observeEvent(input$`afschot-vlaanderen-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        trendYearRegionUI(
          id = "dash", 
          ns = ns, uiText = uiText, 
          showCombinatie = TRUE,
          plotFunction = "F05_1", doHide = FALSE
        )
       );
       plotCreated("afschot-vlaanderen")
    })

    observeEvent(input$`afschot-provincie-button`, {
      output$`afschot-plots-vlaanderen` <- renderUI(
        countYearProvinceUI(id = ns("dash"), uiText = uiText, doHide = FALSE)
      );
      plotCreated("afschot-provincie")
    })

    observe(print(plotCreated()))

    # Server
    observe(
      switch(plotCreated(),
        `afschot-vlaanderen` = trendYearRegionServer(
            id = "dash", 
            data = results$ecoData, 
            species = results$specie,
            timeRange = results$timeRange,
            regionLevel = reactive("flanders"),
            locaties = reactive("Vlaams Gewest"),
            geoData = reactive(specieGeoData),
            allSpatialData = spatialData,
            biotoopData = reactive(biotoopData[["flanders"]]),
            title = reactive(names(dash_titlesJacht()[dash_titlesJacht() == "F05_1"]))
          ),
          `afschot-provincie` = {
            countYearProvinceServer(
              id = "dash",
              data = results$ecoData,
              timeRange = reactive(
                if (id == "Edelhert")
                  c(2008, max(specieEcoData$afschotjaar)) else 
                  range(specieEcoData$afschotjaar)),
              title = reactive(uiText$title[uiText$plotFunction == "countYearProvinceUI"])
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