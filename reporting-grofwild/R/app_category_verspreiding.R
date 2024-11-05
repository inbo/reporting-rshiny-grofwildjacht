#' UI for the 'verspreiding' Category page
#' @param id id character, module id
#' @inherit shiny::verticalLayout return
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingUI <- function(id, specie){
  
  ns <- NS(namespace = id)
      
  verticalLayout(
          
    # header
    headerUI(
      path = c("home", "specie", "category", "subcategory", "plot"), 
      id = id, specie = specie, 
      category = "Verspreiding",
      subcategory = getTabTitle(value = "huidig", category = "verspreiding")
    ),
            
    # image
    fluidRow(
      column(width = 12, 
        img(src = "www/category-verspreiding-header.png", width = "100%")
      )
    ),
    br(),
           
    # navigation page with plots and specie sidebar panel
    navbarPage(
        
      title = "",
      
      id = ns("subcategory"),
            
      tabPanel(
        title = getTabTitle(
          value = "huidig", category = "verspreiding"
        ), 
        value = "huidig",
        categoryPanel(
          id = id, specie = specie,
          uiOutput(outputId = ns("output-huidig"))
        )
      ),
      
      tabPanel(
        title = getTabTitle(
          value = "toekomstig", category = "verspreiding"
        ), 
        value = "toekomstig",
        categoryPanel(
          id = id, specie = specie,
          uiOutput(outputId = ns("output-toekomstig"))
        )
      )
    )
  )
  
}

#' Server function for the 'verspreiding' Category page
#' @param id id character, module id
#' @return Shiny module function
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingServer <- function(id, specie){
  
  moduleServer(id, function(input, output, session){  
        
    ns <- session$ns
    
    ## initialization
    outputCreated <- reactiveVal(value = NULL)
    nextPage <- reactiveVal(value = NULL)
    
    ## input
    results <- reactiveValues(renderedTabs = "Verspreiding")
    
    results$specie <- reactive(specie())
    
    # Create data upon user choices
    results$spatialData <- reactive({
      req(spatialData)
      filterSpatial(
        allSpatialData = spatialData, 
        species = results$specie(), 
#        regionLevel = req(input$dash_regionLevel), 
        year = NULL
      )
  })
        
#    results$ecoData <- reactive(
#      subset(ecoData, wildsoort == results$specie())
#    )
#    
#    results$geoData <- reactive({
#      req(geoData)
#      subset(geoData, wildsoort == results$specie())
#    })
#    
#    # Enrich data with FBZ
#    results$combinedData <- reactive(
#      merge(
#        x = results$ecoData(), 
#        y = results$geoData()[, c("ID", "FaunabeheerZone")], 
#        by = "ID"
#      )
#    )
    
    ## Header
    
    # Update subcategory in path
    output$pathSubcategory <- renderUI(
      actionLink(
        inputId = ns("pathSubcategory-button"), 
        label = getTabTitle(
          value = input$subcategory, 
          category = "verspreiding"
        )
      )
    )
    
    ## Sidebar with input parameters
    
    ## Tab with available plots
    
    initTab <- reactiveVal(TRUE)
    # Go back to page if subcategory is clicked on in the path
    observeEvent(input$`pathSubcategory-button`, initTab(TRUE))
    observeEvent(input$subcategory, initTab(TRUE))

    # Create tab
    observe(if(initTab()){

      if(isTruthy(input$subcategory)){
        
        categoryCardVerspreiding <- function(...){
          categoryCard(
            id = id, 
            uiText = uiText,
            specie = results$specie(), 
            category = "verspreiding", 
            ...
          )
        }
        
        switch(input$subcategory, 
            
          toekomstig = {
            output$`output-toekomstig` <- renderUI(          
              bslib::layout_column_wrap(
                width = 1/3, gap = "2em",
                categoryCardVerspreiding(output = "mapSpreadUI")
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

    # dash plot F17_4
    observeEvent(input$`mapSpreadUI-button`, {
      output$`output-toekomstig` <- renderUI(
          mapSpreadUI(
          id = ns(id), 
          uiText = uiText, context = "wild",
          specie = results$specie(),
          doHide = FALSE
        )
      )
       outputCreated("mapSpreadUI")
    })

    observe(print(outputCreated()))
    
    # Update plot in path
    output$pathPlot <- renderText({
      if(isTruthy(outputCreated()))
        getOutputTitle(
          output = outputCreated(), 
          uiText = uiText, specie = results$specie(), 
            type = "verspreiding",
          n = 55
        )
      else ""
    })

    # Server
    observe(
      switch(outputCreated(),
        mapSpreadUI = mapSpreadServer(
          id = id,
          allSpatialData = spatialData,
          regionLevel = reactive("flanders"),
          locaties = reactive("Vlaams Gewest"),
          species = results$specie(),
          type = "F17_4"
        )
      )
    )
      
    ## Output
      
    # Redirection:

    observeEvent(input$`pathHome`, {
      print("verspreiding: Go to home page")
      nextPage("home")
    })
  
    observeEvent(input$`pathSpecie-button`, {
      print("verspreiding: Go to specie page")
      nextPage("specie")
    })
      
    return(nextPage)

  })
  
}