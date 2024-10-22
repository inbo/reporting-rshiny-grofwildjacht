# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################

shinyServer(function(input, output, session) {
      
      
  # For debugging
  # -------------
  observeEvent(input$debug_console, browser())
      
  output$debug <- renderUI({
            
    if (doDebug)
      tags$div(style = "margin-top:50px",
        h5(actionLink(inputId = "debug_console", label = "Connect with console"),
        align = "left"),
        verbatimTextOutput("print")
      )
    })

  # Current page (home at initialization)
  page <- reactiveVal("home")
        
  ## Home page
  observeEvent(page(), {
    if(identical(page(), "home"))
      output$page <- renderUI(frontUI())
  })
   
  ## Specie page
  
  # Create the page
  observeEvent(input$wildsoort, {
    if(isTruthy(input$wildsoort)){
      output$page <- renderUI(specieUI(id = input$wildsoort)) 
      page("specie")
    }
  })

  # Update the components of the page which are specie-specific
  observe({
    if(isTruthy(input$wildsoort)){
      currentPage <- specieServer(id = input$wildsoort)
      if(!is.null(currentPage())){
        print(paste("Page changed to:", currentPage()))
        page(currentPage())
       }
     }
  })

})
