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
        
  ## Initiate the application
  page <- reactiveVal("home")
  observe({
    if(identical(page(), "home")){
      print("Go to home page")
      output$page <- renderUI(frontUI())
    }
  })
   
  ## Specie page
  observe({
    if(isTruthy(input$wildsoort)){
      
      # Create the page
      output$page <- renderUI(specieUI(id = input$wildsoort)) 
      # Update the components of the page which are specie-specific
      currentPage <- specieServer(id = input$wildsoort)
      if(!is.null(currentPage())){
        print(paste("Page changed to:", currentPage()))
        page(currentPage())
      }
      
    }
  })

})
