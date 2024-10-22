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
    print(paste("Update page to:", page()))
    if(identical(page(), "home")){
      print("Open home page")
      output$page <- renderUI(frontUI())
    }
    if(identical(page(), "afshot")){
      print("Open afshot page")
      output$page <- renderUI(afshotUI(id = "test"))
    }
  })
   
  ## Specie page
  
  # Create the page
  observeEvent(input$wildsoort, {
    if(isTruthy(input$wildsoort)){
      print("Create specie page")
      output$page <- renderUI(specieUI(id = input$wildsoort)) 
      page("specie")
    }
  })

  # Update the components of the page which are specie-specific
  observe({
    if(isTruthy(input$wildsoort)){
      print("Update specie page")
      nextPage <- specieServer(id = input$wildsoort)
      if(!is.null(nextPage())){
        print(paste("Page will be changed to:", nextPage()))
        page(nextPage())
       }
     }
  })

})
