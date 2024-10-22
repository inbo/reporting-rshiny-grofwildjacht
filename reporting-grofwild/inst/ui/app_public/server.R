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
  
  # Current specie
  specie <- reactiveVal()
        
  ## Home page
  observeEvent(page(), {
    print(paste("Update page to:", page()))
    if(identical(page(), "home")){
      print("Open home page")
      output$page <- renderUI(frontUI())
    }
    if(identical(page(), "afschot")){
      print("Open afschot page")
      output$page <- renderUI(afschotUI(id = specie()))
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
      result <- specieServer(id = input$wildsoort)      
      
      # re-direct to 'Home' or 'Category' page
      nextPage <- result$nextPage()
      if(!is.null(nextPage)){
        print(paste("Page will be changed to:", nextPage))
          page(nextPage)
          specie(result$specie)# save specie selected in the app
       }

     }
  })

  observe(print(paste("Specie currently selected is:", specie())))

})
