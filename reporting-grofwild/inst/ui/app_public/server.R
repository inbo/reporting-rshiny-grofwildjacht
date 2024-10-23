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
    switch(page(),
      home = {
        print("Open home page")
        output$page <- renderUI(frontUI())
      },
      afschot = {
        print("Open afschot page")
        output$page <- renderUI(afschotUI(id = specie()))
    })
  })
   
  ## Specie page
  
  # Create the page
  observeEvent(input$wildsoort, {
    if(isTruthy(input$wildsoort)){
      print("Create specie page")
      output$page <- renderUI(specieUI(id = input$wildsoort))
    }
  })

  # Update the components of the page which are specie-specific
  observe({
    nextPage <- specieServer(id = input$wildsoort)

    if(!is.null(nextPage())){
      
      # re-direct to 'Home' or 'Category' page
      page(nextPage())
      
      # save specie selected in the app
      if(!is.null(attr(nextPage(), "specie")))
        specie(attr(nextPage(), "specie"))
    }
  })

  observe(print(paste("Page currently selected is:", page())))
  observe(print(paste("Specie currently selected is:", specie())))
  
  ## Category page
  observe(if(page() == "afschot") afschotServer(id = specie()))

})
