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
  isHome <- reactiveVal(TRUE)
  observe({
    if(isHome())
      output$page <- renderUI(frontUI())
  })
   
  ## Specie page  
  observe({
    if(isTruthy(input$wildsoort)){
      
      # Create the page
      output$page <- renderUI(specieUI(id = input$wildsoort)) 
      # Update the components of the page which are specie-specific
      goHome <- specieServer(id = input$wildsoort)
      isHome(goHome())
      
    }
  })

})
