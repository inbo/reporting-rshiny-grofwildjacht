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
  output$page <- renderUI(frontUI())
#  isHome <- reactive(TRUE)
#  output$page <- eventReactive(isHome(), 
#    renderUI(frontUI(id = "front"))
#  )
   
  ## Specie page
      
  # Create the page
  observe(
    if(isTruthy(input$wildsoort))
      output$page <- renderUI(specieUI(input))      
  )   
  
  # Update the page
  # specieServer(input, output, session)
      
})
