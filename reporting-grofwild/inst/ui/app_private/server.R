# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



shinyServer(function(input, output, session) {
      
      
      # For debugging
      # -------------
      
      
      observeEvent(input$debug_console, browser())
      
      
      output$print <- renderPrint({
            
                        results$labeltypes()
            
          })
      
      
      output$debug <- renderUI({
            
            if (doDebug)
              tags$div(style = "margin-top:50px",
                  h5(actionLink(inputId = "debug_console", label = "Connect with console"),
                      align = "left"),
                  verbatimTextOutput("print")
              )
          })
      
      
      
      # Tabpages
      # ----------
      
      
      
      results <- reactiveValues()
      
      
      # Load code for all tabpages
      for (serverFile in list.files("serverFiles", full.names = TRUE))
        source(serverFile, local = TRUE)
      
      
      
      # Tabpanel grofwild
      output$wbe_content <- renderUI({
            
            source(file.path("uiFiles", "uiWbe.R"), local = TRUE)$value
            
          })
      
    })
