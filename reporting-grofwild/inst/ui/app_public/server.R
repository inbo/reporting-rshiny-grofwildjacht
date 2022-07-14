# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



shinyServer(function(input, output, session) {
      
      
      # For debugging
      # -------------
      
      
      observeEvent(input$debug_console, browser())
      
      
      output$print <- renderPrint({
            
#                        names(session$clientData)
            
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
      output$grof_content <- renderUI({
          
          source(file.path("uiFiles", "uiGrofwild.R"), local = TRUE)$value
          
        })
      
      # Tabpanel wildschade
      output$schade_content <- renderUI({
          source(file.path("uiFiles", "uiWildschade.R"), local = TRUE)$value
          
        })
      
      # Tabpanel dashboard
      output$dash_content <- renderUI({
          source(file.path("uiFiles", "uiDashboard.R"), local = TRUE)$value
          
        })
      
      observeEvent(input$tabs, {
          if (input$tabs == "WBE")
            browseURL("https://grofwildjacht.inbo.be/WBE")
        })
      
    })
