# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



shinyServer(function(input, output, session) {
      
      
      # For debugging
      # -------------
      
      
      observe({
            
            if (is.null(input$debug_console))
              return(NULL)
            
            if (input$debug_console > 0) {
              
              options(browserNLdisabled = TRUE)
              saved_console <- ".RDuetConsole"
              if (file.exists(saved_console)) {load(saved_console)}
              isolate(browser())
              save(file = saved_console, list = ls(environment()))
              
            }
            
          })
      
      
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
            # disclaimer 
            showNotification("Door technische problemen bij de doorstroom van wildschade data van het e-loket naar deze website ontbreekt de recentste wildsschade data van het e-loket", type = "error", duration = NULL)
            source(file.path("uiFiles", "uiWildschade.R"), local = TRUE)$value
            
          })
      
      
    })
