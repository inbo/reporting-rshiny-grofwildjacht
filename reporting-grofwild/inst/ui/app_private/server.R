# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck
###############################################################################



shinyServer(function(input, output, session) {
      
      
      # For debugging
      # -------------
      
      
      observeEvent(input$debug_console, browser())
      
      
      output$print <- renderPrint({
                        
          })
      
      
      output$debug <- renderUI({
            
            if (doDebug)
              tags$div(style = "margin-top:50px",
                  h5(actionLink(inputId = "debug_console", label = "Connect with console"),
                      align = "left"),
                  verbatimTextOutput("print")
              )
          })
      
      
      # Version
      # -------
    
      versionServer(id = "private")  
    
      
      # Tabpages
      # ----------
      
      # Tabpanel WBE
      wbeServer(id = NULL, 
        currentKbo = currentKbo, 
        ecoData = ecoData, 
        geoData = geoData, 
        schadeData = schadeData,
        toekenningsData = toekenningsData, 
        biotoopData = biotoopData,
        spatialData = spatialData,
        defaultYear = defaultYear, 
        uiText = uiText
      )
                
      observeEvent(input$tabs, {
          if (input$tabs %in% c("Grofwild", "Wildschade", "Dashboard"))
            js$browseURL("https://faunabeheer.inbo.be")
        })
      
    })
