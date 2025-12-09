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
      
        # Contact
        # -------
        
        contactServer(id = "privateContact", uiText = uiText, subject = 'Faunabeheer WBE web applicatie') 
        
        
      # Version
      # -------
    
      versionServer(id = "private")  
    
      
      # Tabpages
      # ----------
      
      # Tabpanel WBE
      species <- wbeServer(id = NULL, 
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
          if (input$tabs %in% c("Publiek"))
            js$browseURL(paste0("https://faunabeheer.inbo.be/app/01_faunabeheer/?specie=", species$specie()))
        })
      
    })
