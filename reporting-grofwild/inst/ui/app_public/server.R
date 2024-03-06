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
        
       
        # Version
        # -------
        
        versionServer(id = "public")
        
        
        
      # Tabpages
      # ----------
      
      
      
      results <- reactiveValues(
        renderedTabs = "Grofwild")
      
      
    # Tabpanel grofwild
    source(list.files("serverFiles", pattern = "Grofwild", full.names = TRUE), local = TRUE)
    output$grof_content <- renderUI({
        
        source(file.path("uiFiles", "uiGrofwild.R"), local = TRUE)$value
        
      })
    
    # Render tabpanel upon need
    observeEvent(input$tabs, {
        
        # render only once
        req(!input$tabs %in% results$renderedTabs)
        
        switch(input$tabs,
          Wildschade = {
            
            output$schade_content <- renderUI({
                source(file.path("uiFiles", "uiWildschade.R"), local = TRUE)$value
              })
            source(list.files("serverFiles", pattern = "Wildschade", full.names = TRUE), local = TRUE)
            results$renderedTabs <- c(results$renderedTabs, "Wildschade")
            
          },
          Dashboard = {
            
            output$dash_content <- renderUI({
                source(file.path("uiFiles", "uiDashboard.R"), local = TRUE)$value
              })
            source(list.files("serverFiles", pattern = "Dashboard", full.names = TRUE), local = TRUE)
            results$renderedTabs <- c(results$renderedTabs, "Dashboard")
            
          },
          WBE = js$browseURL("https://wbe.inbo.be") 
        )
        
      })
    
      
    })
