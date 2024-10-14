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
        
       
        # Version
        # -------
        
#        versionServer(id = "public")
        
        
        
      # Tabpages
      # ----------
      
      output$carouselOutput <- slickR::renderSlickR({
        img <- system.file(
          "ui", "www", paste0("carousel-", 1:4, ".png"), 
          package = "reportingGrofwild"
        )
        slickR::slickR(
          obj = img, slideId = "carousel", slideType = 'img',
          width = "100%", padding = 1
        ) + 
          slickR::settings(dots = TRUE, arrows = FALSE, autoplay = TRUE)
      })
      
})
