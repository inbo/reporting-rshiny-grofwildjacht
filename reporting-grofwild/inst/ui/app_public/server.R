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
  # Next page to be navigate to
  nextPage <- reactiveVal(NULL)
  
  # Current specie
  specie <- reactiveVal()
        
  ## Create the page (UI side)
  observeEvent(page(), {
    switch(page(),
      home = {
        print("Open home page")
        output$page <- renderUI(frontUI())
      },
      specie = {
        print("Open specie page")
        output$page <- renderUI(specieUI(id = "specie", specie = input$wildsoort))
      },
      afschot = {
        print(paste("Open afschot page with specie:", specie()))
        output$page <- renderUI(afschotUI(id = "afschot", specie = specie()))
      },
      schade = {
        print(paste("Open schade page with specie:", specie()))
        output$page <- renderUI(schadeUI(id = "schade", specie = specie()))
      },
      `populatie indicatoren` = {
        print(paste("Open populatie page with specie:", specie()))
        output$page <- renderUI(populatieUI(id = "populatie", specie = specie()))
      },
      `verspreiding` = {
        print(paste("Open verspreiding page with specie:", specie()))
        output$page <- renderUI(verspreidingUI(id = "verspreiding", specie = specie()))
      }
    )
  })

  observeEvent(input$wildsoort, 
    if(isTruthy(input$wildsoort))	page("specie")
  )

  observe(print(paste("Page currently selected is:", page())))
  observe(print(paste("Specie currently selected is:", specie())))
  
  ## Update the page (server side)
  
  # Specie:
  nextPageSpecie <- specieServer(id = "specie")
  observeEvent(nextPageSpecie(), ignoreNULL = TRUE, {
    # re-direct to 'Home' or 'Category' page
    page(nextPageSpecie())
    # save specie selected in the app
    if(!is.null(attr(nextPageSpecie(), "specie")))
      specie(attr(nextPageSpecie(), "specie"))
  })

  # Category: afschot
  nextPageAfschot <- afschotServer(id = "afschot")
  # re-direct to 'Home' or 'Specie' page
  observeEvent(nextPageAfschot(), ignoreNULL = TRUE, page(nextPageAfschot()))

  # Category: schade
  nextPageSchade <- schadeServer(id = "schade")
  # re-direct to 'Home' or 'Specie' page
  observeEvent(nextPageSchade(), ignoreNULL = TRUE, page(nextPageSchade()))

  # Category: populatie indicatoren
  nextPagePopulatie <- populatieServer(id = "populatie")
  # re-direct to 'Home' or 'Specie' page
  observeEvent(nextPagePopulatie(), ignoreNULL = TRUE, page(nextPagePopulatie()))
  
  # Category: verspreiding
  nextPageVerspreiding <- verspreidingServer(id = "verspreiding")
  # re-direct to 'Home' or 'Specie' page
  observeEvent(nextPageVerspreiding(), ignoreNULL = TRUE, page(nextPageVerspreiding()))
  
})
