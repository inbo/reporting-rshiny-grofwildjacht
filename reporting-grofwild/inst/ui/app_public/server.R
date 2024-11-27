# Project: inbo-grofwildjacht_git
# 
# Author: mvarewyck, lcougnaud
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

  ## Selection
  
  selection <- reactiveValues(
    specie = species[1],
    category = "Categorie",
    subcategory = "Subcategorie",
    plot = "Visualisatie"
  )
  
  observeEvent(input$specie, selection$specie <- input$specie)
  
  ## Navigation
  
  # Change tabs
  observeEvent(selection$specie, {
    updateTabsetPanel(session, "navbarID", selected = selection$specie)
  }, ignoreInit = TRUE)
  observeEvent(selection$category, {
    updateTabsetPanel(session, "navbarID", selected = selection$category)
  }, ignoreInit = TRUE)


  # Display available category tabs for a specie
  observeEvent(selection$specie, {
        
    categoriesSpecie <- getCategories(specie = selection$specie)    
    
    categoriesToHide <- setdiff(categories, categoriesSpecie)
    for(category in categoriesToHide)
      hideTab(inputId = "navbarID", target = category)
    
    categoriesToShow <- categoriesSpecie
    for(category in categoriesToShow)
      showTab(inputId = "navbarID", target = category)

  })
  
  # Save selection 
  observeEvent(input$navbarID, {      
    if (input$navbarID %in% species) {
      selection$specie <- input$navbarID
    } else if (input$navbarID %in% categories) {
      selection$category <- input$navbarID        
    } else if (input$navbarID %in% subcategories) {
      selection$subcategory <- input$navbarID
    }
  })

  # Update title of tabs in the navigation bar
  output$specie <- renderUI(selection$specie)
  output$category <- renderUI(tools::toTitleCase(sub("-", " ", selection$category)))
  output$subcategory <- renderUI(selection$subcategory)

  # Update the selected tabPanel based on the hash
  # (see https://stackoverflow.com/a/74874638)
  observeEvent(session$clientData$url_hash, {
      
    req(input$navbarID)
      currentHash <- utils::URLdecode(session$clientData$url_hash)
      if (currentHash != createQueryString(selection, page = input$navbarID)) {
        newSelection <- gsub("^#", "", strsplit(currentHash, split = "/")[[1]])
        if (!is.na(newSelection[1])) selection$specie <- newSelection[1]
        if (!is.na(newSelection[2])) selection$category <- newSelection[2]
        if (!is.na(newSelection[3])) selection$subcategory <- newSelection[3]
        if (!is.na(newSelection[4])) selection$plot <- newSelection[4]
      }
    }, 
    priority = 1
  )
  
  # Update the hash based on the selected tabPanel
  observeEvent(input$navbarID, {
        
    req(input$navbarID != "Home")
    currentHash <- session$clientData$url_hash
    pushQueryString <- createQueryString(
      selection = selection, page = input$navbarID
    )
    if (currentHash != pushQueryString){
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)  

  # Page content

  observeEvent(selection$specie, {
    category <- specieServer(id = selection$specie, specie = selection$specie)
    selection$category <- category
  })

  observeEvent(selection$category, {
    switch(selection$category,
      beheer = 
        afschotServer(id = "beheer", specie = selection$specie),
      schade = 
        schadeServer(id = "schade", specie = selection$specie),
      `populatie-indicatoren` = 
        populatieServer(id = "populatie", specie = selection$specie),
      verspreiding = 
        verspreidingServer(id = "verspreiding", specie = selection$specie)
    )
  })

})
