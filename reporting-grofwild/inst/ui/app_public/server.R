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
  
  specie <- reactiveVal("Specie")
  category <- reactiveVal("Categorie")
  subcategory <- reactiveVal("Subcategorie")
  plot <- reactiveVal("Visualisatie/Tabel")
  
  observe(print(paste("Specie:", specie())))
  observe(print(paste("Category:", category())))
  observe(print(paste("Subcategory:", subcategory())))
  observe(print(paste("Output:", plot())))
  
  # Save selection 
  observeEvent(input$navbarID, {      
    print(paste("navbar ID is:", input$navbarID))
    if (input$navbarID %in% species) {
      specie(input$navbarID)
    } else if (input$navbarID %in% categories) {
      category(input$navbarID)       
    } else if (input$navbarID %in% subcategories) {
      subcategory(input$navbarID)
    }else if (input$navbarID %in% outputs) {
      plot(input$navbarID)
    }
  })
  
  ## Specie
  
  # Save user choice
  observeEvent(input$specie, 
    if(isTruthy(input$specie))
      specie(input$specie)
  )
  
  # Update tab title
  output$specie <- renderUI(specie())
  
  # Change tab
  observeEvent(specie(), {
    updateTabsetPanel(session, "navbarID", selected = specie())
  }, ignoreInit = TRUE)

  # Update page content
  observe({
    category <- specieServer(id = specie(), specie = specie)
  })
  
  ## Category
  
  # Display available category tabs for a specie
  observeEvent(specie(), {
        
    categoriesSpecie <- getCategories(specie = specie())    
        
    categoriesToHide <- setdiff(categories, categoriesSpecie)
    for(category in categoriesToHide)
      hideTab(inputId = "navbarID", target = category)
        
    for(category in categoriesSpecie)
      showTab(inputId = "navbarID", target = category)
        
  })

  # Update tab title
  output$category <- renderUI(
    ifelse(
      isTruthy(category()),
      getCategoryTitle(category()),
      "Categorie"
    )
  )
  
  # Change tab
  observeEvent(category(), {
    updateTabsetPanel(session, "navbarID", selected = category())
  }, ignoreInit = TRUE)

  ## Subcategory

  # Display available subcategory tabs
  observeEvent(category(), {
        
    if(category() != "Categorie"){
      
      subcategoriesCategory <- getSubcategories(category = category())    
        
      categoriesToHide <- setdiff(subcategories, subcategoriesCategory)
      for(subcategory in categoriesToHide)
        hideTab(inputId = "navbarID", target = subcategory)
        
      for(subcategory in subcategoriesCategory)
        showTab(inputId = "navbarID", target = subcategory)
      
    }
    
    # reset selected subcategory
    subcategory("Subcategorie")
      
  })

  # Update tab title
  output$subcategory <- renderUI(
    ifelse(
      isTruthy(subcategory()),
      getSubcategoryTitle(subcategory = subcategory()),
      "Subcategorie"
    )
  )
  
  # Update page content
  observe({
    if(isTruthy(subcategory()) && subcategory() != "Subcategorie"){
      print(paste("Update page content for category: ", category()))
      args <- list(
        id = subcategory(), 
        specie = specie,
        subcategory = subcategory
      )
      fct <- paste0(category(), "CardServer")
      do.call(fct, args)
    }
  })
  
  ## Outputs (table/plot)
  
  # Display available output tabs
  observeEvent(subcategory(), {
        
    if(plot() != "Subcategorie"){
          
      outputsSubcategory <- getOutputs(subcategory = subcategory())    
          
      outputsToHide <- setdiff(outputs, outputsSubcategory)
      for(output in outputsToHide)
        hideTab(inputId = "navbarID", target = output)
        
      for(output in outputsSubcategory)
        showTab(inputId = "navbarID", target = output)
          
    }
        
    # reset selected ouput
    plot("Visualisatie/Tabel")
        
  })

  # Update tab title
  output$output <- renderUI(
    if(plot() %in% outputs)
      getOutputTitle(output = plot(), 
        uiText = uiText, n = 200, 
        type = getCategoryOutput(plot())
      )
  )

  # Update page content
  observe({
    if(isTruthy(plot()) && plot() != "Visualisatie/Tabel"){
      args <- list(
        id = plot(), 
        specie = specie,
        plot = plot
      )
      fct <- paste0(category(), "OutputServer")
      do.call(fct, args)
    }
  })

  ## Navigation
#
#  # Update the selected tabPanel based on the hash
#  # (see https://stackoverflow.com/a/74874638)
#  observeEvent(session$clientData$url_hash, {
#      
#    req(input$navbarID)
#      currentHash <- utils::URLdecode(session$clientData$url_hash)
# TODO
#      if (currentHash != createQueryString(selection, page = input$navbarID)) {
#        newSelection <- gsub("^#", "", strsplit(currentHash, split = "/")[[1]])
#        if (!is.na(newSelection[1])) specie(newSelection[1])
#        if (!is.na(newSelection[2])) category(newSelection[2])
#        if (!is.na(newSelection[3])) subcategory(newSelection[3])
#        if (!is.na(newSelection[4])) plot(newSelection[4])
#      }
#    }, 
#    priority = 1
#  )
#  
#  # Update the hash based on the selected tabPanel
#  observeEvent(input$navbarID, {
#        
#    req(input$navbarID != "Home")
#    currentHash <- session$clientData$url_hash
#    pushQueryString <- createQueryString(
#      selection = selection, page = input$navbarID
#    )
#    if (currentHash != pushQueryString){
#      updateQueryString(pushQueryString, mode = "push", session)
#    }
#  }, priority = 0)

})
