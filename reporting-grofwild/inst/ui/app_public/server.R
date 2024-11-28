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
    specie = "Specie",
    category = "Categorie",
    subcategory = "Subcategorie",
    output = "Visualisatie/Tabel"
  )
  
  # Save selection 
  observeEvent(input$navbarID, {      
    print(paste("navbar ID is:", input$navbarID))
    if (input$navbarID %in% species) {
      selection$specie <- input$navbarID
    } else if (input$navbarID %in% categories) {
      selection$category <- input$navbarID        
    } else if (input$navbarID %in% subcategories) {
      selection$subcategory <- input$navbarID
    }else if (input$navbarID %in% outputs) {
      selection$output <- input$navbarID
    }
  })
  
  ## Specie
  
  # Save user choice
  observeEvent(input$specie, selection$specie <- input$specie)
  
  # Update tab title
  output$specie <- renderUI(selection$specie)
  
  # Change tab
  observeEvent(selection$specie, {
    updateTabsetPanel(session, "navbarID", selected = selection$specie)
  }, ignoreInit = TRUE)

  # Update page content
  observe({
    if(isTruthy(selection$specie)){
      category <- specieServer(id = selection$specie, specie = selection$specie)
      if(isTruthy(category()))
        selection$category <- category()
    }
   })
  observe(paste("Selected category is:", selection$category))
  
  ## Category
  
  # Display available category tabs for a specie
  observeEvent(selection$specie, {
        
    categoriesSpecie <- getCategories(specie = selection$specie)    
        
    categoriesToHide <- setdiff(categories, categoriesSpecie)
    for(category in categoriesToHide)
      hideTab(inputId = "navbarID", target = category)
        
    for(category in categoriesSpecie)
      showTab(inputId = "navbarID", target = category)
        
  })

  # Update tab title
  output$category <- renderUI(getCategoryTitle(selection$category))
  
  # Change tab
  observeEvent(selection$category, {
    updateTabsetPanel(session, "navbarID", selected = selection$category)
   }, ignoreInit = TRUE)

  ## Subcategory

  # Display available subcategory tabs
  observeEvent(selection$category, {
        
    if(selection$category != "Categorie"){
      
      subcategoriesCategory <- getSubcategories(category = selection$category)    
        
      categoriesToHide <- setdiff(subcategories, subcategoriesCategory)
      for(subcategory in categoriesToHide)
        hideTab(inputId = "navbarID", target = subcategory)
        
      for(subcategory in subcategoriesCategory)
        showTab(inputId = "navbarID", target = subcategory)
      
    }
    
    # reset selected subcategory
    selection$subcategory <- "Subcategorie"
      
  })

  # Update tab title
  output$subcategory <- renderUI(
    getSubcategoryTitle(subcategory = selection$subcategory)
  )
  
  # Update page content
  observe({
    if(isTruthy(selection$subcategory) && selection$subcategory != "Subcategorie"){
      args <- list(
        id = selection$subcategory, 
        specie = selection$specie,
        subcategory = selection$subcategory
      )
      fct <- paste0(selection$category, "CardServer")
      do.call(fct, args)
    }
  })
  
  ## Outputs (table/plot)
  
  # Display available output tabs
  observeEvent(selection$subcategory, {
        
    if(selection$output != "Subcategorie"){
          
      outputsSubcategory <- getOutputs(subcategory = selection$subcategory)    
          
      outputsToHide <- setdiff(outputs, outputsSubcategory)
      for(output in outputsToHide)
        hideTab(inputId = "navbarID", target = output)
        
      for(output in outputsSubcategory)
        showTab(inputId = "navbarID", target = output)
          
    }
        
    # reset selected ouput
    selection$output <- "Visualisatie/Tabel"
        
  })

  # Update tab title
  output$output <- renderUI(
    if(selection$output %in% outputs)
      getOutputTitle(output = selection$output, 
        uiText = uiText, n = 200, 
        type = getCategoryOutput(selection$output)
      )
  )

  # Update page content
  observe({
    if(isTruthy(selection$output) && selection$output != "Visualisatie/Tabel"){
      args <- list(
        id = selection$output, 
        specie = selection$specie,
        plot = selection$output
      )
      fct <- paste0(selection$category, "OutputServer")
      do.call(fct, args)
    }
  })

  ## Navigation

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
        if (!is.na(newSelection[4])) selection$output <- newSelection[4]
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

})
