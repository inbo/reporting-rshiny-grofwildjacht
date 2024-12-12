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
    req(specie() != "Specie")
    print(paste("Update tab to", specie()))
    updateTabsetPanel(session, "navbarID", selected = specie())
  }, ignoreInit = TRUE)

  # Update page content
  categorySpecie <- reactive(
    specieServer(id = specie(), specie = specie)
  )
 
  # Go to a 'category' page if respective card is selected in the specie page
  observeEvent(categorySpecie()(), {
    if(isTruthy(categorySpecie()()) && categorySpecie()() != "Categorie"){
      print(paste("Update category to:", categorySpecie()()))
      category(categorySpecie()())
    }
  })
  
  ## Category
  
  # Display available category tabs for a specie
  observeEvent(specie(), {
        
    categoriesSpecie <- getCategories(specie = specie()) 
    print(paste("Update category tabs for", specie()))
        
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
    req(category() != "Categorie")
    print(paste("Update tab to", category()))    
    updateTabsetPanel(session, "navbarID", selected = category())
  }, ignoreInit = TRUE)

  ## Subcategory

  # Display available subcategory tabs
  observeEvent(category(), {
        
    if(category() != "Categorie"){
      
      print(paste("Update subcategory tabs for", category()))
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
      isTruthy(subcategory()) && subcategory() != "Subcategorie",
      getSubcategoryTitle(subcategory = subcategory()),
      "Subcategorie"
    )
  )
  
  # Update page content
  outputCategory <- reactive({
    if(isTruthy(subcategory()) && subcategory() != "Subcategorie"){
      print(paste("Go to:", category(), "page"))
      args <- list(
        id = subcategory(), 
        specie = specie,
        subcategory = subcategory
      )
      fct <- paste0(category(), "CardServer")
      do.call(fct, args)
    }else reactiveVal()
  })

  # Go to 'output' page if respective output clicked on the category page
  observeEvent(outputCategory()(), {
    if(isTruthy(outputCategory()()) && outputCategory()() != "Visualisatie/Tabel"){
      print(paste("Update plot to:", outputCategory()()))
      plot(outputCategory()())
    }else reactiveVal()
  })
  
  ## Outputs (table/plot)
  
  # Display available output tabs
  observeEvent(subcategory(), {
        
    if(subcategory() != "Subcategorie"){
          
      print(paste("Update output tabs for", subcategory()))
      outputsSubcategory <- getOutputs(subcategory = subcategory())    
          
      outputsToHide <- setdiff(outputs, outputsSubcategory)
      for(output in outputsToHide)
        hideTab(inputId = "navbarID", target = output)
        
      for(output in outputsSubcategory)
        showTab(inputId = "navbarID", target = output)
          
    }
        
    # reset selected output
    plot("Visualisatie/Tabel")
        
  })

  # Update tab title
  output$output <- renderUI(
    if(plot() %in% outputs){
      getOutputTitle(output = plot(), 
        uiText = uiText, n = 200, 
        type = getCategoryOutput(plot())
      )
    }else plot()
  )
  
  # Change tab
  observeEvent(plot(), {
    req(plot() != "Visualizatie/Tabel")
    print(paste("Update tab to", plot()))    
    updateTabsetPanel(session, "navbarID", selected = plot())
  }, ignoreInit = TRUE)

  # Update page content
  observe({
    if(isTruthy(plot()) && plot() != "Visualisatie/Tabel"){
      print(paste("Go to:", category(), plot(), "output page"))
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
  
  selection <- reactive(
    c(
      specie = specie(),
      category = category(), 
      subcategory = subcategory(),
      plot = plot()
    )
  )
  

  # Update the selected tabPanel based on the hash
  # (see https://stackoverflow.com/a/74874638)
  observeEvent(session$clientData$url_hash, {
      
    req(input$navbarID)
    
    currentHash <- utils::URLdecode(URL = session$clientData$url_hash)
    query <- createQueryString(selection(), page = input$navbarID)
      
    if (currentHash != query) {
      
      newSelection <- gsub("^#", "", strsplit(currentHash, split = "/")[[1]])
      
      specie(ifelse(!is.na(newSelection[1]), newSelection[1], "Specie"))
      category(ifelse(!is.na(newSelection[2]), newSelection[2], "Categorie"))
      subcategory(ifelse(!is.na(newSelection[3]), newSelection[3], "Subcategorie"))
      plot(ifelse(!is.na(newSelection[4]), newSelection[4], "Visualisatie/Tabel"))

    }
  }, priority = 1)
  
  # Update the hash based on the selected tabPanel
  observeEvent(input$navbarID, { 
    req(input$navbarID != "Home")
    currentHash <- session$clientData$url_hash
    query <- createQueryString(selection(), page = input$navbarID)
    if (currentHash != query)
      updateQueryString(queryString = query, mode = "push", session)
  }, priority = 0)

})
