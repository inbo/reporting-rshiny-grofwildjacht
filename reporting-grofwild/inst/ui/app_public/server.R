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
  
  # Store current tab
  # use a generic 'current tab' (e.g. no specific 'category()')
  # because tab should also be updated even if e.g. category() does not change
  # if specified via the hash (e.g. go from plot tab -> category tab)
  currentTab <- reactiveVal("Home")
  
  # should next tab(s) be reset?
  resetNextTab <- reactiveVal(TRUE)
  
  # should tab be updated (if category/subcategory/plot updated)?
  updateTab <- reactiveVal(TRUE)
  
  observe(print(paste("Specie:", specie())))
  observe(print(paste("Category:", category())))
  observe(print(paste("Subcategory:", subcategory())))
  observe(print(paste("Output:", plot())))
  
  # Save selection 
  observeEvent(input$navbarID, {      
    print(paste("Update of navbar ID to:", input$navbarID))
    if (input$navbarID %in% species) {
      specie(input$navbarID)
    } else if (input$navbarID %in% categories) {
      category(input$navbarID)       
    } else if (input$navbarID %in% subcategories) {
      subcategory(input$navbarID)
    }else if (input$navbarID %in% outputs) {
      plot(input$navbarID)
    }
    currentTab(input$navbarID);updateTab(TRUE);resetNextTab(TRUE)
  })
  
  ## Specie
  
  # Save user choice
  observeEvent(input$specie, 
    if(isTruthy(input$specie)){
      print("Save specie")
      specie(input$specie)
      updateTab(TRUE);resetNextTab(TRUE)
    }
  )
  
  # Update tab title
  output$specie <- renderUI(specie())
  
  # Change tab
  observeEvent(specie(), {
    if(specie() != "Specie" && updateTab()){
      currentTab(specie())
      print(paste("Go to:", specie(), "page"))
    }
  })

  # Update page content
  categorySpecie <- reactive(
    specieServer(id = specie(), specie = specie)
  )
 
  # Go to a 'category' page if respective card is selected in the specie page
  observeEvent(categorySpecie()(), {
    if(isTruthy(categorySpecie()()) && categorySpecie()() != "Categorie"){
      print(paste("Update category to:", categorySpecie()()))
      category(categorySpecie()())
      updateTab(TRUE);resetNextTab(TRUE)
    }
  })
  
  ## Category
  
  # Display available category tabs for a specie
  observeEvent(specie(), {

    categoriesToShow <- getCategories(specie = specie()) 
    print(paste("Update category tabs for", specie()))
        
    categoriesToHide <- setdiff(categories, categoriesToShow)
    for(category in categoriesToHide)
      hideTab(inputId = "navbarID", target = category)
        
    for(category in categoriesToShow)
      showTab(inputId = "navbarID", target = category)
    
    # reset selected output
    if(resetNextTab())  category("Categorie")
        
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
    if(category() != "Categorie" && updateTab()){
      print(paste("Update current tab to:", category()))
      currentTab(category())
    }
  })

  # Update page content
  outputCategory <- reactive({
    if(currentTab() %in% categories){
      isolate({
        print(paste("Go to:", category(), "page"))
        categoryServer(id = category(), 
          specie = specie, category = category()
        )
      })
    }else reactiveVal()
  })
  
  # Go to 'output' page if respective output clicked on the category page
  observeEvent(outputCategory()(), {
    if(isTruthy(outputCategory()()) && outputCategory()() != "Subcategorie"){
      print(paste("Update subcategory to:", outputCategory()()))
      subcategory(outputCategory()())
      updateTab(TRUE)
    }
  })

  # Change tab
  observeEvent(subcategory(), {
    if(subcategory() != "Subcategorie" && updateTab()){
      print(paste("Update current tab to:", subcategory()))
      currentTab(subcategory())
    }
  })

  ## Subcategory

  # Display available subcategory tabs
  showAvailableSubcategoryTabs <- reactive(list(category(), specie()))
  observeEvent(showAvailableSubcategoryTabs(), {

    category <- if(category() == "Categorie"){
      getCategories(specie = specie())
    }else{category()}
    
    print(paste("Update subcategory tabs for", 
      capture.output(str(category, no.list = TRUE))))
    
    subcategoriesToShow <- getSubcategories(category = category)
        
    subcategoriesToHide <- setdiff(subcategories, subcategoriesToShow)
    for(subcategory in subcategoriesToHide)
      hideTab(inputId = "navbarID", target = subcategory)
       
    for(subcategory in subcategoriesToShow)
      showTab(inputId = "navbarID", target = subcategory)
    
    # reset selected subcategory
    if(resetNextTab())  subcategory("Subcategorie")
      
  })

  # Update tab title
  output$subcategory <- renderUI(
    ifelse(
      isTruthy(subcategory()) && subcategory() != "Subcategorie",
      getSubcategoryTitle(subcategory = subcategory(), uiText = uiText),
      "Subcategorie"
    )
  )
  
  # Update page content
  outputSubcategory <- reactive({
    if(currentTab() %in% subcategories){
      isolate({
        print(paste("Go to:", subcategory(), "page"))
        args <- list(
          id = subcategory(), 
          specie = specie,
          subcategory = subcategory
        )
        fct <- paste0(category(), "CardServer")
        do.call(fct, args)
      })
    }else reactiveVal()
  })

  # Go to 'output' page if respective output clicked on the category page
  observeEvent(outputSubcategory()(), {
    if(isTruthy(outputSubcategory()()) && outputSubcategory()() != "Visualisatie/Tabel"){
      print(paste("Update plot to:", outputSubcategory()()))
      plot(outputSubcategory()())
      updateTab(TRUE)
    }
  })
  
  ## Outputs (table/plot)
  
  # Display available output tabs
  showAvailableOutputTabs <- reactive(list(subcategory(), category(), specie()))
  observeEvent(showAvailableOutputTabs(), {
        
    args <- 
      # no specified subcategory, and ...
      if(subcategory() == "Subcategorie"){
        # no specified category
        category <- if(category() == "Categorie"){
          getCategories(specie = specie())
        # specified category
        }else{category()}
        list(category = category)
      # specified category/subcategory
      }else{
        list(subcategory = subcategory())
      }
    print(paste("Update output tabs for", 
      capture.output(str(args, no.list = TRUE))))
      
    outputsToShow <- do.call(getOutputs, args)
        
    outputsToHide <- setdiff(outputs, outputsToShow)
    for(output in outputsToHide)
      hideTab(inputId = "navbarID", target = output)
      
    for(output in outputsToShow)
      showTab(inputId = "navbarID", target = output)
        
    # reset selected output
    if(resetNextTab())  plot("Visualisatie/Tabel")
        
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
    if(plot() != "Visualisatie/Tabel" && updateTab()){
      print(paste("Update current tab to:", plot()))
      currentTab(plot())
    }
  })

  # reset category/subcategory
  observeEvent(plot(), {
        
    # in case plot selected from navigation bar and ...
    if(plot() != "Visualisatie/Tabel"){
      
      # ... respective subcategory not selected
      subcategoryOutput <- getSubcategoryOutput(plot())
      if(subcategory() != subcategoryOutput){
        print(paste("Reset subcategory to", subcategoryOutput))
        subcategory(subcategoryOutput)
        updateTab(FALSE);resetNextTab(FALSE)# only update shown nav
      }
      
      # ... respective category not selected
      categoryOutput <- getCategoryOutput(plot())    
      if(category() != categoryOutput){
        print(paste("Reset category to", categoryOutput))
        category(categoryOutput)
        updateTab(FALSE);resetNextTab(FALSE) # only update shown nav
      }
    
    }
      
  }, priority = 2)

  # Update page content
  outputSpecie <- reactive({
    if(currentTab() %in% outputs){
      isolate({
        categoryOutput <- getCategoryOutput(output = plot())  
        print(paste("Go to:", categoryOutput, plot(), "output page"))
        args <- list(
          id = plot(), 
          specie = specie,
          plot = plot
        )
        fct <- paste0(categoryOutput, "OutputServer")
        do.call(fct, args)
     })
    }else reactiveVal()
  })
  # Update specie in top bar if changed in the 'output' page
  observeEvent(outputSpecie()(), {
    if(isTruthy(outputSpecie()()) && !identical(outputSpecie()(), specie())){
      print(paste("Specie updated in the 'output' page:", outputSpecie()()))
      specie(outputSpecie()())
      updateTab(FALSE)
      resetNextTab(FALSE)
    }
  })

  ## Change tabs
  observe(print(paste("Current tab is:", currentTab())))
  observeEvent(currentTab(), {
    print(paste("Update tab to", currentTab()))
    updateTabsetPanel(session, "navbarID", selected = currentTab())    
  }, ignoreInit = TRUE)

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
      
      print("Update selected tab panel based on hash")
      
      newSelection <- strsplit(gsub("^#", "", currentHash), split = "/")[[1]]
      
      # update selection in navigation bar
      specie(ifelse(!is.na(newSelection[1]), newSelection[1], "Specie"))
      category(ifelse(!is.na(newSelection[2]), newSelection[2], "Categorie"))
      subcategory(ifelse(!is.na(newSelection[3]), newSelection[3], "Subcategorie"))
      plot(ifelse(!is.na(newSelection[4]), newSelection[4], "Visualisatie/Tabel"))
      
      # go to the selected page
      currentTab(tail(newSelection, 1))
      
      # no extra reset(s)
      resetNextTab(FALSE) 

    }
  }, priority = 1)
  
  # Update the hash based on the selected tabPanel
  observeEvent(input$navbarID, { 
    req(input$navbarID != "Home")
    print("Update hash")
    currentHash <- session$clientData$url_hash
    query <- createQueryString(selection(), page = input$navbarID)
    if (currentHash != query)
      updateQueryString(queryString = query, mode = "push", session)
  }, priority = 0)

})
