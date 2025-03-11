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
  
  defaultTabs <- list(
    specie = "Diersoort",
    category = "Categorie",
    subcategory = "Subcategorie",
    plot = "Visualisatie/Tabel")
  
  specie <- reactiveVal(defaultTabs$specie)
  category <- reactiveVal(defaultTabs$category)
  subcategory <- reactiveVal(defaultTabs$subcategory)
  plot <- reactiveVal(defaultTabs$plot)
  
  # Store current tab
  # use a generic 'current tab' (e.g. no specific 'category()')
  # because tab should also be updated even if e.g. category() does not change
  # if specified via the hash (e.g. go from plot tab -> category tab)
  currentTab <- reactiveVal("Home")
  
  # should next tab(s) be reset?
  resetNextTab <- reactiveVal(TRUE)
  
  # should tab be updated (if category/subcategory/plot updated)?
  updateTab <- reactiveVal(TRUE)
  
  if(doDebug)
    observe(print(paste("Specie:", specie())))
  if(doDebug)
    observe(print(paste("Category:", category())))
  if(doDebug)
    observe(print(paste("Subcategory:", subcategory())))
  if(doDebug)
    observe(print(paste("Output:", plot())))
  
  # Save selection 
  observeEvent(input$navbarID, {
    if(doDebug)
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

  # list of current available categories, subcategories, outputs
  categoriesCur <- reactive(
    getInfo(specie = specie(), variable = "category",
      infoOutput = infoOutput, defaults = defaultTabs
    )
  )
  subcategoriesCur <- reactive(
    getInfo(specie = specie(), category = category(), variable = "subcategory",
      infoOutput = infoOutput, defaults = defaultTabs
    )
  )
  outputsCur <- reactive(
    getInfo(specie = specie(), 
      category = category(), subcategory = subcategory(), 
      variable = "output",
      infoOutput = infoOutput, defaults = defaultTabs
    )
  )
  
  ## Specie
  
  # Save user choice
  observeEvent(input$specie, 
    if(isTruthy(input$specie)){
      if(doDebug)  print("Save specie")
      specie(input$specie)
      updateTab(TRUE);resetNextTab(TRUE)
    }
  )
  
  # Update tab title
  output$specie <- renderUI(specie())
  
  # Change tab
  observeEvent(specie(), {
    if(specie() != defaultTabs$specie && updateTab()){
      currentTab(specie())
      if(doDebug)  
        print(paste("Update current tab to:", specie(), "page"))
    }
  })

  # Update page content
  categorySpecie <- reactive({
    if(currentTab() %in% species){
      isolate({
        if(doDebug)
          print(paste("Go to:", specie(), "page"))
        
        # Reset all next tabs
        category(defaultTabs$category)
        subcategory(defaultTabs$subcategory)
        plot(defaultTabs$plot)
        
        specieServer(id = specie(), specie = specie)
      })
    }else reactiveVal()
   })
 
  # Go to a 'category' page if respective card is selected in the specie page
  observeEvent(categorySpecie()(), {
    if(isTruthy(categorySpecie()()) && categorySpecie()() != defaultTabs$category){
      if(doDebug)
        print(paste("Update category to:", categorySpecie()()))
      category(categorySpecie()())
      updateTab(TRUE);resetNextTab(TRUE)
    }
  })
  
  ## Category
  
  # Display available category tabs for a specie
  observeEvent(categoriesCur(), {

    categoriesToShow <- categoriesCur()
    if(doDebug)
      print(paste("Update category tabs to", categoriesToShow))
        
    categoriesToHide <- setdiff(categories, categoriesToShow)
    for(category in categoriesToHide)
      hideTab(inputId = "navbarID", target = category)
        
    for(category in categoriesToShow)
      showTab(inputId = "navbarID", target = category)
    
    # reset selected output
    if(resetNextTab())  category(defaultTabs$category)
        
  })

  # Update tab title
  output$category <- renderUI({
    ifelse(
      isTruthy(category()),
      getCategoryTitle(category()),
      defaultTabs$category
    )
  })
  
  # Change tab
  observeEvent(category(), {
    if(category() != defaultTabs$category && updateTab()){
      if(doDebug)
        print(paste("Update current tab to:", category()))
      currentTab(category())
    }
  })

  # Update page content
  outputCategory <- reactive({
    if(currentTab() %in% categories){
      isolate({
        if(doDebug)
          print(paste("Go to:", category(), "page"))
        
        # Reset all next tabs
        subcategory(defaultTabs$subcategory)
        plot(defaultTabs$plot)
        
        categoryServer(
          id = category(), 
          specie = specie, category = category(),
          subcategories = subcategoriesCur(),
          uiText = uiText
        )
        
      })
    }else reactiveVal()
  })
  
  # Go to 'subcategory' page if respective subcategory clicked on the category page
  observeEvent(outputCategory()(), {
    if(isTruthy(outputCategory()()) && outputCategory()() != defaultTabs$subcategory){
      if(doDebug)
        print(paste("Update subcategory to:", outputCategory()()))
      subcategory(outputCategory()())
      updateTab(TRUE)
    }
  })

  # Change tab
  observeEvent(subcategory(), {
    if(subcategory() != defaultTabs$subcategory && updateTab()){
      if(doDebug)
        print(paste("Update current tab to:", subcategory()))
      currentTab(subcategory())
    }
  })

  ## Subcategory
  
  subcategorySpecie <- reactiveVal()

  # Display available subcategory tabs
  observeEvent(subcategoriesCur(), {
    
    if(doDebug)
      print("Update subcategory tabs")
    
    subcategoriesToShow <- subcategoriesCur()
        
    subcategoriesToHide <- setdiff(subcategories, subcategoriesToShow)
    for(subcategory in subcategoriesToHide)
      hideTab(inputId = "navbarID", target = subcategory)
       
    for(subcategory in subcategoriesToShow)
      showTab(inputId = "navbarID", target = subcategory)
    
    # reset selected subcategory
    if(resetNextTab())  subcategory(defaultTabs$subcategory)
      
  })

  # Update tab title
  output$subcategory <- renderUI(
    ifelse(
      isTruthy(subcategory()) && subcategory() != defaultTabs$subcategory,
      getSubcategoryTitle(subcategory = subcategory(), uiText = uiText),
      defaultTabs$subcategory
    )
  )
  
  # Update page content
  outputSubcategory <- reactive({
    if(currentTab() %in% subcategories){
      isolate({
        if(doDebug)
          print(paste("Go to:", subcategory(), "page"))
        
        # Reset all next tabs
        plot(defaultTabs$plot)
        
        args <- list(
          id = subcategory(), 
          specie = specie,
          subcategory = subcategory,
          outputs = outputsCur(),
          # general
          subcategories = subcategoriesCur(),
          uiText = uiText
        )
        fct <- paste0(category(), "CardServer")
        do.call(fct, args)
      })
    }else reactiveVal()
  })

  # Go to 'output' page if respective output clicked on the subcategory page
  observeEvent(outputSubcategory()(), {
    if(isTruthy(outputSubcategory()()) && outputSubcategory()() != defaultTabs$plot){
      if(doDebug)
        print(paste("Update plot to:", outputSubcategory()()))
      plot(outputSubcategory()())
      updateTab(TRUE)
    }
  })
  
  ## Outputs (table/plot)
  
  # Display available output tabs
  observeEvent(outputsCur(), {
  
    if(doDebug)
      print("Update output tabs")
      
    outputsToShow <- outputsCur()
        
    outputsToHide <- setdiff(outputs, outputsToShow)
    for(output in outputsToHide)
      hideTab(inputId = "navbarID", target = output)
      
    for(output in outputsToShow)
      showTab(inputId = "navbarID", target = output)
        
    # reset selected output
    if(resetNextTab())  plot(defaultTabs$plot)
        
  })

  # Update tab title
  output$output <- renderUI(
    if(plot() %in% outputs){
      getOutputTitle(
        output = plot(), 
        uiText = uiText, n = 200, 
        type = unique(subset(infoOutput, output == plot())$category)
      )
    }else plot()
  )
  
  # Change tab
  observeEvent(plot(), {
    if(plot() != defaultTabs$plot && updateTab()){
      if(doDebug)
        print(paste("Update current tab to:", plot()))
      currentTab(plot())
    }
  })

  # reset category/subcategory
  observeEvent(plot(), {
        
    # in case plot selected from navigation bar and ...
    if(plot() != defaultTabs$plot){
      
      # ... respective subcategory not selected
      subcategoryOutput <- getSubcategoryOutput(plot())
      if(subcategory() != subcategoryOutput){
        if(doDebug)
          print(paste("Reset subcategory to", subcategoryOutput))
        subcategory(subcategoryOutput)
        updateTab(FALSE);resetNextTab(FALSE)# only update shown nav
      }
      
      # ... respective category not selected
      categoryOutput <- getCategorySubcategory(subcategoryOutput) 
      if(category() != categoryOutput){
        if(doDebug)
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
        categoryOutput <- unique(as.character(subset(infoOutput, output == plot())$category))
        if(doDebug)
          print(paste("Go to:", categoryOutput, plot(), "output page"))
        args <- c(
          list(
            id = plot(), 
            specie = specie,
            plot = plot,
            uiText = uiText,
            outputs = outputsCur()
          ),
          switch(categoryOutput, 
            beheer = list(
              ecoData = ecoData, geoData = geoData, 
              openingstijdenData = openingstijdenData, 
              spatialData = spatialData, 
              biotoopData = biotoopData,
              defaultYear = defaultYear
            ),
            populatie = list(
              ecoData = ecoData, geoData = geoData
            ),
            schade = list(
              schadeData = schadeData, 
              spatialData = spatialData, 
              biotoopData = biotoopData, 
              defaultYear = defaultYear, 
              schadeTypes = schadeTypes, schadeCodes = schadeCodes
            ),
            verspreiding = list(
              ecoData = ecoData, geoData = geoData, 
              spatialData = spatialData,
              waarnemingenData = waarnemingenData,
              defaultYear = defaultYear 
            )
          )
        )
        fct <- paste0(categoryOutput, "OutputServer")
        do.call(fct, args)
     })
    }else reactiveVal()
  })
  # Update specie in top bar if changed in the 'output' page
  observeEvent(outputSpecie()(), {
    if(isTruthy(outputSpecie()()) && !identical(outputSpecie()(), specie())){
      if(doDebug)
        print(paste("Specie updated in the 'output' page:", outputSpecie()()))
      specie(outputSpecie()())
      updateTab(FALSE)
      resetNextTab(FALSE)
    }
  })

  ## Change tabs
  if(doDebug)
    observe(print(paste("Current tab is:", currentTab())))
  
  observeEvent(currentTab(), {
    if(doDebug)
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
      
      if(doDebug)
        print("Update selected tab panel based on hash")
      
      newSelection <- strsplit(gsub("^#", "", currentHash), split = "/")[[1]]
      
      # update selection in navigation bar
      specie(ifelse(!is.na(newSelection[1]), newSelection[1], defaultTabs$specie))
      category(ifelse(!is.na(newSelection[2]), newSelection[2], defaultTabs$category))
      subcategory(ifelse(!is.na(newSelection[3]), newSelection[3], defaultTabs$subcategory))
      plot(ifelse(!is.na(newSelection[4]), newSelection[4], defaultTabs$plot))
      
      # go to the selected page
      currentTab(tail(newSelection, 1))
      
      # no extra reset(s)
      resetNextTab(FALSE) 
      
      # tabs should not be updated
      updateTab(FALSE)

    }
  }, priority = 1)
  
  # Update the hash based on the selected tabPanel
  updateHash <- reactive(list(selection(), input$navbarID))
  observeEvent(updateHash(), { 
    req(input$navbarID != "Home")
    if(doDebug)
      print("Update hash")
    currentHash <- session$clientData$url_hash
    query <- createQueryString(selection(), page = input$navbarID)
    if (currentHash != query)
      updateQueryString(queryString = query, mode = "push", session)
  }, priority = 0)


  versionServer(id = "public")
  
  output$mailLink <- renderUI({
      
      tags$a(
          id = "contact", 
          href = paste("mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer web applicatie&body=Link:", 
            paste0("faunabeheer.inbo.be/", createQueryString(selection(), page = input$navbarID))), 
          target="_blank", "Contact"
        )
      
    })

})
