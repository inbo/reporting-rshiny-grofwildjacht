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
  
  schade_code <- reactiveVal(NULL)
  schade_gewas <- reactiveVal(NULL)
  schade_voertuig <- reactiveVal(NULL)
  
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
  observeEvent(input$navbarID, ignoreInit = TRUE, {
    if(doDebug)
      print(paste("Update of navbar ID to:", input$navbarID))
    if (input$navbarID %in% species) {
      specie(input$navbarID)
      currentTab(input$navbarID)
    } else if (input$navbarID %in% categories) {
      category(input$navbarID)  
      currentTab(input$navbarID)
    } else if (input$navbarID %in% subcategories) {
      subcategory(input$navbarID)
      currentTab(input$navbarID)
    } else if (input$navbarID %in% outputs) {
      plot(input$navbarID)
      currentTab(getSubcategoryOutput(input$navbarID))
    }
    updateTab(TRUE);resetNextTab(TRUE)
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
    output$specie <- renderUI({
        
        if (isTruthy(specie()) && specie() != defaultTabs$specie) {
          HTML(sprintf('
                <span style="display: inline-block; line-height: 1;">
                <span style="font-size: smaller; font-style: italic; display: block; color: #ffffff;">Diersoort:</span>
                <span style="display: block; color: #ffffff;">%s</span>
                </span>
                ', specie()))
        } else {
          defaultTabs$specie
        }
        
      })

  
  # Change tab
  observeEvent(specie(), {
      if(specie() != defaultTabs$specie && updateTab()){
        if(doDebug)
          print(paste("Update current tab to:", specie()))
        currentTab(specie())
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
        
        specieServer(id = specie(), specie = specie, 
          categories = getInfo(specie = specie(), variable = "category",
            infoOutput = infoOutput)
        )
        
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
  
  # Update tab title
  output$category <- renderUI({
      
      if (isTruthy(category()) && category() != defaultTabs$category) {
        HTML(sprintf('
              <span style="display: inline-block; line-height: 1;">
              <span style="font-size: smaller; font-style: italic; display: block; color: #ffffff;">Categorie:</span>
              <span style="display: block; color: #ffffff;">%s</span>
              </span>
              ', getCategoryTitle(category())))
      } else {
        defaultTabs$category
      }
      
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
      plot(defaultTabs$plot)
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
  
  # Update tab title
  output$subcategory <- renderUI({
      
      if (isTruthy(subcategory()) && subcategory() != defaultTabs$subcategory) {
        HTML(sprintf('
            <span style="display: inline-block; line-height: 1;">
            <span style="font-size: smaller; font-style: italic; display: block; color: #ffffff;">Subcategorie:</span>
            <span style="display: block; color: #ffffff;">%s</span>
            </span>
            ', getSubcategoryTitle(subcategory = subcategory(), uiText = uiText)))
      } else {
        defaultTabs$subcategory
      }
      
    })
  

# reset category
observeEvent(subcategory(), {
    
    
    # Reset schade defaults when subcategory changes
    schade_code(NULL)
    schade_gewas(NULL)
    schade_voertuig(NULL)
    
    
    # in case subcategory selected from navigation bar and ...
    if(subcategory() != defaultTabs$subcategory){
      
      # ... selected plot is not from this subcategory
      if (plot() != defaultTabs$plot) {
        subcategoryOutput <- getSubcategoryOutput(plot())
        if(subcategory() != subcategoryOutput){
          if(doDebug)
            print("Reset plot to default")
          plot(defaultTabs$plot)
          updateTab(FALSE);resetNextTab(FALSE)# only update shown nav
        }
      }
      
      # ... respective category not selected
      categoryOutput <- as.character(getCategorySubcategory(subcategory())) 
      if(category() != categoryOutput){
        if(doDebug)
          print(paste("Reset category to", categoryOutput))
        category(categoryOutput)
        plot(defaultTabs$plot)
        updateTab(FALSE);resetNextTab(FALSE) # only update shown nav
      }
      
    }
    
  }, priority = 2)

  
  ## Outputs (table/plot)
  
  # Update tab title

    output$output <- renderUI({
        
        plotName <- if(plot() %in% outputs){
            getOutputTitle(
              output = plot(), 
              uiText = uiText, n = 200, 
              type = unique(subset(infoOutput, output == plot())$category)
            )
          }else plot()
        
        if (isTruthy(plot()) && plot() != defaultTabs$plot) {
          HTML(sprintf('
                <span style="display: inline-block; line-height: 1;">
                <span style="font-size: smaller; font-style: italic; display: block; color: #ffffff;">Visualisatie:</span>
                <span style="display: block; color: #ffffff;">%s</span>
                </span>
                ', plotName))
        } else {
          defaultTabs$plot
        }
        
      })
  
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
        
    # Reset schade defaults when plot changes
    schade_code(NULL)
    schade_gewas(NULL)
    schade_voertuig(NULL)
    
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
  outputSelection <- reactive({
    
      specie()
      if (currentTab() %in% subcategories) {
      isolate({
          categoryOutput <- getInfo(subcategory = subcategory(), infoOutput = infoOutput, variable = "category")
        if(doDebug)
          print(paste("Go to:", categoryOutput, subcategory(), "output page"))
        args <- c(
          list(
            id = subcategory(), 
            specie = specie,
            uiText = uiText,
            subcategory = subcategory,
            subcategories = subcategoriesCur(),
            outputs = outputsCur(),
            plot = plot,
            defaultTabs = defaultTabs
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
            schade = {
              list(
                schadeData = schadeData, 
                spatialData = spatialData, 
                biotoopData = biotoopData, 
                defaultYear = defaultYear, 
                schadeTypes = schadeTypes, schadeCodes = schadeCodes,
                # isolate - prevent loop if changed too quickly in plot
                schade_code = reactive(isolate(schade_code())),
                schade_gewas = reactive(isolate(schade_gewas())),
                schade_voertuig = reactive(isolate(schade_voertuig()))
              )
            },
            verspreiding = list(
              ecoData = ecoData, geoData = geoData, 
              spatialData = spatialData,
              waarnemingenData = waarnemingenData,
              biotoopData = biotoopData,
              defaultYear = defaultYear 
            ),
            draagvlak = list(
              draagvlakData = draagvlakData
            ),
            links = list()
          )
        )
        fct <- paste0(categoryOutput, "OutputServer")
        do.call(fct, args)
     })
     
     }
    
  })
  # Update specie in top bar if changed in the 'output' page
  observe({
      
      req(outputSelection())
      
      if (!is.null(outputSelection()$specie()) && !identical(outputSelection()$specie(), specie())) {
      
        if(doDebug)
          print(paste("Specie updated in the 'output' page:", outputSelection()$specie()))
        
        specie(outputSelection()$specie())
        updateTab(TRUE)
        resetNextTab(FALSE)
        
      }
      
      # optional
      req("schade_code" %in% names(outputSelection()))
      if (doDebug)
        print(paste("Update schade settings on output page", 
            paste(outputSelection()$schade_code(), collapse = ", ")))
      
      if (!setequal(outputSelection()$schade_code(), isolate(schade_code())))
        schade_code(outputSelection()$schade_code())
      if (!setequal(outputSelection()$schade_gewas(), isolate(schade_gewas())))
        schade_gewas(outputSelection()$schade_gewas())
      if (!setequal(outputSelection()$schade_voertuig(), isolate(schade_voertuig())))
        schade_voertuig(outputSelection()$schade_voertuig())
            
  })

  ## Change tabs
  observeEvent(currentTab(), {
    if(doDebug)
      print(paste("Update tab to", currentTab()))
    updateTabsetPanel(session, "navbarID", selected = currentTab())    
  }, ignoreInit = TRUE, priority = 10)

  ## Navigation
  
  selection <- reactive({
      # Check for relevant choices
      categoryChoices <- getInfo(specie = specie(), variable = "category", 
        infoOutput = infoOutput, defaults = defaultTabs
      )
      subcategoryChoices <- getInfo(specie = specie(), variable = "subcategory", 
        infoOutput = infoOutput, defaults = defaultTabs
      )
      outputChoices <- getInfo(specie = specie(), variable = "output", 
        infoOutput = infoOutput, defaults = defaultTabs
      )
    list(
      specie = specie(),
      category = if (as.character(category()) %in% categoriesCur()) 
          as.character(category()) else 
          defaultTabs$category, 
      subcategory = if (as.character(subcategory()) %in% subcategoriesCur()) 
          as.character(subcategory()) else 
          defaultTabs$subcategory,
      plot = if (plot() %in% outputsCur()) 
          plot() else 
          defaultTabs$plot
    )
  })
  
  # Reset navbarID choices based on species
  observeEvent(specie(), {
      
      # Reset choices navbar
      ## category
      currentCat <- getInfo(specie = specie(), variable = "category", 
        infoOutput = infoOutput, defaults = defaultTabs
      )
      resetNavbarChoices(allChoices = categories, 
        currentChoices = currentCat)
      ## subcategory
      currentSub <- getInfo(specie = specie(), variable = "subcategory", 
        infoOutput = infoOutput, defaults = defaultTabs
      )
      resetNavbarChoices(allChoices = subcategories, 
        currentChoices = currentSub)
      ## outputs
      currentPlots <- getInfo(specie = specie(), variable = "output", 
        infoOutput = infoOutput, defaults = defaultTabs
      )
      resetNavbarChoices(allChoices = outputs, 
        currentChoices = currentPlots)
      
      ## Reset selection based on available choices
      category(selection()$category)
      subcategory(selection()$subcategory)
      plot(selection()$plot)
      updateSelectInput(session, "specie", selected = specie())  
      
      if (doDebug)
        print("Update choices navbar")
      
    })

  # Update the selected tabPanel based on the hash
  # (see https://stackoverflow.com/a/74874638)
  observeEvent(session$clientData$url_search, {
      
    req(input$navbarID)
    
    currentHash <- session$clientData$url_search
    query <- createQueryString(selection(), page = input$navbarID, defaults = defaultTabs)
      
    if (!identical(parseQueryString(currentHash), parseQueryString(query))) {
      
      if(doDebug)
        print("Update selected tab panel based on hash")
      
      newSelection <- parseQueryString(session$clientData$url_search)
      
      # update selection in navigation bar
      specieTmp <- if (!is.null(newSelection[["gbifkey"]])) {
          speciesInfo <- read.csv(file.path(system.file("extdata", package = "reportingGrofwild"), "species-info.csv"))
          speciesInfo[match(newSelection[["gbifkey"]], speciesInfo$gbifkey), "species.name"]
        } else if (!is.null(newSelection[["specie"]])) {
          newSelection[["specie"]]
        } else {
          defaultTabs$specie
        }
      specie(specieTmp)
      category(ifelse(!is.null(newSelection[["category"]]), newSelection[["category"]], defaultTabs$category))
      subcategory(ifelse(!is.null(newSelection[["subcategory"]]), newSelection[["subcategory"]], defaultTabs$subcategory))
      plot(ifelse(!is.null(newSelection[["plot"]]), newSelection[["plot"]], defaultTabs$plot))
      
      # go to the selected page
      if (!is.null(plot()) && plot() != defaultTabs$plot) {
        currentTab(plot())
      } else if (!is.null(subcategory()) && subcategory() != defaultTabs$subcategory) {
        currentTab(subcategory())
      } else if (!is.null(category()) && category() != defaultTabs$category) {
        currentTab(category())
      } else if (!is.null(specie()) && specie() != defaultTabs$specie) {
        currentTab(specie())
      } else {
        currentTab("Home")
      }
      # no extra reset(s)
      resetNextTab(FALSE) 
      
      # tabs should not be updated
      updateTab(FALSE)

    }
  }, priority = -1)
  
  # Update the hash based on the selected tabPanel
  updateHash <- reactive(list(selection(), input$navbarID))
  observeEvent(updateHash(), { 
    req(input$navbarID != "Home")
    if(doDebug)
      print("Update hash")
    currentHash <- session$clientData$url_search
    query <- createQueryString(selection(), page = input$navbarID, defaults = defaultTabs)
    if (!identical(parseQueryString(currentHash), parseQueryString(query)))
      updateQueryString(queryString = query, mode = "push", session)
  }, priority = 0)


  versionServer(id = "public")
  
  output$mailLink <- renderUI({
      
      tags$a(
          id = "contact", 
          href = paste("mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer web applicatie&body=Link:", 
            paste0("faunabeheer.inbo.be/", createQueryString(selection(), page = input$navbarID, defaults = defaultTabs))), 
          target="_blank", "Contact"
        )
      
    })
  
  # List all choices in search navigation field
  observe({
      
      navigationChoices <- data.frame(
        value = c("", infoOutputList$id),
        label = c("", infoOutputList$label),
        html = c("", apply(infoOutputList[, c("category", "subcategory", "output")], 1, function(x) 
              paste(x[x != ""], collapse = " > "))),
        stringsAsFactors = FALSE
      )
      
      updateSelectizeInput(session, inputId = "search", choices = navigationChoices,
        server = TRUE,
        options = list(
          create = FALSE,
#          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
          onType = I("function (str) {if (str === \"\") {this.close();}}"),
          onItemAdd = I("function() {this.close();}"),
          placeholder = "Zoek in navigatie",
          render = I(
            "{
              option: function(item, escape) {
              return '<div class=\"long-selectize\">' + item.html + '</div>'; }
              }"
          ))
      )
      
    })
  
  # Selected via 'Search' box
  observeEvent(input$search, {
      
      req(input$search)
      
      if (input$search %in% categories) {
        category(input$search)
        subcategory(defaultTabs$subcategory)
        plot(defaultTabs$plot)
      } else if (input$search %in% subcategories) {
        category(getCategorySubcategory(input$search))
        subcategory(input$search)
        plot(defaultTabs$plot)
      } else {
        selectedSubCategory <- getSubcategoryOutput(input$search)
        subcategory(selectedSubCategory)
        category(getCategorySubcategory(selectedSubCategory))
        plot(input$search)
      }
      
      if(doDebug)
        print(paste("Search selection:", input$search))
 
      # go to the selected page
      currentTab(input$search)
      
      # no extra reset(s)
      resetNextTab(FALSE) 
      
      # tabs should not be updated
      updateTab(FALSE)
      
    })
  
  })
  