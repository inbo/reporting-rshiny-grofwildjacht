# build all specie tabs
specieTabs <- lapply(species, function(specie){
  bslib::nav_panel(
    title = tools::toTitleCase(specie), 
    value = specie,
    specieUI(
      id = specie, 
      speciesList = groupSpecies(allSpecies = allWildsoorten)
    )
  )
})

# build all category tabs
categoryTabs <- lapply(categories, function(category){
    
  speciesList <- groupSpecies(allSpecies = allWildsoorten,
    selectedSpecies = getInfo(category = category, variable = "specie", 
      infoOutput = infoOutput))
    
  bslib::nav_panel(
    title = getCategoryTitle(category),
    value = category, 
    categoryUI(
      category = category, id = category,
      ecoData = ecoData, schadeData = schadeData,
      uiText = uiText,
      speciesList = speciesList
    )
  )
  
})

# build all subcategory tabs - contain placeholder for cards 
subcategoryTabs <- unlist(lapply(categories, function(category) {
    
    subcategoriesTmp <- subcategories[startsWith(subcategories, category)]
    
    # Create nav_panels for each subcategory in this category
    subcategory_panels <- lapply(subcategoriesTmp, function(subcategory) {

        speciesList <- groupSpecies(
          allSpecies = allWildsoorten,
          selectedSpecies = getInfo(subcategory = subcategory, variable = "specie",
            infoOutput = infoOutput)
        )
        
        args <- list(
          id = subcategory, 
          category = category,
          uiText = uiText,
          speciesList = speciesList,
          select = TRUE
        )
        
        bslib::nav_panel(
          title = HTML(paste0("&nbsp;&nbsp;", getSubcategoryTitle(subcategory = subcategory, uiText = uiText))),
          value = subcategory,
          do.call(outputUI, args)
        )
      })
    
    subcategory_panels <- append(
      list(bslib::nav_panel(
        title = tags$span(
          class = "custom-tab-title",
          getCategoryTitle(category)
        ),
        value = category,
        NULL
      )),
      subcategory_panels
      )
      
      subcategory_panels
    
  }), recursive = FALSE)



# build all output tabs - contain placeholder for plot/table and parameters
outputTabs <- unlist(lapply(subcategories, function(subcategory) {
      
      outputsTmp <- unique(infoOutput[which(infoOutput$subcategory == subcategory), "output"])
      
      # Create nav_panels for each subcategory in this category
      output_panels <- lapply(outputsTmp, function(output) {
          
          category <- unique(infoOutput[which(infoOutput$output == output), "category"])
          title <- getOutputTitle(output = output, 
            uiText = uiText, n = 200, type = category)
          speciesList <- groupSpecies(allSpecies = allWildsoorten,
            selectedSpecies = getInfo(output = output, variable = "specie", 
              infoOutput = infoOutput))
          
          args <- list(
            id = output, category = category, select = TRUE,
            speciesList = speciesList,
            schadeSelection = category == "schade",
            whiteWell = TRUE
          )
          
          bslib::nav_panel(
            title = HTML(paste0("&nbsp;&nbsp;", title)),   
            value = output,
            do.call(outputUI, args)
          )
        })
      
      output_panels <- append(
        list(bslib::nav_panel(
            title = tags$span(
              class = "custom-tab-title",
              getSubcategoryTitle(subcategory = subcategory, uiText = uiText)
            ),
            value = subcategory,
            NULL
          )),
        output_panels
      )
      
      output_panels
      
    }), recursive = FALSE)
    

shinyUI(
        
  bootstrapPage(
      
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = js_code, functions = 'browseURL'),
                
    ## For debugging
    uiOutput("debug"),
                
    ## Header
    ## ------
                
    tags$head(
      tags$style(HTML('
            #search + .selectize-control .selectize-input.has-items:after,
						#search + .selectize-control .selectize-input:after {
            content: "\\f002" !important; /* Font Awesome search icon */
            font-family: "Font Awesome 5 Free" !important;
            font-weight: 900 !important;
            border: none !important;
            background: none !important;
            width: auto !important;
            height: auto !important;
            right: 10px !important;
            top: 50% !important;
            transform: translateY(-50%) !important;
            cursor: pointer !important;
            }
            ')),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
#    tags$meta(charset = "utf-8"),
#    tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
      tags$link(rel = "stylesheet",
          href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
          integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
          crossorigin="anonymous"
      ),
      tags$link(rel = "stylesheet", href = "www/style.css")
    ),
                
    ## Body
    ## ------
    bslib::navset_tab(
        
      id = "navbarID",
      
      bslib::nav_panel(title = "Home", 
        frontUI(speciesList = groupSpecies(allSpecies = allWildsoorten), uiText = uiText)
      ),
      bslib::nav_item( tags$span(">", style = "color: #ffffff; font-size: 12; font-weight: bold; padding: 0 10px;")),
      
      do.call(bslib::nav_menu, 
        append(
          list(title = htmlOutput("specie", inline = TRUE)), 
          specieTabs
        )
      ),
      bslib::nav_item( tags$span(">", style = "color: #ffffff; font-size: 12; font-weight: bold; padding: 0 10px;")),
      do.call(bslib::nav_menu,
        append(
          list(title = htmlOutput("category", inline = TRUE)), 
          categoryTabs
        )
      ),
      bslib::nav_item( tags$span(">", style = "color: #ffffff; font-size: 12; font-weight: bold; padding: 0 10px;")),
      do.call(bslib::nav_menu,
        append(
          list(title = htmlOutput("subcategory", inline = TRUE)), 
          subcategoryTabs
        )
      ),
      bslib::nav_item( tags$span(">", style = "color: #ffffff; font-size: 12; font-weight: bold; padding: 0 10px;")),
      do.call(bslib::nav_menu,
        append(
          list(title = htmlOutput("output", inline = TRUE)), 
          outputTabs
        )
      ),
      bslib::nav_item(selectizeInput(inputId = "search", label = NULL, choices = NULL, width = "100%")), 
      bslib::nav_spacer(), # right align next items
      bslib::nav_item(uiOutput("mailLink")),
      bslib::nav_item(
        tags$a(
          id = "WBE", 
          href = "https://wbe.inbo.be", 
          target="_parent", 
          "WBE"
        )
      ),
      bslib::nav_item(versionUI(id = "public"))

    )

  )

)
