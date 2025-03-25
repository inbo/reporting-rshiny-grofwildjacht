# build all specie tabs
specieTabs <- lapply(species, function(specie){
  bslib::nav_panel(
    title = tools::toTitleCase(specie), 
    value = specie,
    specieUI(
      id = specie, 
      speciesList = schadeWildsoorten,
      categories = getInfo(specie = specie, variable = "category",
        infoOutput = infoOutput
      )
    )
  )
})

# build all category tabs
categoryTabs <- lapply(categories, function(category){

  bslib::nav_panel(
    title = getCategoryTitle(category),
    value = category, 
    categoryUI(
      category = category, id = category,
      ecoData = ecoData, schadeData = schadeData,
      uiText = uiText,
      speciesList = schadeWildsoorten
    )
  )
  
})

# build all subcategory tabs - contain placeholder for cards 
subcategoryTabs <- lapply(subcategories, function(subcategory){
   
  category <- strsplit(subcategory, split = "-")[[1]][1]

  args <- c(
    list(
      id = subcategory, category = category, 
      speciesList = schadeWildsoorten,
      select = TRUE
    ),
    if(category == "schade")
      list(
        schadeTypes = schadeTypes, gewasChoices = gewasChoices, 
        voertuigChoices = voertuigChoices
      )
  )
  
  bslib::nav_panel(
    title = getSubcategoryTitle(subcategory = subcategory, 
      uiText = uiText),   
    value = subcategory,
    do.call(outputUI, args)
  )
  
})

# build all output tabs - contain placeholder for plot/table and parameters
outputTabs <- lapply(outputs, function(output){

  category <- unique(infoOutput[which(infoOutput$output == output), "category"])
  title <- getOutputTitle(output = output, 
    uiText = uiText, n = 200, type = category)

  args <- c(
    list(
      id = output, category = category, select = TRUE,
      speciesList = schadeWildsoorten,
      whiteWell = TRUE
    ),
    if(category == "schade")
      list(
        schadeTypes = schadeTypes, 
        gewasChoices = gewasChoices, 
        voertuigChoices = voertuigChoices
      )
  )

  bslib::nav_panel(
    title = title,   
    value = output,
    do.call(outputUI, args)
  )

})

shinyUI(
        
  bootstrapPage(
      
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = js_code, functions = 'browseURL'),
                
    ## For debugging
    uiOutput("debug"),
                
    ## Header
    ## ------
                
    tags$head(
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
        frontUI(speciesList = schadeWildsoorten)
      ),
      
      do.call(bslib::nav_menu, 
        append(
          list(title = htmlOutput("specie", inline = TRUE)), 
          specieTabs
        )
      ),
      do.call(bslib::nav_menu,
        append(
          list(title = htmlOutput("category", inline = TRUE)), 
          categoryTabs
        )
      ),
      do.call(bslib::nav_menu,
        append(
          list(title = htmlOutput("subcategory", inline = TRUE)), 
          subcategoryTabs
        )
      ),
      do.call(bslib::nav_menu,
        append(
          list(title = htmlOutput("output", inline = TRUE)), 
          outputTabs
        )
      ),
      bslib::nav_item(selectizeInput(inputId = "search", label = NULL, choices = NULL)), 
      bslib::nav_spacer(), # right align next items
      bslib::nav_item(uiOutput("mailLink")),
      bslib::nav_item(
        tags$a(
          id = "WBE", 
          href = "https://wbe.inbo.be", 
          # target="_blank", 
          "WBE"
        )
      ),
      bslib::nav_item(versionUI(id = "public"))

    )

  )

)
