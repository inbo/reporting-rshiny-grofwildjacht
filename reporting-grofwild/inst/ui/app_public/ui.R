# build all specie tabs
specieTabs <- lapply(species, function(specie){
  tabPanel(
    title = tools::toTitleCase(specie), 
    value = specie,
    specieUI(id = specie, specie = specie)
  )
})

# build all category tabs
categoryTabs <- lapply(categories, function(category){
      
  dataDate <- if(category == "schade"){
    schadeData
  }else{ecoData}
  maxDate <- max(dataDate$afschot_datum, na.rm = TRUE)
  
  infoText <- welcomeSectionUI(
    id = category, uiText = uiText,
    category = category,
    context = "description",
    maxDate = maxDate
  )
 
  img <- file.path("www", paste("category", category, "header.png", sep = "-"))   
      
  tabPanel(
    title = getCategoryTitle(category),
    value = category, 
    if(file.exists(img))  fluidRow(img(src = img, width = "100%")),
    fluidRow(column(10, offset = 1, infoText))
  )
  
})

# build all subcategory tabs - contain placeholder for cards 
subcategoryTabs <- lapply(subcategories, function(subcategory){
   
  category <- strsplit(subcategory, split = "-")[[1]][1]

  tabPanel(
    title = getSubcategoryTitle(subcategory = subcategory),   
    value = subcategory,
    outputUI(id = subcategory, category = category)
  )
  
})

# build all output tabs - contain placeholder for plot/table and parameters
outputTabs <- lapply(outputs, function(output){
      
  category <- getCategoryOutput(output)
  title <- getOutputTitle(output = output, 
    uiText = uiText, n = 200, type = category)

  tabPanel(
    title = title,   
    value = output,
    outputUI(id = output, category = category)
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
    navbarPage(
        
      title = "", id = "navbarID",
      
      tabPanel(title = "Home", frontUI()),
      
      do.call(navbarMenu, 
        append(
          list(title = htmlOutput("specie", inline = TRUE)), 
          specieTabs
        )
      ),
      do.call(navbarMenu,
        append(
          list(title = htmlOutput("category", inline = TRUE)), 
          categoryTabs
        )
      ),
      do.call(navbarMenu,
        append(
          list(title = htmlOutput("subcategory", inline = TRUE)), 
          subcategoryTabs
        )
      ),
      do.call(navbarMenu,
        append(
          list(title = htmlOutput("output", inline = TRUE)), 
          outputTabs
        )
      ),
      
      tabPanel(title = 
        tags$a(
          id = "contact", 
          href="mailto:faunabeheer@inbo.be?SUBJECT=Faunabeheer WBE web applicatie", 
          target="_blank", "Contact"
        )
      ),
      tabPanel(title =
        shiny::actionLink(
          inputId = "WBE", 
          label = "WBE", 
          onclick = "window.open('https://wbe.inbo.be', '_self')"
        )
      ),
      tabPanel(title = versionUI(id = "public"))

    )

  )

)
