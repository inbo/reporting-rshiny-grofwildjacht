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
  specie <- species[1]
  categoryUI <- switch(category, 
    beheer = afschotUI(id = "beheer", specie = specie),
    schade = schadeUI(id = "schade", specie = specie),
    populatie = populatieUI(id = "populatie", specie = specie),
    verspreiding = verspreidingUI(id = "verspreiding", specie = specie)
  )    
  tabPanel(
    title = tools::toTitleCase(sub("-", " ", category)), 
    value = category,
    categoryUI
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
      
      navbarMenu(title = htmlOutput("subcategory", inline = TRUE),
        uiOutput("tabsSubcategories")
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
