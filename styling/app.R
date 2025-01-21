# Species page cards
# 
# Author: mvarewyck
###############################################################################

## UI side
ui <- function() {
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", href = "www/style.css")
    ),
    
    tags$div(style = "margin-left: 15px; margin-top: 15px", sidebarLayout(    
      
      sidebarPanel(width = 3, "Wild zwijn"),
      mainPanel(width = 9, uiOutput("items"))
  
  ))

)
  
}


## Server side
server <- function(input, output, session) {
  
  values <- c(
    "beheer",
    "schade",
    "populatie",
    "verspreiding", "draagvlak",
    "woordenlijst"
  )
  
  names <- lapply(values, function(type){
      foto <- paste0("specie-", gsub("[[:blank:]]", "-", type), ".png")
      title <- ifelse(type == "afschot", "beheer", type)
      title <- toupper(title)
      HTML(paste0(
          "<div class='radio-tiles-title'>", title, "</div>",
          "<div>", img(src = paste0("www/", foto), width = "100%"), "</div>"
        ))
    })
  
  
  
  output$items <- renderUI({
      
      tags$div(style = "margin-top: -20px;",
        radioButtons(
          inputId = "cards", label = "", inline = TRUE,
          choiceValues = values, choiceNames = names,
          selected = character(0)
        ),
        tags$script("$('.radio-inline').addClass('radio-tiles');")
      )
      
    })
  
}

shinyApp(ui, server)
