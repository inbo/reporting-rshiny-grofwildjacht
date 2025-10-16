library(shiny)
library(dplyr)
library(readr)
library(htmltools)

# Function to load data ####
load_data <- function() {
  uiText <- read_csv2("../reporting-grofwild/inst/extdata/uiText.csv", show_col_types = FALSE)
  return(uiText)
}

# Define UI ####
ui <- fluidPage(
  titlePanel("Translation Editor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Item", "Select Item:",
                  choices = c("", unique(load_data()$plotFunction)), selected = ""),
      selectInput("Type", "Select Type:",
                  choices = c("Public", "Private", "Summary"), selected = "Public"),
      actionButton("save_csv", "Save to CSV")
    ),
    
    mainPanel(
      textAreaInput("title_unformatted", "Title (Unformatted):",
                    width = "100%", height = "100px"),
      htmlOutput("title_rendered"),
      textAreaInput("description_unformatted", "Text (Unformatted):",
                    width = "100%", height = "200px"),
      htmlOutput("description_rendered")
    )
  )
)

# Define server logic ####
server <- function(input, output, session) {
  translations <- reactiveVal(load_data())
  
  # Update text areas when item is selected or type changes ####
  observeEvent({
    list(input$Item, input$Type)
  }, {
    req(input$Item != "")
    data <- translations()
    current_row <- data %>% filter(plotFunction == input$Item)
    
    if (nrow(current_row) > 0) {
      # Title is always the same field in 'title' column
      updateTextAreaInput(session, "title_unformatted",
                          value = current_row$title)
      
      # Description depends on selected type
      desc_val <- switch(
        input$Type,
        "Public" = current_row$description,
        "Private" = current_row$wbe,
        "Summary" = current_row$summary,
        ""
      )
      
      updateTextAreaInput(session, "description_unformatted",
                          value = ifelse(!is.na(desc_val), desc_val, ""))
    } else {
      updateTextAreaInput(session, "title_unformatted", value = "")
      updateTextAreaInput(session, "description_unformatted", value = "")
    }
  })
  
  # Render previews for title and text ####
  output$title_rendered <- renderUI({
    req(input$title_unformatted)
    HTML(input$title_unformatted)
  })
  
  output$description_rendered <- renderUI({
    req(input$description_unformatted)
    HTML(input$description_unformatted)
  })
  
  # Save updated data to CSV ####
  observeEvent(input$save_csv, {
    data <- translations()
    req(input$Item != "")
    
    # Update title field (always same column)
    data[data$plotFunction == input$Item, "title"] <- input$title_unformatted
    
    # Choose correct column for description part
    target_col <- switch(
      input$Type,
      "Public" = "description",
      "Private" = "wbe",
      "Summary" = "summary"
    )
    data[data$plotFunction == input$Item, target_col] <- input$description_unformatted
    
    # Save updated data to CSV
    write_csv2(data, "../reporting-grofwild/inst/extdata/uiText.csv")
    
    showModal(modalDialog(
      title = "CSV Saved",
      paste("Changes for", input$Item, "have been saved to uiText.csv"),
      easyClose = TRUE,
      footer = NULL
    ))
    
    translations(data)
  })
}

# Run the app ####
shinyApp(ui = ui, server = server)
