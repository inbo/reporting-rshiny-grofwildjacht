library(shiny)
library(dplyr)
library(readr)
library(htmltools)

# Load data function
load_data <- function() {
  read_csv2("../reporting-grofwild/inst/extdata/uiText.csv", show_col_types = FALSE)
}

# UI definition
ui <- fluidPage(
  titlePanel("Translation Editor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Item", "Select Item:",
                  choices = c("", unique(load_data()$plotFunction))),
      selectInput("Type", "Select Type:",
                  choices = c("Public", "Private", "Summary")),
      textInput("new_plotFunction", "New plotFunction name:"),
      actionButton("add_plotFunction", "Add New plotFunction"),
      actionButton("save_csv", "Save to CSV")
    ),
    mainPanel(
      textAreaInput("title_unformatted", "Title (Unformatted):", width = "100%", height = "100px"),
      htmlOutput("title_rendered"),
      textAreaInput("description_unformatted", "Text (Unformatted):", width = "100%", height = "200px"),
      htmlOutput("description_rendered")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  translations <- reactiveVal(load_data())
  
  # Update translation fields based on item and type
  observeEvent({ list(input$Item, input$Type) }, {
    req(input$Item != "")
    data <- translations()
    current_row <- data %>% filter(plotFunction == input$Item)
    if (nrow(current_row) > 0) {
      updateTextAreaInput(session, "title_unformatted",
                          value = current_row$title)
      desc_val <- switch(
        input$Type, "Public" = current_row$description,
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
  
  # Add new plotFunction
  observeEvent(input$add_plotFunction, {
    new_name <- trimws(input$new_plotFunction)
    req(new_name != "")
    data <- translations()
    if (new_name %in% data$plotFunction) {
      showNotification("This plotFunction already exists.", type = "error")
    } else {
      new_row <- tibble(
        plotFunction = new_name,
        title = "",
        description = "",
        wbe = "",
        summary = ""
      )
      updated_data <- bind_rows(data, new_row)
      translations(updated_data)
      updateSelectInput(session, "Item", choices = c("", updated_data$plotFunction), selected = new_name)
      updateTextInput(session, "new_plotFunction", value = "")
      showNotification(paste("Added new plotFunction:", new_name), type = "message")
    }
  })
  
  # Render preview for title
  output$title_rendered <- renderUI({
    req(input$title_unformatted)
    HTML(input$title_unformatted)
  })
  
  # Render preview for text
  output$description_rendered <- renderUI({
    req(input$description_unformatted)
    HTML(input$description_unformatted)
  })
  
  # Save data to CSV
  observeEvent(input$save_csv, {
    data <- translations()
    req(input$Item != "")
    data[data$plotFunction == input$Item, "title"] <- input$title_unformatted
    target_col <- switch(
      input$Type, "Public" = "description",
      "Private" = "wbe",
      "Summary" = "summary"
    )
    data[data$plotFunction == input$Item, target_col] <- input$description_unformatted
    write_csv2(data, "../reporting-grofwild/inst/extdata/uiText.csv")
    showModal(modalDialog(
      title = "CSV Saved",
      paste("Changes for", input$Item, "saved to uiText.csv"),
      easyClose = TRUE
    ))
    translations(data)
  })
}

# Run app
shinyApp(ui, server)
