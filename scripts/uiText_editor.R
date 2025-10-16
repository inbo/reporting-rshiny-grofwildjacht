library(shiny)
library(dplyr)
library(readr)
library(htmltools)
library(googlesheets4)

email <- Sys.getenv("email")

# append @inbo.be if missing ####
if(!grepl("@", email)) {
  email <- paste0(email, "@inbo.be")
}

gs4_auth(email)
sheet_id <- "1Rgkn1qEFpk7zAc_8QgL_bbXghlD09UzEMSGAIevq2rI" 

# Function to load data ####
load_data_initiate <- function() {
  translations <- read_csv2("../data/output/UAT_direct/translations.csv")
  
  write_sheet(translations, 
              sheet_id,
              sheet = "translations")
  
  return(translations)
}

# Reload data from googlesheet ####
load_data <- function() {
  
  # Replace with your actual Google Sheet ID ####
  translations <- read_sheet(sheet_id,
                             sheet = "translations")
  
  return(translations)
}


# Initial data load ####
load_csv <- askYesNo("Do you want to load the data from the CSV to the googlesheet?", title = "Data Load", yes = load_data_initiate, no = NULL)

if(load_csv){
  translations <- load_data_initiate()
} else {
  translations <- load_data()
}

# Define UI ####
ui <- fluidPage(
  titlePanel("Translation Editor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("title_id", "Select Title ID:", choices = c("", unique(translations$title_id)), selected = ""),
      selectInput("language", "Select Language:", choices = c("en", "fr", "nl")),
      actionButton("save", "Save to Google Sheet"),
      actionButton("save_csv", "Save to CSV")  # New button to save data to CSV
    ),
    
    mainPanel(
      textAreaInput("title_unformatted", "Title (Unformatted):", width = "100%", height = "100px"),
      htmlOutput("title_rendered"),
      textAreaInput("description_unformatted", "Description (Unformatted):", width = "100%", height = "200px"),
      htmlOutput("description_rendered")
    )
  )
)

# Define server logic ####
server <- function(input, output, session) {
  updated_data <- load_data()
  
  # Reactive value to store translations data ####
  translations_rv <- reactivePoll(
    intervalMillis = 10000,  # Check for updates every 5 seconds
    session = session,
    checkFunc = function() {
      showNotification("Checking for updates...", type = "message", duration = 3)
      
      if (nrow(updated_data) > 0) {
        showNotification("Data has been updated", type = "message", duration = 3)
        load_data()
      } else {
        return(FALSE)
      }
    },
    valueFunc = function() {
      showNotification("Data has been updated", type = "message", duration = 3)
      load_data()
    }
  )
  
  # Reactive values ####
  # to store the current selections of title_id and language
  current_title_id <- reactiveVal(NULL)
  current_language <- reactiveVal(NULL)
  
  # to store the local changes made by the user
  local_changes <- reactiveVal(list())
  
  # Update the dropdown choices for Title ID whenever data is reloaded
  observe({
    updateSelectInput(session, "title_id", 
                      choices = c("", unique(translations_rv()$title_id)),
                      selected = current_title_id() %||% "") # Use empty string if current_title_id is NULL
  })
  
  # Update the dropdown choices for Language and retain the current selection
  observe({
    updateSelectInput(session, "language", 
                      choices = c("en", "fr", "nl"),
                      selected = current_language()) # Retain current selection
  })
  
  # Update reactive values when user selects a Title ID or Language
  observeEvent(input$title_id, {
    current_title_id(input$title_id) # Store selected Title ID
  })
  
  observeEvent(input$language, {
    current_language(input$language) # Store selected Language
  })
  
  observe({
    updated_data <- translations_rv()
    
    if (!identical(updated_data, translations)) {
      translations <<- updated_data
      
      # Apply local changes
      for (change in local_changes()) {
        translations[translations$title_id == change$title_id, change$column] <- change$value
      }
      
      # Update UI elements if the current selection has changed
      if (input$title_id %in% translations$title_id) {
        current_row <- translations[translations$title_id == input$title_id, ]
        
        updateTextAreaInput(session, "title_unformatted", 
                            value = current_row[[paste0("title_", input$language)]])
        updateTextAreaInput(session, "description_unformatted", 
                            value = current_row[[paste0("description_", input$language)]])
      }
      
      showNotification("Data updated from Google Sheet", type = "message", duration = 3)
    }
  })
  
  
  # Reactive expression to filter translations data based on selected Title ID
  filtered_data <- reactive({
    req(input$title_id != "")
    translations_rv() %>% filter(title_id == input$title_id)
  })
  
  # Update text areas when Title ID or Language changes
  observe({
    req(input$title_id != "") # Only proceed if a title_id is selected
    req(filtered_data()) # Ensure filtered data is available
    
    lang_col_title <- paste0("title_", input$language)
    lang_col_description <- paste0("description_", input$language)
    
    updateTextAreaInput(session, "title_unformatted", value = filtered_data()[[lang_col_title]])
    updateTextAreaInput(session, "description_unformatted", value = filtered_data()[[lang_col_description]])
  })
  
  # Render HTML content for the title in real-time as user types in the text area
  output$title_rendered <- renderUI({
    req(input$title_unformatted) # Ensure input is not NULL before rendering HTML
    HTML(input$title_unformatted)
  })
  
  observe({
    if (input$title_id == "") {
      updateTextAreaInput(session, "title_unformatted", value = "")
      updateTextAreaInput(session, "description_unformatted", value = "")
    }
  })
  
  # Render HTML content for the description in real-time as user types in the text area
  output$description_rendered <- renderUI({
    req(input$description_unformatted) # Ensure input is not NULL before rendering HTML
    HTML(input$description_unformatted)
  })
  
  # Reactive values to track changes
  title_changed <- reactiveVal(FALSE)
  description_changed <- reactiveVal(FALSE)
  
  # Observe changes in title and description ####
  observeEvent(input$title_unformatted, {
    title_changed(TRUE)
    local_changes(c(local_changes(), list(list(
      title_id = input$title_id,
      column = paste0("title_", input$language),
      value = input$title_unformatted
    ))))
  })
  
  observeEvent(input$description_unformatted, {
    description_changed(TRUE)
    local_changes(c(local_changes(), list(list(
      title_id = input$title_id,
      column = paste0("description_", input$language),
      value = input$description_unformatted
    ))))
  })
  
  # Auto-save function
  autoSave <- reactive({
    req(filtered_data(), input$language, input$title_id)
    
    if (title_changed() || description_changed()) {
      updated_translations <- translations_rv()
      
      for (change in local_changes()) {
        updated_translations[updated_translations$title_id == change$title_id, change$column] <- change$value
      }
      
      tryCatch({
        sheet_write(updated_translations, sheet_id, sheet = "translations")
        showNotification("Changes auto-saved to Google Sheet", type = "message", duration = 3)
        local_changes(list())  # Clear local changes after successful save
      }, error = function(e) {
        showNotification(paste("Auto-save failed:", e$message), type = "error", duration = 5)
      })
      
      title_changed(FALSE)
      description_changed(FALSE)
      current_title_id(input$title_id)
      current_language(input$language)
    }
  })
  
  # # Trigger auto-save every 30 seconds
  # observe({
  #   invalidateLater(30000) # 30000 milliseconds = 30 seconds
  #   autoSave()
  # })
  
  # # Trigger reload every 5 seconds
  # observe({
  #   invalidateLater(5000) # 5000 milliseconds = 5 seconds
  #   load_data()
  # })
  
  # Save changes made by the user and reload data from CSV file
  observeEvent(input$save, {
    req(filtered_data())
    
    lang_col_title <- paste0("title_", input$language)
    lang_col_description <- paste0("description_", input$language)
    
    updated_translations <- translations_rv()
    
    updated_translations[updated_translations$title_id == input$title_id, lang_col_title] <- input$title_unformatted
    updated_translations[updated_translations$title_id == input$title_id, lang_col_description] <- input$description_unformatted
    
    # Update Google Sheet
    sheet_write(updated_translations, sheet_id, 
                sheet = "translations")
    
    current_title_id(input$title_id)
    current_language(input$language)
    
    showModal(modalDialog(
      title = "Success",
      "Changes have been saved successfully to Google Sheet!",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Add this new observer for the CSV save button
  observeEvent(input$save_csv, {
    req(filtered_data())
    
    lang_col_title <- paste0("title_", input$language)
    lang_col_description <- paste0("description_", input$language)
    
    updated_translations <- translations_rv()
    
    updated_translations[updated_translations$title_id == input$title_id, lang_col_title] <- input$title_unformatted
    updated_translations[updated_translations$title_id == input$title_id, lang_col_description] <- input$description_unformatted
    
    # Create a file name with current date and time
    file_name <- "../data/output/UAT_direct/translations.csv"
    
    # Save the data to CSV
    write_csv2(updated_translations, file_name)
    
    # Show a success message
    showModal(modalDialog(
      title = "CSV Saved",
      paste("Translations have been saved to", file_name),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
