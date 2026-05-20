library(shiny)
library(dplyr)
library(readr)
library(htmltools)
library(shinyWidgets)
conflicted::conflicts_prefer(dplyr::filter)

load_data <- function() {
  read_csv2("../reporting-grofwild/inst/extdata/uiText.csv", show_col_types = FALSE)
}

# UI ####
ui <- fluidPage(
  ## TitlePanel ####
  titlePanel("Translation Editor"),
  ## SideBar ####
  sidebarLayout(
    sidebarPanel(
      selectInput("Item", "Select plotFunction:",
                  choices = c("", unique(load_data()$plotFunction))),
      selectInput("Type", "Select Type:",
                  choices = c("Public", "Private", "Summary")),
      textInput("new_plotFunction", "New plotFunction name:"),
      actionButton("add_plotFunction", "Add New plotFunction"),
      actionButton("save_csv", "Save to CSV"),
      actionButton("reset_buffer", "Reset Changes"),
      HTML("<br>
      <br><h3>Instructions</h3>
      <ol start=1> 
      <li><b>Select a plotFunction</b>, this is the name of the function used to create a visualisation or the name of an element</li>
      <li><b>Select a type</b>, each plotfunction has 3 types, namely: 
          <ul>
          <li><i>Public</i>: The description under the graph or the translation of an element on <a href='https://faunabeheer.inbo.be' target='_blank'>the public page (faunabeheer.inbo.be)</a></li>
          <li><i>Private</i>: The description under the graph or the translation of an element on <a href='https://wbe.inbo.be' target='_blank'>the private page (wbe.inbo.be)</a></li>
          <li><i>Summary</i>: The short description on the relevant tile on <a href='https://faunabeheer.inbo.be' target='_blank'>the public page (faunabeheer.inbo.be)</a></li>
          </ul>
      </li>
      <li>If needed use <a href='https://web.stanford.edu/group/csp/cs21/htmlcheatsheet.pdf' target='_blank'>HTML styling</a> to change the Title and/or Description</li>
      <li>Alternatively a New plotFunction can be added by:
        <ol start=1> 
          <li>Adding the new title in 'New plotFunction name:'</li>
          <li>Clicking 'Add New plotFunction'</li>
          <li>Populating the description fields</li>
        </ol>
      <li>Click 'Save to CSV' to save any changes to the <a href='https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/uat/reporting-grofwild/inst/extdata/uiText.csv' target='_blank'>uiText.csv</a> file</li>
      </ol>
      <h4>Some advice</h4>
      <i>When adding urls make sure both the URL (href='https://url.be') and the target (target='_blank') are quoted with single quotes ( ' instead of \" )</i><br>
      <i>When using styling make sure you close the styling element by adding the &lt;/&gt; element</i><br>
      <i>When you want to add a text hover:
        <ol start=1>
          <li>If the text you want to explain is missing a plotFunction, create a new plotFunction using 'Add New plotFunction'. Make sure the plotFunction has the following format: hover_&lt;text to explain&gt;</li>
          <li>Next add &lt;span class='tooltip-wrapper'&gt;text to explain&lt;span class='tooltip-box'&gt;{{{hover_text to explain}}}&lt;/span&gt;&lt;/span&gt; instead of the text you want to explain</li>

        </ol>
      </i>
           ")
    ),
    ## Main ####
    mainPanel(
      textAreaInput("title_unformatted", "Title (Unformatted):", width = "100%", height = "100px"),
      HTML("<b>Title (formatted):</b><br><br>"),
      htmlOutput("title_rendered"),
      HTML("<br>"),
      textAreaInput("description_unformatted", "Description (Unformatted):", width = "100%", height = "200px"),
      HTML("<b>Description (formatted):</b><br><br>"),
      htmlOutput("description_rendered")
    )
  )
)

# SERVER ####
server <- function(input, output, session) {
  ## Initial data load ####
  translations <- reactiveVal(load_data())
  
  ## Initiate edited_data ####
  # Buffered edits stored here; initially a copy of original data 
  edited_data <- reactiveVal(load_data())
  
  ## Initiate original_row ####
  # Track current selected row snapshot for change detection
  original_row <- reactiveVal(NULL)
  
  ## Filter Current Row & Set desc_val ####
  observeEvent(list(input$Item, input$Type), {
    req(input$Item != "")
    data <- edited_data()
    current_row <- data %>% filter(plotFunction == input$Item)
    
    if (nrow(current_row) > 0) {
      original_row(current_row)
      updateTextAreaInput(session, "title_unformatted", 
                          value = current_row$title)
      
      desc_val <- switch(input$Type,
                         "Public" = current_row$description,
                         "Private" = current_row$wbe,
                         "Summary" = current_row$summary,
                         "")
      
      updateTextAreaInput(session, "description_unformatted",
                          value = ifelse(!is.na(desc_val), desc_val, ""))
    } else {
      original_row(NULL)
      updateTextAreaInput(session, "title_unformatted", value = "")
      updateTextAreaInput(session, "description_unformatted", value = "")
    }
  })
  
  ## Update buffered edits reactiveVal ####
  # immediately on user typing
  observeEvent(input$title_unformatted, {
    req(input$Item != "")
    data <- edited_data()
    
    # Find row index
    idx <- which(data$plotFunction == input$Item)
    if (length(idx) == 1) {
      data[idx, "title"] <- input$title_unformatted
      edited_data(data)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$description_unformatted, {
    req(input$Item != "")
    data <- edited_data()
    idx <- which(data$plotFunction == input$Item)
    if (length(idx) == 1) {
      target_col <- switch(input$Type,
                           "Public" = "description",
                           "Private" = "wbe",
                           "Summary" = "summary")
      data[idx, target_col] <- input$description_unformatted
      edited_data(data)
    }
  }, ignoreInit = TRUE)
  
  ## Add New PlotFunction ####
  observeEvent(input$add_plotFunction, {
    new_name <- trimws(input$new_plotFunction)
    req(new_name != "")
    
    # Update both translations and edited_data reactiveVals
    for (buffer_name in c("translations", "edited_data")) {
      data <- get(buffer_name)()
      if (new_name %in% data$plotFunction) {
        showNotification("This plotFunction already exists.", type = "error")
        return()
      }
      new_row <- tibble(
        plotFunction = new_name,
        title = "",
        description = "",
        wbe = "",
        summary = ""
      )
      updated_data <- bind_rows(data, new_row)
      # Assign back to the corresponding reactiveVal
      if (buffer_name == "translations") translations(updated_data)
      if (buffer_name == "edited_data") edited_data(updated_data)
    }
    
    original_row(new_row)
    updateSelectInput(session, "Item", choices = c("", translations()$plotFunction), selected = new_name)
    updateTextInput(session, "new_plotFunction", value = "")
    showNotification(paste("Added new plotFunction:", new_name), type = "message")
  })
  
  ## Render Title ####
  output$title_rendered <- renderUI({
    req(input$title_unformatted)
    HTML(input$title_unformatted)
  })
  
  ## Render Description ####
  output$description_rendered <- renderUI({
    req(input$description_unformatted)
    HTML(input$description_unformatted)
  })
  
  ## Save to CSV ####
  # Save buffered edits to CSV
  observeEvent(input$save_csv, {
    req(input$Item != "")
    data <- edited_data()
    
    # Write the entire buffered data frame to CSV at once
    write_delim(data, "../reporting-grofwild/inst/extdata/uiText.csv", 
                delim = ";", 
                quote = "all", 
                na = "\"\"")
    
    showModal(modalDialog(
      title = "CSV Saved",
      paste("All current changes saved to uiText.csv"),
      easyClose = TRUE
    ))
    
    # Reset original_row to current saved row for detecting further changes
    original_row(data %>% filter(plotFunction == input$Item))
    
    # Also update translations reactiveVal so it's in sync
    translations(data)
  })
  
  ## reset buffer confirmation ####
  observeEvent(input$reset_buffer, {
    ask_confirmation(
      inputId = "confirm_reset",
      title = "Are you sure?",
      text = "This will discard all unsaved changes and reload the last saved data.",
      type = "warning",
      btn_labels = c("Cancel", "Yes, reset"),
      btn_colors = c("#AAAAAA", "#FF0000"),
      closeOnClickOutside = FALSE
    )
  })
  
  ## reset buffer ####
  observeEvent(input$confirm_reset, {
    if (isTRUE(input$confirm_reset)) {
      updated_data <- load_data()  # reload original data from CSV
      edited_data(updated_data)    # reset reactive buffer
      
      # If an item is selected, update inputs accordingly
      if (input$Item != "") {
        current_row <- updated_data %>% filter(plotFunction == input$Item)
        if (nrow(current_row) > 0) {
          updateTextAreaInput(session, "title_unformatted", value = current_row$title)
          desc_val <- switch(input$Type,
                             "Public" = current_row$description,
                             "Private" = current_row$wbe,
                             "Summary" = current_row$summary,
                             "")
          updateTextAreaInput(session, "description_unformatted", value = ifelse(!is.na(desc_val), desc_val, ""))
          original_row(current_row)
        }
      }
      showNotification("Buffered changes reset from CSV file", type = "message")
    }
  })
}

shinyApp(ui, server)
