# Server file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################

### Filter Data
### ---------------

# define full dataset on startup
results$exoten_data <- exotenData

# define user choices in reactive manner

output$exoten_regionList <- renderUI({ 
      
      selectInput(inputId = "exoten_region", label = "Regio('s)",
          choices = na.omit(unique(results$exoten_data$locality)),
          selected = "Belgium", multiple = TRUE)
      
    })

output$exoten_timeOptions <- renderUI({
      
      sliderInput(inputId = "exoten_time", label = "Periode", 
          value = c(min(results$exoten_data$first_observed, na.rm = TRUE), defaultYear),
          min = min(results$exoten_data$first_observed, na.rm = TRUE),
          max = max(results$exoten_data$first_observed, na.rm = TRUE),
          step = 1,
          sep = "")
      
    })

# Filter data upon user choices
#results$exoten_data <- reactive({
#      
#      toRetain <- results$exoten_data$first_observed %in% req(input$exoten_time[1]):req(input$exoten_time[2]) &
#                  results$exoten_data$locality %in% req(input$exoten_region)
#              
#      exoten_data[toRetain,]
#      
#    })

output$nrows <- renderText({
      
      nrow(results$exoten_data)
#      head(results$exoten_data)
    })