# Server file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################

### Filter Data
### ---------------


# define primary user choices 
output$exoten_regionOptions <- renderUI({ 
      
      selectInput(inputId = "exoten_region", label = "Regio('s)",
          choices = na.omit(unique(exotenData$locality)),
          selected = "België", multiple = TRUE)
      
    })

output$exoten_timeOptions <- renderUI({
      
      sliderInput(inputId = "exoten_time", label = "Periode", 
          value = c(min(exotenData$first_observed, na.rm = TRUE), defaultYear),
          min = min(exotenData$first_observed, na.rm = TRUE),
          max = max(exotenData$first_observed, na.rm = TRUE),
          step = 1,
          sep = "")
      
    })

# define primary dataset
results$subExotenData <- reactive({
      
      validate(need(input$exoten_region, "Gelieve regio te selecteren"))
      
      toRetain <- exotenData$first_observed %in% req(input$exoten_time[1]):req(input$exoten_time[2]) &
          exotenData$locality %in% req(input$exoten_region)
      
      exotenData[toRetain,]
      
    })

output$nrows <- renderText({
      
      validate(need(nrow(results$subExotenData()) > 0, "Geen data beschikbaar"))
      paste("Totaal aantal:", nrow(results$subExotenData()))
#      head(results$exoten_data)
    })


currentChoices <- reactiveValues(
                        source = unique(sort(exotenData$source)),
                        kingdom = unique(sort(exotenData$kingdom)))
currentSelected <- reactiveValues(
                        source = unique(sort(exotenData$source)),
                        kingdom = unique(sort(exotenData$kingdom)))

output$exoten_bronOptions <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_bron", "Bron", choices = currentChoices$source, selected = currentSelected$source, multiple = TRUE)
    })

output$exoten_kingdomOptions <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_kingdom", "Kingdom", choices = currentChoices$kingdom, selected = currentSelected$kingdom, multiple = TRUE)
    })


# Define updated choices and selected based on all other input filters
observe({
      filterData1 <- subset(exotenData, kingdom %in% input$exoten_kingdom)
      
      currentChoices$source <- unique(sort(filterData1$source))
      currentSelected$source <- currentSelected$source[currentSelected$source %in% currentChoices$source]
      
    })

observe({
      filterData2 <- subset(exotenData, source %in% input$exoten_bron)

      currentChoices$kingdom <- unique(sort(filterData2$kingdom))
      currentSelected$kingdom <- currentSelected$kingdom[currentSelected$kingdom %in% currentChoices$kingdom]
      
    })

results$exoten_data <- reactive({
      
      subset(exotenData, source %in% currentSelected$source & 
                          kingdom %in% currentSelected$kingdom)
      
    })

output$nrowsFinal <- renderText({
      
      validate(need(nrow(results$exoten_data()) > 0, "Geen data beschikbaar"))
      paste("Totaal aantal:", nrow(results$exoten_data()))
#      head(results$exoten_data)
    })


## Plots
## -----------------


## Plot number of species per year

output$exoten_titleSoortenPerJaar <- renderUI({      
      
      h2(paste("Aantal geïntroduceerde uitheemse soorten per jaar", 
                "in", vectorToTitleString(input$exoten_region),
                yearToTitleString(req(input$exoten_time))
              )
        )
      
    })

callModule(module = plotModuleServer, id = "exoten_soortenPerJaar",
    plotFunction = "countIntroductionYear", 
    data = reactive(results$exoten_data())
)

## Plot cumulative number of species per year

output$exoten_titleSoortenCumulatiefPlot <- renderUI({      
      
      h2(paste("Cumulatief aantal geïntroduceerde uitheemse soorten", 
                "in", vectorToTitleString(input$exoten_region),
                yearToTitleString(req(input$exoten_time))
              )
        )
   })


callModule(module = plotModuleServer, id = "exoten_soortenCumulatiefPlot",
    plotFunction = "cumulativeIntroductionYear", 
    data = reactive(results$exoten_data())
)
