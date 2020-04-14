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


### Define (reactive) user choices
### ---------------

currentChoices <- reactiveValues()
currentSelected <- reactiveValues()

observe({
      
      currentChoices$source <- currentSelected$source <- unique(sort(results$subExotenData()$source))
      currentChoices$kingdom <- currentSelected$kingdom <- unique(sort(results$subExotenData()$kingdom))
      currentChoices$pw1 <- currentSelected$pw1 <- unique(sort(results$subExotenData()$pathway_level1))
      
      
    })


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

output$exoten_pw1Options <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_pw1", "Pathway level 1", choices = currentChoices$pw1, selected = currentSelected$pw1, multiple = TRUE)
    })


## Define updated choices and selected based on all other input filters

observeEvent(c(input$exoten_kingdom, input$exoten_pw1, input$exoten_bron), {
      filterData1 <- subset(results$subExotenData(), kingdom %in% input$exoten_kingdom &
              pathway_level1 %in% input$exoten_pw1)
      
      
      currentChoices$source <- unique(sort(filterData1$source))
      currentSelected$source <- input$exoten_bron[input$exoten_bron %in% currentChoices$source]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_pw1, input$exoten_kingdom), {
      filterData2 <- subset(results$subExotenData(), source %in% input$exoten_bron &
              pathway_level1 %in% input$exoten_pw1)
      
      currentChoices$kingdom <- unique(sort(filterData2$kingdom))
      currentSelected$kingdom <- input$exoten_kingdom[input$exoten_kingdom %in% currentChoices$kingdom]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_pw1), { 
      filterData3 <- subset(results$subExotenData(), source %in% input$exoten_bron &
              kingdom %in% input$exoten_kingdom)
      
      currentChoices$pw1 <- unique(sort(filterData3$pathway_level1))
      currentSelected$pw1 <- input$exoten_pw1[input$exoten_pw1 %in% currentChoices$pw1]
      
    })


### Final Data set
### ---------------


results$exoten_data <- reactive({
      
      subset(results$subExotenData(), source %in% currentSelected$source & 
              kingdom %in% currentSelected$kingdom &
              pathway_level1 %in% currentSelected$pw1
      )
      
    })

output$nrowsFinal <- renderText({
      
      validate(need(nrow(results$exoten_data()) > 0, "Geen data beschikbaar"))
      paste("Totaal aantal:", nrow(results$exoten_data()))
#      head(results$exoten_data)
    })


### Plots
### -----------------


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
