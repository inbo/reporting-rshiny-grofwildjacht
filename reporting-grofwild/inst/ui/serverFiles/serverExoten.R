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
      currentChoices$phylum <- unique(sort(results$subExotenData()$phylum))
        currentSelected$phylum <- NULL
      currentChoices$class <- unique(sort(results$subExotenData()$class))
        currentSelected$class <- NULL
      currentChoices$order <- unique(sort(results$subExotenData()$order))
        currentSelected$order <- NULL
      currentChoices$family <- unique(sort(results$subExotenData()$family))
        currentSelected$family <- NULL
      currentChoices$pw1 <- currentSelected$pw1 <- unique(sort(results$subExotenData()$pathway_level1))
      currentChoices$pw2 <- unique(sort(results$subExotenData()$pathway_level2))
        currentSelected$pw2 <- NULL
      currentChoices$habitat <- currentSelected$habitat <- habitats
      currentChoices$doe <- currentSelected$doe <- unique(sort(results$subExotenData()$degree_of_establishment))
      
    })

#callModule(module = selectModuleServer,
#    id = "kingdom",
#    label = "kingdom test",
#    choices = currentChoices$kingdom,
#    selected = currentSelected$kingdom)

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

# conditional on higher taxa definitions
output$exoten_phylumOptions <- renderUI({ 
      
      if (!is.null(req(input$exoten_kingdom)))
        
        selectInput(inputId = "exoten_phylum", label = "Phylum", 
            choices = currentChoices$phylum, selected = currentSelected$phylum,
            multiple = TRUE)
    })

output$exoten_classOptions <- renderUI({
      
      if (!is.null(req(input$exoten_phylum)) & !is.null(req(input$exoten_kingdom)))
        
        selectInput(inputId = "exoten_class", label = "Klasse",
            choices = currentChoices$class, selected = currentSelected$class, 
            multiple = TRUE
        )
    })

output$exoten_orderOptions <- renderUI({
      
      if (!is.null(req(input$exoten_class)) & !is.null(req(input$exoten_phylum)) & !is.null(req(input$exoten_kingdom)))
        
        selectInput(inputId = "exoten_order", label = "Orde",
            choices = currentChoices$order, selected = currentSelected$order, 
            multiple = TRUE
        )
    })

output$exoten_familyOptions <- renderUI({
      
      if (!is.null(req(input$exoten_order)) & !is.null(req(input$exoten_class)) & 
          !is.null(req(input$exoten_phylum)) & !is.null(req(input$exoten_kingdom)))
        
        selectInput(inputId = "exoten_family", label = "Familie",
            choices = currentChoices$family, selected = currentSelected$family, 
            multiple = TRUE
        )
    })

output$exoten_pw1Options <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_pw1", "Pathway level 1", choices = currentChoices$pw1, selected = currentSelected$pw1, multiple = TRUE)
    })

output$exoten_pw2Options <- renderUI({
      
      if (!is.null(req(input$exoten_pw1)))
      
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_pw2", "Pathway level 2", choices = currentChoices$pw2, selected = currentSelected$pw2, multiple = TRUE)
    })

output$exoten_habitatOptions <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_habitat", "Habitat", choices = currentChoices$habitat, selected = currentSelected$habitat, multiple = TRUE)
    })

output$exoten_doeOptions <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_doe", "Degree of establishment", choices = currentChoices$doe, selected = currentSelected$doe, multiple = TRUE)
    })

## Define updated choices and selected based on all other input filters

observeEvent(c(input$exoten_kingdom, input$exoten_pw1, input$exoten_habitat, input$exoten_doe, input$exoten_bron), {
      filterData1 <- subset(results$subExotenData(), kingdom %in% input$exoten_kingdom &
                                                     pathway_level1 %in% input$exoten_pw1 &
                                                     degree_of_establishment %in% input$exoten_doe)
      
      filterData1 <- filterData1[apply(filterData1[, .SD, .SDcols = which(colnames(filterData1) %in% input$exoten_habitat)], 1, function(x) any(x, na.rm = TRUE)), ]
                                             
      
      currentChoices$source <- unique(sort(filterData1$source))
      currentSelected$source <- input$exoten_bron[input$exoten_bron %in% currentChoices$source]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_pw1, input$exoten_habitat, input$exoten_doe, input$exoten_kingdom), {
      filterData2 <- subset(results$subExotenData(), source %in% input$exoten_bron &
                                                     pathway_level1 %in% input$exoten_pw1 &
                                                     degree_of_establishment %in% input$exoten_doe)
      
      filterData2 <- filterData2[apply(filterData2[, .SD, .SDcols = which(colnames(filterData2) %in% input$exoten_habitat)], 1, function(x) any(x, na.rm = TRUE)), ]
                                             
      
      currentChoices$kingdom <- unique(sort(filterData2$kingdom))
      currentSelected$kingdom <- input$exoten_kingdom[input$exoten_kingdom %in% currentChoices$kingdom]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_doe, input$exoten_pw1), { 
      filterData3 <- subset(results$subExotenData(), source %in% input$exoten_bron &
                                                     kingdom %in% input$exoten_kingdom &
                                                     degree_of_establishment %in% input$exoten_doe)
                                             
      filterData3 <- filterData3[apply(filterData3[, .SD, .SDcols = which(colnames(filterData3) %in% input$exoten_habitat)], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$pw1 <- unique(sort(filterData3$pathway_level1))
      currentSelected$pw1 <- input$exoten_pw1[input$exoten_pw1 %in% currentChoices$pw1]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_pw1, input$exoten_doe, input$exoten_habitat), { 
      
      filterData4 <- subset(results$subExotenData(), source %in% input$exoten_bron &
                                                     kingdom %in% input$exoten_kingdom &
                                                     pathway_level1 %in% input$exoten_pw1 &
                                                     degree_of_establishment %in% input$exoten_doe)
                                                       
      currentChoices$habitat <- names(which(apply(filterData4[, .SD, .SDcols = which(colnames(filterData4) %in% habitats)], 2, function(x) any(x, na.rm = TRUE))))
      currentSelected$habitat <- input$exoten_habitat[input$exoten_habitat %in% currentChoices$habitat]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_pw1, input$exoten_doe), { 
      filterData5 <- subset(results$subExotenData(), source %in% input$exoten_bron &
                                                     kingdom %in% input$exoten_kingdom &
                                                     pathway_level1 %in% input$exoten_pw1)
                                             
      filterData5 <- filterData5[apply(filterData5[, .SD, .SDcols = which(colnames(filterData5) %in% input$exoten_habitat)], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$doe <- unique(sort(filterData5$degree_of_establishment))
      currentSelected$doe <- input$exoten_doe[input$exoten_doe %in% currentChoices$doe]
      
    })


### Final Data set
### ---------------


results$exoten_data <- reactive({
      
      # filter for variables
      dataSub <- subset(results$subExotenData(), source %in% currentSelected$source & 
                                                  kingdom %in% currentSelected$kingdom &
                                                  pathway_level1 %in% currentSelected$pw1 &
                                                  degree_of_establishment %in% input$exoten_doe)

      # filter for habitat logicals
      dataSub[apply(dataSub[, .SD, .SDcols = which(colnames(dataSub) %in% input$exoten_habitat)], 1, function(x) any(x, na.rm = TRUE)), ]
                

      
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
