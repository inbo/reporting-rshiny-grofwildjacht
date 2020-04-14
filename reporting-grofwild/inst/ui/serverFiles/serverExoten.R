## Server file for exoten indicators / checklist
## 
## Author: Eva Adriaensen
################################################################################
#
#### Filter Data
#### ---------------
#
#
## define primary user choices 
#output$exoten_regionOptions <- renderUI({ 
#      
#      selectInput(inputId = "exoten_region", label = "Regio('s)",
#          choices = na.omit(unique(exotenData$locality)),
#          selected = "België", multiple = TRUE)
#      
#    })
#
#output$exoten_timeOptions <- renderUI({
#      
#      sliderInput(inputId = "exoten_time", label = "Periode", 
#          value = c(min(exotenData$first_observed, na.rm = TRUE), defaultYear),
#          min = min(exotenData$first_observed, na.rm = TRUE),
#          max = max(exotenData$first_observed, na.rm = TRUE),
#          step = 1,
#          sep = "")
#      
#    })
#
## define primary dataset
#results$subExotenData <- reactive({
#      
#      validate(need(input$exoten_region, "Gelieve regio te selecteren"))
#      
#      toRetain <- exotenData$first_observed %in% req(input$exoten_time[1]):req(input$exoten_time[2]) &
#                  exotenData$locality %in% req(input$exoten_region)
#              
#      exotenData[toRetain,]
#      
#    })
#
#output$nrows <- renderText({
#      
#      validate(need(nrow(results$subExotenData()) > 0, "Geen data beschikbaar"))
#      paste("Totaal aantal:", nrow(results$subExotenData()))
##      head(results$exoten_data)
#    })
#
#
## define secondary user choices in a reactive manner
#
#output$exoten_bronOptions <- renderUI({ 
#      
##      subset(subExotenData)
#      
#      selectInput(inputId = "exoten_bron", label = "Bron(nen)",
#          choices = na.omit(unique(results$subExotenData()$source)),
#          selected = NULL, multiple = TRUE)
#      
#    })
#
#
#output$exoten_kingdomOptions <- renderUI({ 
#      
#      selectInput(inputId = "exoten_kingdom", label = "Kingdom",
#          choices = na.omit(unique(results$subExotenData()$kingdom)),
#          selected = NULL, multiple = TRUE)
#      
#    })
#
#
#output$exoten_phylumChoices <- renderUI({ 
#      
#      if (!is.null(req(input$exoten_kingdom)))
#        
#        selectInput(inputId = "exoten_phylum", label = "Phylum",
#            choices = na.omit(unique(results$subExotenData()$phylum)),
#            selected = NULL, multiple = TRUE
#        )
#    })
#output$exoten_classChoices <- renderUI({
#      
#      if (!is.null(req(input$exoten_phylum)) & 
#          !is.null(req(input$exoten_kingdom)))
#        
#        selectInput(inputId = "exoten_class", label = "Klasse",
#            choices = na.omit(unique(results$subExotenData()$class)),
#            selected = NULL, multiple = TRUE
#        )
#    })
#
#output$exoten_orderChoices <- renderUI({
#      
#      if (!is.null(req(input$exoten_class)) & 
#          !is.null(req(input$exoten_phylum)) & 
#          !is.null(req(input$exoten_kingdom)))
#        
#        selectInput(inputId = "exoten_order", label = "Orde",
#            choices = na.omit(unique(results$subExotenData()$order)),
#            selected = NULL, multiple = TRUE
#        )
#    })
#
#output$exoten_familyChoices <- renderUI({
#      
#      if (!is.null(req(input$exoten_order)) &
#          !is.null(req(input$exoten_class)) & 
#          !is.null(req(input$exoten_phylum)) & 
#          !is.null(req(input$exoten_kingdom)))
#        
#        selectInput(inputId = "exoten_family", label = "Familie",
#            choices = na.omit(unique(results$subExotenData()$family)),
#            selected = NULL, multiple = TRUE
#        )
#    })
#
#
## Filter data upon user choices
#
#results$exoten_data <- reactive({
#      
#      validate(need(results$subExotenData(), "Geen data beschikbaar"),
#               need(input$exoten_time, "Gelieve periode te selecteren"),
#               need(input$exoten_region, "Gelieve regio te selecteren"))
#      
#      # filter upon user choices
#      if (!is.null(input$exoten_bron)) {    
#        
#        toKeepBron <- results$subExotenData()$source %in% req(input$exoten_bron)
#        
#      } else toKeepBron <- NULL
#      
#      if (!is.null(input$exoten_kingdom)) {
#        
#        toKeepKingdom <- results$subExotenData()$kingdom %in% req(input$exoten_kingdom)
#        
#      } else toKeepKingdom <- NULL
#      
#      
#      # combine user choices
#      if (is.null(toKeepBron) & is.null(toKeepKingdom)) {
#        
#        toKeep <- rep(TRUE, nrow(results$subExotenData()))
#      
#      } else {
#        
#        keepersList <- list(toKeepBron,toKeepKingdom)
#        keepersList[sapply(keepersList, is.null)] <- NULL
#        toKeep <- Reduce(`&`, keepersList)
#        
#      }
#
#      # return final dataset
#      results$subExotenData()[toKeep,]
#      
#    })
#
#results$exoten_currentBronOptions = reactive({
#      na.omit(unique(results$exoten_data()$source))
#    })
#observeEvent(results$exoten_currentBronOptions(), {
#      updateSelectInput(session, "exoten_bron",
#          choices = results$exoten_currentBronOptions()
#      )
#    })
#
#
#output$nrowsFinal <- renderText({
#      
#      validate(need(nrow(results$exoten_data()) > 0, "Geen data beschikbaar"))
#      paste("Totaal aantal:", nrow(results$exoten_data()))
##      head(results$exoten_data)
#    })
#
#
### Plots
### -----------------
#
#
### Plot number of species per year
#
#output$exoten_titleSoortenPerJaar <- renderUI({      
#      
#      h2(paste("Aantal geïntroduceerde uitheemse soorten per jaar", 
#                "in", vectorToTitleString(input$exoten_region),
#                yearToTitleString(req(input$exoten_time))
#              )
#        )
#      
#    })
#
#callModule(module = plotModuleServer, id = "exoten_soortenPerJaar",
#    plotFunction = "countIntroductionYear", 
#    data = reactive(results$exoten_data())
#)
#
### Plot cumulative number of species per year
#
#output$exoten_titleSoortenCumulatiefPlot <- renderUI({      
#      
#      h2(paste("Cumulatief aantal geïntroduceerde uitheemse soorten", 
#                "in", vectorToTitleString(input$exoten_region),
#                yearToTitleString(req(input$exoten_time))
#              )
#        )
#   })
#
#
#callModule(module = plotModuleServer, id = "exoten_soortenCumulatiefPlot",
#    plotFunction = "cumulativeIntroductionYear", 
#    data = reactive(results$exoten_data())
#)
