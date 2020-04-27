# Server file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################

### Filter Data
### ---------------

## create reactive values to store possible choices and current selected

currentChoices <- reactiveValues()
currentSelected <- reactiveValues()

observe({
      
      currentChoices$locality <- unique(sort(exotenData$locality))
      currentSelected$locality <- NULL
      
    })

# define primary user choices 
output$exoten_regionOptions <- renderUI({ 
      
      selectInput(inputId = "exoten_region", label = "Regio('s)",
          choices = currentChoices$locality,
          selected = currentSelected$locality, multiple = TRUE)
      
    })

output$exoten_timeOptions <- renderUI({
      
      sliderInput(inputId = "exoten_time", label = "Periode", 
          value = c(min(exotenData$first_observed, na.rm = TRUE), defaultYear),
          min = min(exotenData$first_observed, na.rm = TRUE),
          max = max(exotenData$first_observed, na.rm = TRUE),
          step = 1,
          sep = "")
      
    })

## Region filter starts empty in UI which will filter out BELGIUM cases.
## Upon a user specified choice, this initial BELGIUM selection will be overwritten
results$exoten_filterLocality <- reactive({
            
      if (is.null(input$exoten_region))
        "België" else
        input$exoten_region
      
    })

# define primary dataset
results$subExotenData <- reactive({
            
      toRetain <- exotenData$first_observed %in% req(input$exoten_time[1]):req(input$exoten_time[2]) &
                  exotenData$locality %in% req(results$exoten_filterLocality() )
      
      exotenData[toRetain,]
      
    })

output$nrows <- renderText({
      
      validate(need(nrow(results$subExotenData()) > 0, "Geen data beschikbaar"))
      paste("Totaal aantal:", nrow(results$subExotenData()))
    })


### Define secondary (reactive) user choices and empty default selection
### ---------------

vars <- c("source", 
          "kingdom", 
          "phylum", 
          "class", 
          "order", 
          "family", 
          "pathway_level1", 
          "pathway_level2", 
          "degree_of_establishment")

## define possible choices 
observe({
      
     for (i in vars)  {
      currentChoices[[i]] <- unique(sort(results$subExotenData()[[i]]))
    }
 
  })

## define selected choices (default NULL)
for (i in vars) {
  currentSelected[[i]] <- NULL
  
}

## set choices and selected for habitat
observe({
      
        ## TODO do we need to adjust habitat choices to exclude habitats for which each observation is NA in results$subExotenData() ?
#      currentChoices$habitat <- habitats
      currentChoices$habitat <- names(which(apply(results$subExotenData()[, .SD, .SDcols = which(colnames(results$subExotenData()) %in% habitats)], 2, function(x) any(x, na.rm = TRUE) )))
        currentSelected$habitat <- NULL
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

# conditional on higher taxa definitions
output$exoten_classOptions <- renderUI({
      
      if (!is.null(req(input$exoten_phylum)) & !is.null(req(input$exoten_kingdom)))
        
        selectInput(inputId = "exoten_class", label = "Klasse",
            choices = currentChoices$class, selected = currentSelected$class, 
            multiple = TRUE
        )
    })

# conditional on higher taxa definitions
output$exoten_orderOptions <- renderUI({
      
      if (!is.null(req(input$exoten_class)) & !is.null(req(input$exoten_phylum)) & !is.null(req(input$exoten_kingdom)))
        
        selectInput(inputId = "exoten_order", label = "Orde",
            choices = currentChoices$order, selected = currentSelected$order, 
            multiple = TRUE
        )
    })

# conditional on higher taxa definitions
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
      selectInput("exoten_pw1", "Pathway level 1", choices = currentChoices$pathway_level1, selected = currentSelected$pathway_level1, multiple = TRUE)
    })

# conditional on higher pathway definition
output$exoten_pw2Options <- renderUI({
      
      req(input$exoten_pw1) 
      
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_pw2", "Pathway level 2", choices = currentChoices$pathway_level2, selected = currentSelected$pathway_level2, multiple = TRUE)
    })

output$exoten_habitatOptions <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_habitat", "Habitat", choices = currentChoices$habitat, selected = currentSelected$habitat, multiple = TRUE)
    })

output$exoten_doeOptions <- renderUI({
      # only update if selected or choices is no longer up to date/possible
      # if updated choices, relevant selection is shown based on previous input
      selectInput("exoten_doe", "Degree of establishment", choices = currentChoices$degree_of_establishment, selected = currentSelected$degree_of_establishment, multiple = TRUE)
    })

## Define what to filter in cases input is empty

results$exoten_filterBron <- reactive({
      
      if (is.null(input$exoten_bron))
        c(NA, unique(sort(results$subExotenData()$source))) else
        input$exoten_bron
      
    })

results$exoten_filterKingdom <- reactive({
            
      if (is.null(input$exoten_kingdom))
        c(NA, unique(sort(results$subExotenData()$kingdom))) else
        input$exoten_kingdom
      
    })

results$exoten_filterPhylum <- reactive({
      
      if (is.null(input$exoten_phylum) |
          is.null(input$exoten_kingdom)) {
        
        subData <- subset(results$subExotenData(), kingdom %in% results$exoten_filterKingdom())
        
        c(NA, unique(sort(subData$phylum))) 
      } else
        input$exoten_phylum
      
    })

results$exoten_filterClass <- reactive({
      
      if (is.null(input$exoten_class) |
          is.null(input$exoten_phylum) |
          is.null(input$exoten_kingdom)) {
        
        subData <- subset(results$subExotenData(), kingdom %in% results$exoten_filterKingdom() &
                                                   phylum %in% results$exoten_filterPhylum() )
      
        c(NA, unique(sort(subData$class)))
      } else
        input$exoten_class
      
    })

results$exoten_filterOrder <- reactive({
      
      if (is.null(input$exoten_order) |
          is.null(input$exoten_class) |
          is.null(input$exoten_phylum) |
          is.null(input$exoten_kingdom)) {
        
        subData <- subset(results$subExotenData(), kingdom %in% results$exoten_filterKingdom() &
                                                   phylum %in% results$exoten_filterPhylum() &
                                                   class %in% results$exoten_filterClass() )
        
        c(NA, unique(sort(subData$order))) 
      } else
        input$exoten_order
      
    })

results$exoten_filterFamily <- reactive({
      
      if (is.null(input$exoten_family) |
          is.null(input$exoten_order) |
          is.null(input$exoten_class) |
          is.null(input$exoten_phylum) |
          is.null(input$exoten_kingdom)) {
        
        subData <- subset(results$subExotenData(), kingdom %in% results$exoten_filterKingdom() &
                                                    phylum %in% results$exoten_filterPhylum() &
                                                    class %in% results$exoten_filterClass() &
                                                    order %in% results$exoten_filterOrder() )
        
        c(NA, unique(sort(subData$family))) 
      } else
        input$exoten_family
      
    })

results$exoten_filterPw1 <- reactive({
            
      if (is.null(input$exoten_pw1))
        c(NA, unique(sort(results$subExotenData()$pathway_level1))) else
        input$exoten_pw1
      
    })

results$exoten_filterPw2 <- reactive({
      
      if (is.null(input$exoten_pw2) | is.null(input$exoten_pw1)) {
        
        subData <- subset(results$subExotenData(), pathway_level1 %in% results$exoten_filterPw1() )
        
        c(NA, unique(sort(subData$pathway_level2)))

      } else
        input$exoten_pw2
      
    })

results$exoten_filterHabitat <- reactive({
            
      if (is.null(input$exoten_habitat))
        colnames(results$subExotenData())[colnames(results$subExotenData()) %in% habitats] else
        input$exoten_habitat
      
    })

results$exoten_filterDoe <- reactive({
      
      if (is.null(input$exoten_doe))
        c(NA, unique(sort(results$subExotenData()$degree_of_establishment))) else
        input$exoten_doe
      
    })

## Update choices and selected based on all other input filters

observeEvent(c(input$exoten_kingdom, input$exoten_pw1, input$exoten_habitat, input$exoten_doe, input$exoten_bron,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData1 <- subset(results$subExotenData(), kingdom %in% results$exoten_filterKingdom() &
                                                     phylum %in% results$exoten_filterPhylum() &
                                                     class %in% results$exoten_filterClass() &
                                                     order %in% results$exoten_filterOrder() &
                                                     family %in% results$exoten_filterFamily() &
                                                     pathway_level1 %in% results$exoten_filterPw1() &
                                                     pathway_level2 %in% results$exoten_filterPw2() &
                                                     degree_of_establishment %in% results$exoten_filterDoe() )
      
      filterData1 <- filterData1[apply(filterData1[, .SD, .SDcols = which(colnames(filterData1) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
                                             
      
      currentChoices$source <- unique(sort(filterData1$source))
      currentSelected$source <- if (is.null(input$exoten_bron)) NULL else input$exoten_bron[input$exoten_bron %in% currentChoices$source]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_pw1, input$exoten_habitat, input$exoten_doe, input$exoten_kingdom,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2
        ), { 
      filterData2 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
#                                                      phylum %in% results$exoten_filterPhylum() &
#                                                      class %in% results$exoten_filterClass() &
#                                                      order %in% results$exoten_filterOrder() &
#                                                      family %in% results$exoten_filterFamily() &
                                                      pathway_level1 %in% results$exoten_filterPw1() &
                                                      pathway_level2 %in% results$exoten_filterPw2() &
                                                      degree_of_establishment %in% results$exoten_filterDoe() )
      
      filterData2 <- filterData2[apply(filterData2[, .SD, .SDcols = which(colnames(filterData2) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
                                             
      
      currentChoices$kingdom <- unique(sort(filterData2$kingdom))
      currentSelected$kingdom <- if (is.null(input$exoten_kingdom)) NULL else input$exoten_kingdom[input$exoten_kingdom %in% currentChoices$kingdom]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_doe, input$exoten_pw1,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData3 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                      kingdom %in% results$exoten_filterKingdom() &
                                                      phylum %in% results$exoten_filterPhylum() &
                                                      class %in% results$exoten_filterClass() &
                                                      order %in% results$exoten_filterOrder() &
                                                      family %in% results$exoten_filterFamily() &
#                                                      pathway_level2 %in% results$exoten_filterPw2() &
                                                      degree_of_establishment %in% results$exoten_filterDoe() )
                                             
      filterData3 <- filterData3[apply(filterData3[, .SD, .SDcols = which(colnames(filterData3) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$pathway_level1 <- unique(sort(filterData3$pathway_level1))
      currentSelected$pathway_level1 <- if (is.null(input$exoten_pw1)) NULL else input$exoten_pw1[input$exoten_pw1 %in% currentChoices$pathway_level1]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_pw1, input$exoten_doe, input$exoten_habitat,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData4 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                     kingdom %in% results$exoten_filterKingdom() &
                                                     phylum %in% results$exoten_filterPhylum() &
                                                     class %in% results$exoten_filterClass() &
                                                     order %in% results$exoten_filterOrder() &
                                                     family %in% results$exoten_filterFamily() &
                                                     pathway_level1 %in% results$exoten_filterPw1() &
                                                     pathway_level2 %in% results$exoten_filterPw2() &
                                                     degree_of_establishment %in% results$exoten_filterDoe() )
                                                       
#      currentChoices$habitat <- names(which(apply(filterData4[, .SD, .SDcols = which(colnames(filterData4) %in% results$exoten_filterHabitat() )], 2, function(x) any(x, na.rm = TRUE))))
      currentChoices$habitat <- names(which(apply(filterData4[, .SD, .SDcols = which(colnames(filterData4) %in% habitats)], 2, function(x) any(x, na.rm = TRUE) )))
      currentSelected$habitat <- if (is.null(input$exoten_habitat)) NULL else input$exoten_habitat[input$exoten_habitat %in% currentChoices$habitat]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_pw1, input$exoten_habitat, input$exoten_doe,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData5 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                     kingdom %in% results$exoten_filterKingdom() &
                                                     phylum %in% results$exoten_filterPhylum() &
                                                     class %in% results$exoten_filterClass() &
                                                     order %in% results$exoten_filterOrder() &
                                                     family %in% results$exoten_filterFamily() &
                                                     pathway_level1 %in% results$exoten_filterPw1() &
                                                     pathway_level2 %in% results$exoten_filterPw2() )
                                             
      filterData5 <- filterData5[apply(filterData5[, .SD, .SDcols = which(colnames(filterData5) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$degree_of_establishment <- unique(sort(filterData5$degree_of_establishment))
      currentSelected$degree_of_establishment <- if (is.null(input$exoten_doe)) NULL else input$exoten_doe[input$exoten_doe %in% currentChoices$degree_of_establishment]
      
    })

## Update choices and selected based on all other input filters for hierarchically lower variables 

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_doe, input$exoten_pw1,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData6 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                      kingdom %in% results$exoten_filterKingdom() &
#                                                      class %in% results$exoten_filterClass() &
#                                                      order %in% results$exoten_filterOrder() &
#                                                      family %in% results$exoten_filterFamily() &
                                                      pathway_level1 %in% results$exoten_filterPw1() &
                                                      pathway_level2 %in% results$exoten_filterPw2() &
                                                      degree_of_establishment %in% results$exoten_filterDoe() )
      
      filterData6 <- filterData6[apply(filterData6[, .SD, .SDcols = which(colnames(filterData6) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$phylum <- unique(sort(filterData6$phylum))
      currentSelected$phylum <- if (is.null(input$exoten_phylum)) NULL else input$exoten_phylum[input$exoten_phylum %in% currentChoices$phylum]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_doe, input$exoten_pw1,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData7 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                      kingdom %in% results$exoten_filterKingdom() &
                                                      phylum %in% results$exoten_filterPhylum() &
#                                                      order %in% results$exoten_filterOrder() &
#                                                      family %in% results$exoten_filterFamily() &
                                                      pathway_level1 %in% results$exoten_filterPw1() &
                                                      pathway_level2 %in% results$exoten_filterPw2() &
                                                      degree_of_establishment %in% results$exoten_filterDoe() )
      
      filterData7 <- filterData7[apply(filterData7[, .SD, .SDcols = which(colnames(filterData7) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$class <- unique(sort(filterData7$class))
      currentSelected$class <- if (is.null(input$exoten_class)) NULL else input$exoten_class[input$exoten_class %in% currentChoices$class]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_doe, input$exoten_pw1,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData8 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                      kingdom %in% results$exoten_filterKingdom() &
                                                      phylum %in% results$exoten_filterPhylum() &
                                                      class %in% results$exoten_filterClass() &
#                                                      family %in% results$exoten_filterFamily() &
                                                      pathway_level1 %in% results$exoten_filterPw1() &
                                                      pathway_level2 %in% results$exoten_filterPw2() &
                                                      degree_of_establishment %in% results$exoten_filterDoe() )
      
      filterData8 <- filterData8[apply(filterData8[, .SD, .SDcols = which(colnames(filterData8) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$order <- unique(sort(filterData8$order))
      currentSelected$order <- if (is.null(input$exoten_order)) NULL else input$exoten_order[input$exoten_order %in% currentChoices$order]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_doe, input$exoten_pw1,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData9 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                      kingdom %in% results$exoten_filterKingdom() &
                                                      phylum %in% results$exoten_filterPhylum() &
                                                      class %in% results$exoten_filterClass() &
                                                      order %in% results$exoten_filterOrder() &
                                                      pathway_level1 %in% results$exoten_filterPw1() &
                                                      pathway_level2 %in% results$exoten_filterPw2() &
                                                      degree_of_establishment %in% results$exoten_filterDoe() )
      
      filterData9 <- filterData9[apply(filterData9[, .SD, .SDcols = which(colnames(filterData9) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$family <- unique(sort(filterData9$family))
      currentSelected$family <- if (is.null(input$exoten_family)) NULL else input$exoten_family[input$exoten_family %in% currentChoices$family]
      
    })

observeEvent(c(input$exoten_bron, input$exoten_kingdom, input$exoten_habitat, input$exoten_doe, input$exoten_pw1,
               input$exoten_phylum, input$exoten_class, input$exoten_order, input$exoten_family, input$exoten_pw2), { 
      filterData10 <- subset(results$subExotenData(), source %in% results$exoten_filterBron() &
                                                      kingdom %in% results$exoten_filterKingdom() &
                                                      phylum %in% results$exoten_filterPhylum() &
                                                      class %in% results$exoten_filterClass() &
                                                      order %in% results$exoten_filterOrder() &
                                                      family %in% results$exoten_filterFamily() &
                                                      pathway_level1 %in% results$exoten_filterPw1() &
                                                      degree_of_establishment %in% results$exoten_filterDoe() )
      
      filterData10 <- filterData10[apply(filterData10[, .SD, .SDcols = which(colnames(filterData10) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
      
      currentChoices$pathway_level2 <- unique(sort(filterData10$pathway_level2))
      currentSelected$pathway_level2 <- if (is.null(input$exoten_pw2) | is.null(input$exoten_pw1)) NULL else input$exoten_pw2[input$exoten_pw2 %in% currentChoices$pathway_level2]
      
    })

### Final Data set
### ---------------

#results$exoten_selectedKingdom <- reactive({
#      
#      if (is.null(currentSelected$exoten_kingdom))
#        c(NA, unique(results$subExotenData()$kingdom)) else
#        currentSelected$exoten_kingdom
#      
#    })

results$exoten_data <- reactive({
      
      # filter for variables
      dataSub <- subset(results$subExotenData(), source %in% results$exoten_filterBron() & 
                                                  kingdom %in% results$exoten_filterKingdom() &
                                                  phylum %in% results$exoten_filterPhylum() &
                                                  class %in% results$exoten_filterClass() &
                                                  order %in% results$exoten_filterOrder() &
                                                  family %in% results$exoten_filterFamily() &
                                                  pathway_level1 %in% results$exoten_filterPw1() &
                                                  pathway_level2 %in% results$exoten_filterPw2() &
                                                  degree_of_establishment %in% results$exoten_filterDoe() )

      # filter for habitat logicals
      if (!is.null(input$exoten_habitat))
      dataSub <- dataSub[apply(dataSub[, .SD, .SDcols = which(colnames(dataSub) %in% results$exoten_filterHabitat() )], 1, function(x) any(x, na.rm = TRUE)), ]
                
      dataSub

      
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
      
      h2(paste("Aantal geïntroduceerde uitheemse soorten", 
              "in", ifelse(is.null(input$exoten_region), "België", vectorToTitleString(input$exoten_region)),
              "per jaar", 
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
              "in", ifelse(is.null(input$exoten_region), "België", vectorToTitleString(input$exoten_region)),
              yearToTitleString(req(input$exoten_time))
          )
      )
    })


callModule(module = plotModuleServer, id = "exoten_soortenCumulatiefPlot",
    plotFunction = "cumulativeIntroductionYear", 
    data = reactive(results$exoten_data())
)

## Plot number of species per year by native region

output$exoten_titleSoortenPerJaarPerOorsprongregio <- renderUI({      
      
      h2(paste("Aantal geïntroduceerde uitheemse soorten", 
              "in", ifelse(is.null(input$exoten_region), "België", vectorToTitleString(input$exoten_region)),
              "per jaar en per regio van oorsprong",
              yearToTitleString(req(input$exoten_time))
          )
      )
    })

callModule(module = optionsModuleServer, id = "exoten_soortenPerJaarPerOorsprongregio", 
    data = reactive(results$exoten_data()),
    types = reactive(c(
            "Continent van oorsprong" = "native_continent",
            "Regio van oorsprong" = "native_range"
        )), 
    labelTypes = "Detail niveau", 
    typesDefault = reactive("native_continent"))

callModule(module = plotModuleServer, id = "exoten_soortenPerJaarPerOorsprongregio",
    plotFunction = "countYearNativerange", 
    data = reactive(results$exoten_data())
)
