# Project: inbo-grofwildjacht_git
# 
# Author: sjunius
###############################################################################

#' Shiny module for creating the interactive tool for Afschot aanvraag - server side
#' @inheritParams countYearSchade
#' @inheritParams countAgeGenderServer
#' @inheritParams optionsModuleServer
#' @return no return value
#' 
#' @author sjunius
#' @import shiny
#' @export
requestAfschotReewildServer <- function(
  id, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      ns <- session$ns
      
      currentYear <- year(Sys.Date())
      pastYears <- c(currentYear-3, currentYear-2, currentYear-1)
      
      aanvraagTable <- reactive({
          
          data <- data()
          data <- data %>% mutate(
            type = dplyr::case_when(
              type_comp %in% c("Geitkits", "Bokkits") ~ "Kits",
              type_comp %in% c("Reebok", "Jaarlingbok") ~ "Bok",
              type_comp %in% c("Smalree", "Reegeit") ~ "Geit",
              TRUE ~ "Onbekend"
              ))
          data$type <- factor(data$type, levels = c("Bok", "Geit", "Kits"))
          
          df <- data %>% filter(!is.na(type), afschotjaar %in% pastYears) %>% 
            group_by(type, afschotjaar) %>% 
            summarise(n = dplyr::n()) 
          
          df_totals <- df %>% 
            group_by(afschotjaar) %>%
            summarise(n = sum(n), .groups = "drop") %>%
            mutate(type = "Totaal")
          
          gemiddeldTotaal <- mean(df_totals$n, na.rm = TRUE)
          
          df <- rbind(df, df_totals)
          
          table_data <- df %>%
            tidyr::pivot_wider(
              names_from = afschotjaar,
              values_from = n,
              values_fill = 0
            ) %>%
            mutate(
              gemiddeld = rowMeans(dplyr::across(where(is.numeric)), na.rm = TRUE),
              percentage = paste0(round((gemiddeld / gemiddeldTotaal) * 100, 2), "%")
            )
        })
  
      output$aanvraagAfschot <- renderUI({
          
          req(aanvraagTable())
          req(nrow(aanvraagTable()) > 0)
          
          df <- aanvraagTable()
          
          colnames(df) <- c("JAAR", pastYears, "GEMIDDELD GEREALISEERD", "PERCENTAGE")
          
          tagList(
            h3(paste0("Aanvraag ", currentYear + 1, "-", currentYear + 3)),
            renderTable({df}, width = "95%", sanitize.text.function = function(x) x)
            )
        })
      
      
      maxNbLabels <- reactive({
          req(input$toekenningsFactor)
          req(aanvraagTable())
          req(nrow(aanvraagTable()) > 0)
          
          round(aanvraagTable()$gemiddeld[aanvraagTable()$type == "Totaal"][[1]] * (input$toekenningsFactor), 0)
          
        })
      
      AssignedNbLabels <- eventReactive(c(input$nbLabelBok, input$nbLabelGeit, input$nbLabelKits), {
          req(input$nbLabelBok, input$nbLabelGeit, input$nbLabelKits)
          
          input$nbLabelBok + input$nbLabelGeit + input$nbLabelKits
          
        })
      
      output$summaryNbLabels <- renderUI({
          req(maxNbLabels())
          req(AssignedNbLabels())
          
          if (AssignedNbLabels() > maxNbLabels()) {
            output$warningMessage <- renderUI( tags$p(style = "color: red;", "De som van alle labels mag niet boven het maximum aantal labels liggen."))
          } else {
            output$warningMessage <- renderUI(NULL)
          }
          
          tagList(
            tags$label("Toegekende labels / Maximum aantal labels"),
            tags$br(),
            paste(AssignedNbLabels(), "/", maxNbLabels()),
            tags$br(),
            uiOutput(ns("warningMessage"))
          )
    
        })
      
      observe({
          req(maxNbLabels())
          req(AssignedNbLabels())
          req(input$nbLabelBok, input$nbLabelGeit, input$nbLabelKits)
          
          if (input$nbLabelBok < ceiling(0.2*maxNbLabels()) | input$nbLabelBok > floor(0.35*maxNbLabels())) {
            output$infoMessageBok <- renderUI( 
              tagList(
                tags$p(style = "color: red;", "Aantal labels moet binnen de grenzen liggen."),
                tags$p( round(input$nbLabelBok / AssignedNbLabels() * 100, 2) , "% van het totaal afschot"))
            )
          } else {
            output$infoMessageBok <- renderUI( 
              tagList(
                tags$p( round(input$nbLabelBok / AssignedNbLabels() * 100, 2) , "% van het totaal afschot"))
            )
          }
          
          if (input$nbLabelGeit < ceiling(0.2*maxNbLabels()) | input$nbLabelGeit > floor(0.3*maxNbLabels())) {
            output$infoMessageGeit <- renderUI( 
              tagList(
                tags$p(style = "color: red;", "Aantal labels moet binnen de grenzen liggen."),
                tags$p( round(input$nbLabelGeit / AssignedNbLabels() * 100, 2) , "% van het totaal afschot"))
            )
          } else {
            output$infoMessageGeit <- renderUI( 
              tagList(
                tags$p( round(input$nbLabelGeit / AssignedNbLabels() * 100, 2) , "% van het totaal afschot"))
            )
          }
          
          if (input$nbLabelKits < ceiling(0.4*maxNbLabels()) | input$nbLabelKits > floor(0.55*maxNbLabels())) {
            output$infoMessageKits <- renderUI( 
              tagList(
                tags$p(style = "color: red;", "Aantal labels moet binnen de grenzen liggen."),
                tags$p( round(input$nbLabelKits / AssignedNbLabels() * 100, 2) , "% van het totaal afschot"))
            )
          } else {
            output$infoMessageKits <- renderUI( 
              tagList(
                tags$p( round(input$nbLabelKits / AssignedNbLabels() * 100, 2) , "% van het totaal afschot"))
            )
          }
          
          
          
        })
      
      output$NbLabelsPerType <- renderUI({
          
          req(maxNbLabels())
          
          tagList(
            fixedRow(
              column(4, 
                wellPanel(
                  tagList(
                    tags$label("Bok"),
                    tags$p("Ondergrens: ", ceiling(0.2*maxNbLabels())),
                    tags$p("Bovengrens: ", floor(0.35*maxNbLabels())),
                    tags$br(),
                    numericInput(ns("nbLabelBok"), label = "Aantal labels", value = round(0.25*maxNbLabels()), step = 1),
                    uiOutput(ns("infoMessageBok"))
                  )
                )
              ),
              column(4, 
                wellPanel(
                  tagList(
                    tags$label("Geit"),
                    tags$p("Ondergrens: ", ceiling(0.2*maxNbLabels())),
                    tags$p("Bovengrens: ", floor(0.3*maxNbLabels())),
                    tags$br(),
                    numericInput(ns("nbLabelGeit"), label = "Aantal labels", value = round(0.25*maxNbLabels()), step = 1),
                    uiOutput(ns("infoMessageGeit"))
                  )
                )
              ),
              column(4, 
                wellPanel(
                  tagList(
                    tags$label("Kits"),
                    tags$p("Ondergrens: ", ceiling(0.4*maxNbLabels())),
                    tags$p("Bovengrens: ", floor(0.55*maxNbLabels())),
                    tags$br(),
                    numericInput(ns("nbLabelKits"), label = "Aantal labels", value = round(0.5*maxNbLabels()), step = 1),
                    uiOutput(ns("infoMessageKits"))
                  )
                )
              )
            )
          )
        })
      
      output$resultaatAanvraag <- renderUI({
          req(aanvraagTable())
          req(nrow(aanvraagTable()) > 0)
          
          totalRee <- aanvraagTable()$gemiddeld[aanvraagTable()$type == "Totaal"][[1]]
          calcFactor <- if (totalRee > 100) {
            1.15
          } else if (totalRee <= 100 & totalRee > 50) {
            1.2
          } else if (totalRee <= 50 & totalRee > 12) {
            1.25
          } else {
            0
          }
        
        
        tagList(
          h3("Resultaat"),
          
          fixedRow(
            column(6, numericInput(inputId = ns("toekenningsFactor"), label = "Toekenningsfactor", 
                min = 0, max = 2, step = 0.01, value = calcFactor)),
            column(6, uiOutput(ns("summaryNbLabels")))
          ),
          fixedRow(
            column(12, tags$em(style = "font-size: 0.9em;",
                "De toekenningsfactor wordt automatisch bepaald 
                  door het totaal afschot van de voorbije 3 jaar maar kan worden 
                  aangepast naar keuze tussen 0,1 en 2. Deze toekenningsfactor bedraagt 
                  standaard 1,15 voor afschot van >100 reeën, 1,20 voor afschot tussen 
                  50 en 100 reeën en 1,25 voor afschot tussen 12 en 50 reeën."))
          ),
          tags$br(),
          uiOutput(ns("NbLabelsPerType")),
          fixedRow(
            column(12, tags$em(style = "font-size: 0.9em;",
                "Een standaard verdeling van 25% bokken, 25% 
                  geiten en 50% kitsen wordt toegepast. Deze verdeling kan aangepast 
                  worden binnen onder- en bovengrenzen. Door het vastleggen van specifieke 
                  onder- en bovengrenzen wordt ervoor gezorgd dat de verhoudingen tussen de 
                  verschillende geslachten en leeftijdscategorieën gerespecteerd blijven. 
                  Het niet realiseren van een afschot in een bepaalde categorie vertaald zich immers 
                  in een daling van het toegekend afschot in alle categorieën in de volgende periode. 
                  Voor reegeit ligt de ondergrens op 20% en de bovengrens op 30%, voor bokken ligt 
                  deze tussen 20 en 35% en voor kitsen tussen 40 en 55%."))
          )
        )
      })
  
    })
  
} 



#' Shiny module for creating the interactive tool for Afschot aanvraag - UI side
#' @inherit welcomeSectionUI
#' @inheritParams getOutputDescription
#' @inheritParams optionsModuleUI
#' @inheritParams reportingGrofwild-common-args
#' @export
requestAfschotReewildUI <- function(id, 
  uiText, doHide = TRUE, context = strsplit(id, split = "_")[[1]][1]) {
  
  title <- getOutputTitle(
    output = "afschotAanvraagReewild", specie = "Ree", 
    uiText = uiText)
  description <- getOutputDescription(
    output = "afschotAanvraagReewild", 
    specie = "Ree", uiText = uiText,
    context = context)
  
  ns <- NS(id)
  
  tagList(
    actionLink(inputId = ns("linkAfschotAanvraag"), label = h2(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkAfschotAanvraag % 2 ==", as.numeric(doHide)), 
      ns = ns,
      
      tagList(
        fixedRow(
          column(12, tags$p(HTML(description)))
        ),
        fixedRow(
          column(12, uiOutput(ns("aanvraagAfschot")))
        ),
        fixedRow(
          column(12, uiOutput(ns("resultaatAanvraag")))
        )
      )
    ),
    tags$br()
  )
}
