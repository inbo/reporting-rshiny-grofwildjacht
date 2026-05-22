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
#' @importFrom shinyjs runjs
#' @importFrom reactable renderReactable reactable reactableOutput colDef
#' @importFrom tidyselect where
#' @export
requestAfschotReewildServer <- function(
  id, data) {
  
  moduleServer(id,
    function(input, output, session) {
      
      # For R CMD check
      type <- afschotjaar <- n <- gemiddeld <- NULL
      
      ns <- session$ns
      
 
      today <- Sys.Date()
      cutoff_date <- as.Date(paste0(format(today, "%Y"), "-10-15"))
      
      # If today is after this year's cutoff, make aanvraag for other period
      currentYear <- year(Sys.Date())
      if (today <= cutoff_date) {
        pastYears <- c(currentYear - 3, currentYear - 2, currentYear - 1)
        nextYears <- c(currentYear, currentYear + 1, currentYear + 2)
      } else {
        pastYears <- c(currentYear - 2, currentYear - 1, currentYear)
        nextYears <- c(currentYear + 1, currentYear + 2, currentYear + 3)
      }
      
      # Initialize reactive data from your aanvraagTable
      table_data <- reactiveVal(NULL)
      updateTable <- reactiveVal(0)
      isClearing <- reactiveVal(FALSE)
      
      observe({
         
          data <- data()
          data <- data %>% mutate(
            type = dplyr::case_when(
              type_comp %in% c("Geitkits", "Bokkits") ~ "Kits",
              type_comp %in% c("Reebok", "Jaarlingbok") ~ "Reebok",
              type_comp %in% c("Smalree", "Reegeit") ~ "Reegeit",
              TRUE ~ "Onbekend"
              ))
          data$type <- factor(data$type, levels = c("Reebok", "Reegeit", "Kits"))
          
          df <- data %>% filter(!is.na(type), afschotjaar %in% pastYears) %>% 
            group_by(type, afschotjaar) %>% 
            summarise(n = dplyr::n()) 
          
          req(nrow(df) > 0)
          
          if (!startsWith(id, "wbe")) {  # Leave input values empty for public page
            df$n <- 0
          }
          
          df_totals <- df %>% 
            group_by(afschotjaar) %>%
            summarise(n = sum(n), .groups = "drop") %>%
            mutate(type = "Totaal")
          
          gemiddeldTotaal <- mean(df_totals$n, na.rm = TRUE)
          
          df <- rbind(df, df_totals)
          
          fullTable <- df %>%
            tidyr::pivot_wider(
              names_from = afschotjaar,
              values_from = n,
              values_fill = 0
            ) %>%
            mutate(
              gemiddeld = rowMeans(dplyr::across(where(is.numeric)), na.rm = TRUE),
              percentage = paste0(round((gemiddeld / gemiddeldTotaal) * 100, 2), "%")
            )
          
          table_data(fullTable)
        })
      
      
      # Create editable table
      output$aanvraagAfschotTable <- renderReactable({
          data <- table_data()
          if (is.null(data)) return(NULL)
          
          year_cols <- as.character(pastYears)
          
          col_defs <- list(
            type = colDef(name = "JAAR")
          )
          
          # Add year columns as editable for first 3 rows
          for (col in year_cols) {
            col_defs[[col]] <- colDef(
              name = col,
              cell = function(value, index, col = col) {
                if (!startsWith(id, "wbe") && index <= 3) {  # Only first 3 rows on public page are editable
                  tags$input(
                    type = "number",
                    value = value,
                    id = ns(paste0("cell_", index, "_", col)),
                    onblur = sprintf(
                      "Shiny.onInputChange('%s', this.value); Shiny.onInputChange('%s', Math.random());",
                      ns(paste0("cell_", index, "_", col)),
                      ns("updateTable")
                    ),
                    style = "width: 100%; padding: 5px;"
                  )
                } else {
                  value
                }
              }
            )
          }
          
          col_defs$gemiddeld <- colDef(
            name = "GEMIDDELD GEREALISEERD",
            cell = function(value) round(value, 2)
          )
          col_defs$percentage <- colDef(name = "PERCENTAGE")
          
          reactable(
            data,
            columns = col_defs,
            striped = FALSE,
            highlight = TRUE
          )
        })
      
        
        # Update calculated values whenever inputs change
        observeEvent(input$updateTable, {
            data <- table_data()
            year_cols <- as.character(pastYears)
            
            for (row in 1:3) {
              for (col in year_cols) {
                input_id <- paste0("cell_", row, "_", col)
                
                if (!is.null(input[[input_id]])) {
                  new_value <- suppressWarnings(as.numeric(input[[input_id]]))
                  if (!is.na(new_value)) {
                    data[[col]][row] <- new_value
                  }
                }
              }
            }
            
            for (i in 1:3) {
              values <- as.numeric(data[i, year_cols])
              data$gemiddeld[i] <- round(mean(values, na.rm = TRUE), 2)
            }
            
            if (nrow(data) == 4) {
              for (col in year_cols) {
                data[[col]][4] <- sum(as.numeric(data[[col]][1:3]), na.rm = TRUE)
              }
              data$gemiddeld[4] <- round(mean(as.numeric(data[4, year_cols]), na.rm = TRUE), 2)
            }
            
            # Calculate percentages (using total average as base = 100%)
            totaal_average <- data$gemiddeld[data$type == "Totaal"]
            if (length(totaal_average) > 0 && totaal_average > 0) {
              for (i in 1:nrow(data)) {
                data$percentage[i] <- paste0(round((data$gemiddeld[i] / totaal_average) * 100, 2), "%")
              }
            }
            
            table_data(data)
          })
        
          
          # Clear the input fields
          observeEvent(input$clearInput, {              
              data <- table_data()
              year_cols <- as.character(pastYears)
              
              for (row in 1:4) {
                for (col in year_cols) {
                  data[[col]][row] <- 0
                  
                  shinyjs::runjs(paste0("Shiny.onInputChange('", ns(paste0("cell_", row, "_", col)),"', '0');"))
                }
              }
              
              data[, "gemiddeld"] <- 0
              data[, "percentage"] <- "0%"
              
              table_data(data)
              
            })
        
      
      output$aanvraagAfschot <- renderUI({          
          tagList(
            h3(paste0("Aanvraag ", nextYears[1], "-", nextYears[3])),
            reactableOutput(ns("aanvraagAfschotTable"))
            )
        })
      
      output$export <- downloadHandler(
        
        filename = function() {
          paste0(
            "aanvraag_afschot_",
            Sys.Date(),
            ".csv"
          )
        },
        
        content = function(file) {
          write.csv(
            table_data(),
            file,
            row.names = FALSE,
            fileEncoding = "UTF-8"
          )
        }
      )
      
      observeEvent(input$importAanvraag, {
          
          req(input$importAanvraag)
          
          tryCatch({
              df <- read.csv(
                input$importAanvraag$datapath,
                stringsAsFactors = FALSE,
                check.names = FALSE,
                fileEncoding = "UTF-8"
              )
            },
            error = function(e) {
              showNotification("Het is niet mogelijk om dit bestand correct te laden. Gelieve een ander bestand te selecteren.", type = "error", duration = 5)
              return()
            }
          )
          
          
          # Basic validation
          required_cols <- colnames(table_data())
          
          if (!all(required_cols %in% colnames(df)) | nrow(df) != 4)
            showNotification("Het format van de ge\u00EFmporteerde bestand is niet correct. Gelieve het format over te nemen van het ge\u00EBxporteerde bestand. ", type = "error", duration = 5)
          else 
            table_data(df)
        })
      
      
      startCalculations <- reactiveVal(0)
      
      observeEvent(table_data(), {
          req(startsWith(id, "wbe"))
          startCalculations(startCalculations() + 1)
        })
      observeEvent(input$calculate, startCalculations(startCalculations() + 1))
      
      maxNbLabels <- reactive({
          startCalculations()
          req(input$toekenningsFactor)
          req(isolate(table_data()))
          req(nrow(isolate(table_data())) > 0)
          
          isolate(round(table_data()$gemiddeld[table_data()$type == "Totaal"][[1]] * (input$toekenningsFactor), 0))
          
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
          
          if (input$nbLabelBok < round(0.2*maxNbLabels()) | input$nbLabelBok > round(0.35*maxNbLabels())) {
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
          
          if (input$nbLabelGeit < round(0.2*maxNbLabels()) | input$nbLabelGeit > round(0.3*maxNbLabels())) {
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
          
          if (input$nbLabelKits < round(0.4*maxNbLabels()) | input$nbLabelKits > round(0.55*maxNbLabels())) {
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
                    tags$label("Reebok"),
                    tags$p("Ondergrens: ", round(0.2*maxNbLabels())),
                    tags$p("Bovengrens: ", round(0.35*maxNbLabels())),
                    tags$br(),
                    numericInput(ns("nbLabelBok"), label = "Aantal labels", value = round(0.25*maxNbLabels()), step = 1),
                    uiOutput(ns("infoMessageBok"))
                  )
                )
              ),
              column(4, 
                wellPanel(
                  tagList(
                    tags$label("Reegeit"),
                    tags$p("Ondergrens: ", round(0.2*maxNbLabels())),
                    tags$p("Bovengrens: ", round(0.3*maxNbLabels())),
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
                    tags$p("Ondergrens: ", round(0.4*maxNbLabels())),
                    tags$p("Bovengrens: ", round(0.55*maxNbLabels())),
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
          startCalculations()
          req(isolate(table_data()))
          req(nrow(isolate(table_data())) > 0)
          
          totalRee <- isolate(table_data()$gemiddeld[table_data()$type == "Totaal"][[1]])
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
                  standaard 1,15 voor afschot van >100 ree\u00EBn, 1,20 voor afschot tussen 
                  50 en 100 ree\u00EBn en 1,25 voor afschot tussen 12 en 50 ree\u00EBn."),
            if (totalRee < 12) {
              tags$p(
              style = "font-size: 0.9em; color: orange; margin-top: 8px;",
              "Het totaal gemiddeld afschot van de laatste 3 jaar is lager dan 12. Het is niet mogelijk om een toekenningsfactor toe te kennen."
              )
            })
          ),
          tags$br(),
          uiOutput(ns("NbLabelsPerType")),
          fixedRow(
            column(12, tags$em(style = "font-size: 0.9em;",
                "Een standaard verdeling van 25% bokken, 25% 
                  geiten en 50% kitsen wordt toegepast. Deze verdeling kan aangepast 
                  worden binnen onder- en bovengrenzen. Door het vastleggen van specifieke 
                  onder- en bovengrenzen wordt ervoor gezorgd dat de verhoudingen tussen de 
                  verschillende geslachten en leeftijdscategorie\u00EBn gerespecteerd blijven. 
                  Het niet realiseren van een afschot in een bepaalde categorie vertaald zich immers 
                  in een daling van het toegekend afschot in alle categorie\u00EBn in de volgende periode. 
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
        if (!startsWith(id, "wbe"))
          fixedRow(
            column(2, actionButton(ns("calculate"), "Berekenen", width = "100%", style = "color: #fff; background-color: steelblue; margin-top: 25px;")),
            column(2, actionButton(ns("clearInput"), "Leegmaken", width = "100%", icon = tags$i(class = "fa fa-trash", style = "color:#fff;"), style = "color: #fff; background-color: brown; margin-top: 25px;")),
            column(2, offset = 3, downloadButton(ns("export"), "Exporteer aanvraag", width = "100%", icon = tags$i(class = "fa fa-download", style = "color:white;"), style = "color: #fff; background-color: steelblue; margin-top: 25px;")),
            column(3, fileInput(ns("importAanvraag"),"Importeer aanvraag", accept = c(".csv"), width = "100%", buttonLabel = "Bladeren", placeholder = "Kies een CSV-bestand"))
          ),
        fixedRow(
          column(12, uiOutput(ns("resultaatAanvraag")))
        )
      )
    ),
    tags$br()
  )
}
