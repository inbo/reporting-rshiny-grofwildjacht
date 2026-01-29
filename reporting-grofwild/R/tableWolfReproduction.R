#' Shiny module for creating the wolf Reproduction table - UI side
#' @inheritParams reportingGrofwild-common-args
#' @inheritParams getOutputTitle
#' @export
tableWolfReproductionUI <- function(id, uiText, specie = NULL, doHide = TRUE, context = "description") {
  
  ns <- NS(id)
  
  title <- getOutputTitle(
    output = "tableWolfReproductionUI", specie = specie, uiText = uiText
  )
  description <- getOutputDescription(
    output = "tableWolfReproductionUI", 
    specie = specie, uiText = uiText, context = context
  )
  
  tagList(
    tags$head(
      tags$style(HTML("
            .highlight {
            background-color: #e6e6e6;
            font-weight: 600;
            border-radius: 4px;
            padding: 2px 6px;
            }
            table caption {
            text-align: left;
            margin-bottom: 6px;
            }
            "))
    ),
    actionLink(inputId = ns("linkTableWolfReproduction"), label = tags$h3(HTML(title))),
    conditionalPanel(
      condition = paste("input.linkTableWolfReproduction % 2 ==", as.numeric(doHide)),
      ns = ns,
      tagList(
        uiOutput(ns("tables")),
        tags$br(),
        tags$div(class = "larger-description", HTML(description)),
        tags$hr()
      )
    )
  )

}

#' Shiny module for creating the wolf Reproduction table - server side
#' @inheritParams dataModuleServer
#' @inheritParams reportingGrofwild-common-args
#' 
#' @return no return value
#' @import shiny
#' @importFrom dplyr filter mutate group_by group_split
#' @export
tableWolfReproductionServer <- function(id, data,
  preSelected = reactive(NULL)) {
  
  moduleServer(id,
    function(input, output, session) {
      
      output$tables <- renderUI({
          req(!is.null(preSelected()$time()))
          
          years <- preSelected()$time()[1]:preSelected()$time()[2]
          terrData <- data %>%
            filter(Jaar %in% years) %>%
            mutate(
              female_change = as.logical(female_change),
              male_change = as.logical(male_change)
            ) %>%
            group_by(Territorium) %>%
            group_split()
          
          table_columns <- lapply(terrData, function(df) {
              
              territorium_name <- unique(df$Territorium)
              
              table_html <- tags$table(
                class = "table table-striped table-condensed",
                tags$caption(
                  style = "caption-side: top; font-weight: bold;",
                  paste("Territorium:", territorium_name)
                ),
                tags$thead(
                  tags$tr(
                    tags$th("Jaar"),
                    tags$th("Vrouwtje"),
                    tags$th("Mannetje"),
                    tags$th("Aantal welpen")
                  )
                ),
                tags$tbody(
                  lapply(seq_len(nrow(df)), function(i) {
                      tags$tr(
                        tags$td(df$Jaar[i]),
                        tags$td(
                          class = if (df$female_change[i]) "highlight" else NULL,
                          df$Vrouwtje[i]
                        ),
                        tags$td(
                          class = if (df$male_change[i]) "highlight" else NULL,
                          df$Mannetje[i]
                        ),
                        tags$td(
                          df$Welpen[i]
                        )
                      )
                    })
                )
              )
              
              column(width = 6, table_html)
            })
          
          fluidRow(table_columns)
        })
    })
  
} 
