
# data <- everGeoAll[provincie == "Antwerpen"]
#' kencijfers summary table. The summary table of number of municipalities for two consecutive years. Filter is applied to the observation and shot
#' data. Municipality is retained in the summary if there is any animal shot.
#' @param data The geo data contains afschot and waarnemingendata -> already aggregated per 
#' region
#' @param bron data source, it should be one of the \code{"waarnemingen.be", "afschot", "both"}
#' @param jaar the year for which summary of municipalities' observed and shot animal stats
#' @param  thresholdWaarnemingen threshold for number of animals observed, needs to be specified if \code{"bron"}
#' is \code{"waarnemingen.be"} or \code{"both"}
#' @param  thresholdAfschot threshold for number of animals shot, needs to be specified if \code{"bron"}
#' is \code{"afschot"} or \code{"both"}
#' @return A list contains the summary table and the observed mulnicipalities 
#' @import data.table
#' @author yzhang
#' @export
#' @importFrom stats na.exclude

tabelKencijfers <- function(data, 
  jaar = 2023, 
  bron = c("both", "waarnemingen.be", "afschot"),
  ns = NULL,
  thresholdWaarnemingen = 0,
  thresholdAfschot = 0){
  
  # For R CMD check
  gemeente_afschot_locatie <- afschotjaar <- dataSource <- aantal <- NULL
  
  relevantColumns <- c("afschotjaar", "provincie", "gemeente_afschot_locatie", "dataSource", "aantal")
  stopifnot(relevantColumns %in% colnames(data))
  bron <- match.arg(bron)
  
  data <- data[!is.na(gemeente_afschot_locatie)]
  
  yearRange <- range(data$afschotjaar)
  
  if (jaar == yearRange[1])
    dataSubset <- data[afschotjaar == jaar] else 
    dataSubset <- data[afschotjaar %in% c(jaar, jaar-1)]
  
  dataSubset <- unique(dataSubset[, relevantColumns, with = FALSE])
  
#  # Redundant -> input data should already be aggregated
#  # Sum per gemeente and dataSource
#  dataSubset <- dataSubset[, .(aantal = sum(aantal)), 
#    by = .(afschotjaar, gemeente_afschot_locatie, dataSource)]
  
  dataSubset <- if (bron == "both")
      dataSubset[(dataSource == "afschot" & aantal >= thresholdAfschot) | 
          (dataSource == "waarnemingen.be" & aantal >= thresholdWaarnemingen)] else if (bron == "afschot")
      dataSubset[(dataSource == "afschot" & aantal >= thresholdAfschot)] else
      dataSubset[dataSource == "waarnemingen.be" & (aantal >= thresholdWaarnemingen)]
  
  dataCurrentYear <- dataSubset[afschotjaar == jaar]
  
  observedCities <- dataCurrentYear[dataSource != "afschot"][["gemeente_afschot_locatie"]]
  afschotCities <-  dataCurrentYear[dataSource == "afschot"][["gemeente_afschot_locatie"]]
  
  resultTable <- data.frame(
    categorie = "Totaal aantal gemeentes", 
    aantal_categorie = length(unique(dataCurrentYear[["gemeente_afschot_locatie"]])),
    gemeente = NA
  )
  
  # create current year summary table
  
  if (jaar > yearRange[1]) {
    
    dataPreYear <- dataSubset[afschotjaar == jaar-1]
    
    common <- unique(intersect(dataCurrentYear[["gemeente_afschot_locatie"]],
        dataPreYear[["gemeente_afschot_locatie"]]))
    new <- unique(na.exclude(setdiff(dataCurrentYear[["gemeente_afschot_locatie"]],
          dataPreYear[["gemeente_afschot_locatie"]])))
    old <- unique(na.exclude(setdiff(dataPreYear[["gemeente_afschot_locatie"]],
          dataCurrentYear[[ "gemeente_afschot_locatie"]])))
    
    resultTable <- rbind(resultTable,  
      data.frame(
        categorie = "Dezelfde gemeentes", 
        aantal_categorie = length(common), 
        gemeente = if (length(common) == 0) NA else sort(common)
      ),
      data.frame(
        categorie = "Nieuwe gemeentes", 
        aantal_categorie = length(new), 
        gemeente = if (length(new) == 0) NA else sort(new)
      ),
      data.frame(
        categorie = "Niet meer in deze gemeentes", 
        aantal_categorie = length(old), 
        gemeente = if (length(old) == 0) NA else sort(old))
    )
    
  } else {
    
    if (nrow(dataCurrentYear) != 0){
      
      resultTable <- rbind(resultTable, data.frame(
          categorie = "Dezelfde gemeentes",
          aantal_categorie = length(unique(dataCurrentYear[["gemeente_afschot_locatie"]])), 
          gemeente = sort(unique(dataCurrentYear[["gemeente_afschot_locatie"]])))
      )
    }
    
  }
  
  resultTable <- as.data.frame(resultTable)
  resultTable$categorie <- factor(resultTable$categorie, 
    levels = unique(resultTable$categorie))
  
  ## current year provincie table 

  if( nrow(dataCurrentYear) > 0 ){
    tableCount <- data.table::dcast(dataCurrentYear, gemeente_afschot_locatie ~dataSource, value.var = "aantal", fill = NA, drop = FALSE, fun.agg = sum)
    
    finalTable <-  merge(resultTable, tableCount, 
      by.x = "gemeente", by.y = "gemeente_afschot_locatie", all.x = TRUE, sort = FALSE)
    
    # sort back to original order
    finalTable <- finalTable[order(finalTable$categorie, finalTable$gemeente),]
    
    if ("waarnemingen.be" %in% colnames(finalTable))
      colnames(finalTable)[colnames(finalTable) == "waarnemingen.be"] <- "waarnemingen" else
      finalTable$waarnemingen <- NA      
      
    finalTable <- finalTable[, c("categorie", "aantal_categorie", "gemeente", "afschot", "waarnemingen")]
        
  }else{
    finalTable <- resultTable 
  }
        
  rownames(finalTable) <- NULL
  
  
  # Format table
  resTable <- finalTable[,c("categorie", "aantal_categorie", "gemeente")]
  resTable[,1] <- paste(  resTable[,"categorie"], resTable[,"aantal"], sep = ": ")
  
  cityList <-  na.omit(resTable[,"gemeente"])
  
  colorList <- ifelse((cityList %in% observedCities) & (! cityList %in% afschotCities),
    "#ef8a62",
    ifelse(
      (cityList %in% observedCities) & ( cityList %in% afschotCities), 
      "transparent", "#67a9cf") )
  names(colorList) <- cityList
  
  formattedTable <- DT::datatable(
    resTable[,c(1,3), drop = FALSE],
    colnames = c("", ""),
    rownames = FALSE,
    extensions = 'RowGroup',
    options = list(
      rowGroup = list(dataSrc = 0),
      pageLength = nrow(resTable),
      columnDefs = list(list(visible=FALSE, targets=0)),
      dom = 't',
      striped = FALSE
    ),
    selection = 'none',
    callback = JS(
      "table.on('click', 'tr.dtrg-group', function () {",
      "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
      "  $(rowsCollapse).toggleClass('hidden');",
      "});",
      paste0("table.one('init', () => $(' #", ns("kencijfer_table"),
        " .dtrg-group').trigger('click'))")
    )
  )
  
  if (length(cityList) > 0 & bron == "both")
    formattedTable <- formatStyle(formattedTable,
      columns = 2,
      valueColumns = 2,
      backgroundColor = styleEqual(unique(cityList), colorList)
    )
  
  
  return(
    list(
      table = formattedTable,
      data = finalTable
    ))
  

}


#' Kencijfer table module UI
#' @inherit welcomeSectionUI
#' @author yzhang
#' @export
kencijferModuleUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  uiText <- uiText[uiText$plotFunction == paste(strsplit(id, "_")[[1]][-1], collapse = "_"), ]
  
  tagList(
    
    actionLink(inputId = ns("linkKencijferTabel"), 
      label = paste("TABEL:", uiText$title), class = "action-h3"),
    
    conditionalPanel(
      condition = "input.linkKencijferTabel % 2 == 0", ns = ns,
      fixedRow(
        tags$p(HTML(decodeText(text = uiText$dash))),
        column(
          4,
          wellPanel(
            uiOutput(ns("kencijferFilter")),
            conditionalPanel(
              condition = "input.dataSource_kencijfer.indexOf('waarnemingen.be') > -1", ns = ns,
              uiOutput(ns("sliderObserve"))
            
            ),
            conditionalPanel(
              condition = "input.dataSource_kencijfer && input.dataSource_kencijfer.includes('afschot')", ns = ns,
              uiOutput(ns("sliderAfschot"))
            )
          )
        ),
        column(
          8,
          tags$div(
            style = "margin-bottom: 10px",
            withSpinner(DT::dataTableOutput(ns("kencijfer_table")))
          ),
          downloadButton(ns("dataDownload"), "Download data", class = "downloadButton")
        )
      ),
      tags$hr()
    )
  )
  
}

#' kencijfer table module server
#' @inheritParams optionsModuleServer 
#' @param kencijfersData geo data for given region
#' @param species a reactive value of the name of the animal species
#' @inheritParams kencijferModuleUI
#' @import shiny
#' @importFrom DT JS formatStyle styleEqual
#' @author yzhang
#' @export

kencijferModuleServer <- function(id, input, output, session, kencijfersData, 
  species){
  
  results <- reactiveValues(
    observeThreshold = 1, 
    shotThreshold = 1
  )
  
  moduleServer(id,
    
    function(input, output, session) {
      
      ns <- session$ns
      
          output$kencijferFilter <- renderUI({
          
            req(kencijfersData())    
              dataSource <- unique(kencijfersData()$dataSource)
              names(dataSource) <- gsub("\\..+", "", dataSource )
              
              tagList(
                div(class = "sliderBlank",
                  sliderInput(inputId = ns("jaar_kencijfer"), label = "Jaar",
                    value = max(kencijfersData()$afschotjaar, na.rm = TRUE),
                    min = min(kencijfersData()$afschotjaar, na.rm = TRUE),
                    max = max(kencijfersData()$afschotjaar, na.rm = TRUE),
                    step = 1,
                    sep = "")),
                
                selectInput(inputId = ns("dataSource_kencijfer"),
                  label = "Data bron",
                  selected = dataSource,
                  choices =  dataSource,
                  multiple = TRUE)
              )
            })
          
          output$sliderObserve <- renderUI({
              
            req(kencijfersData())
              sliderInput(
                inputId = ns("thresholdWaarnemingen"),
                label = "Waarnemingen drempel",
                value = results$observeThreshold,
                min = 1,
                max = 10,
                step = 1,
                sep = ""
              )
            })
          
          
          output$sliderAfschot <- renderUI({
              maxSchot <- max(10,  max(kencijfersData()[(dataSource == "afschot") &(afschotjaar ==  input$jaar_kencijfer), "aantal"], na.rm = TRUE))
              
              sliderInput(
                inputId = ns("thresholdAfschot"),
                label = "Afschot drempel",
                value = min(results$shotThreshold, maxSchot),
                min = 1,
                max =  maxSchot,
                step = 1,
                sep = ""
              )
            })
  
      
     observeEvent(input$jaar_kencijfer, {
          
          req(input$thresholdAfschot)
          
          if (results$shotThreshold != input$thresholdAfschot)
            results$shotThreshold <- input$thresholdAfschot
          if (results$observeThreshold != input$thresholdWaarnemingen) 
            results$observeThreshold <- input$thresholdWaarnemingen
          
        })
      
      results$res <- reactive({
          
          req(input$dataSource_kencijfer)
          req(input$jaar_kencijfer)
          
          tabelKencijfers(
            data = req(kencijfersData()), 
            jaar = as.numeric(input$jaar_kencijfer),
            bron = ifelse(length(input$dataSource_kencijfer) == 2, "both", input$dataSource_kencijfer),
            thresholdWaarnemingen = input$thresholdWaarnemingen,
            thresholdAfschot = input$thresholdAfschot,
            ns = ns
          ) 
          
        })
      
      output$kencijfer_table <- DT::renderDataTable(
        results$res()$table
      )
     
      
      ## download button 
      output$dataDownload <- downloadHandler(
        
        filename = function()
          nameFile(content = "kencijfer", species = species(), 
            year = input$jaar_kencijfer, fileExt = "csv"),
        content = function(file)
          write.csv(results$res()$data, file, row.names = FALSE)
      )
      
      
      return(reactive(results$res()))
    })

}
