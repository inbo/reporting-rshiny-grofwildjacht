
# data <- everGeoAll[provincie == "Antwerpen"]
#' kencijfers summary table. The summary table of number of municipalities for two consecutive years. Filter is applied to the observation and shot
#' data. Municipality is retained in the summary if there is any animal shot.
#' @param data The geo data contains aschot and waarnemingendata
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
#' 

tabelKencijfers <- function(data, 
                            jaar = 2023, 
                            bron = c("both", "waarnemingen.be","afschot"),
                            thresholdWaarnemingen = 0,
                            thresholdAfschot = 0){
  
  releventColumns <- c("afschotjaar", "provincie", "gemeente_afschot_locatie", "dataSource", "aantal")
  stopifnot(releventColumns %in% colnames(data))
  bron <- match.arg(bron)
  
  data <- data[!is.na(gemeente_afschot_locatie)]
  
  yearRange <- range(data$afschotjaar)
  
  if(jaar == yearRange[1]) dataSubset <- data[afschotjaar == jaar]  else dataSubset <- data[afschotjaar %in% c(jaar, jaar-1)]
  
  dataSubset <- unique(dataSubset[, releventColumns, with = FALSE])
  
  if(bron == "both"){
    
    dataSubset <- dataSubset[ (dataSource == "afschot" & aantal >= thresholdAfschot ) | (dataSource == "waarnemingen.be" & aantal >=   thresholdWaarnemingen) ]
 
   } else if(bron == "afschot"){
    
    dataSubset <- dataSubset[(dataSource == "afschot" & aantal >= thresholdAfschot)]
    
  }else{
    
    dataSubset <- dataSubset[dataSource == "waarnemingen.be" & (aantal >=   thresholdWaarnemingen)]
    
  }
  
  dataCurrentYear <-   dataSubset[afschotjaar == jaar]
  
  observedCities <- dataCurrentYear[dataSource != "afschot"][["gemeente_afschot_locatie"]]
  afschotCities <-  dataCurrentYear[dataSource == "afschot"][["gemeente_afschot_locatie"]]
  
  resultTable <- cbind("Totaal aantal gemeentes", length( unique( dataCurrentYear[["gemeente_afschot_locatie"]])),length( unique( dataCurrentYear[["gemeente_afschot_locatie"]])) )
  # create current year summary table
  
  if(jaar > yearRange[1]){
    
    dataPreYear <-   dataSubset[afschotjaar == jaar-1]
    
    common <- intersect(dataCurrentYear[[ "gemeente_afschot_locatie"]],dataPreYear[["gemeente_afschot_locatie"]]) |> unique()
    new <- setdiff(dataCurrentYear[[ "gemeente_afschot_locatie"]],dataPreYear[["gemeente_afschot_locatie"]]) |> na.exclude() |> unique()
    old <-  setdiff(dataPreYear[["gemeente_afschot_locatie"]], dataCurrentYear[[ "gemeente_afschot_locatie"]]) |> na.exclude()|> unique()
    
    resultTable <- rbind(  resultTable,  if(length(common) != 0) cbind("Dezelfde gemeentes",length(common), sort(common)) else cbind("Dezelfde gemeentes", 0, NA),
                           if(length(new) != 0) cbind("Nieuwe gemeentes", length( new ), sort(new)) else cbind("Nieuwe gemeentes", 0, NA),
                           if(length(old) != 0) cbind("Niet meer in deze gemeentes", length(old), sort(old)) else cbind("Niet meer in deze gemeentes", 0, NA)
    )
    
  }else{
    if(nrow(dataCurrentYear) != 0){
      resultTable <- rbind( resultTable, cbind("Dezelfde gemeentes",
                                               length(dataCurrentYear[["gemeente_afschot_locatie"]]), 
                                               sort( dataCurrentYear[["gemeente_afschot_locatie"]])))
    }
    
  }
  
  colnames(resultTable) <- c("category", "count", "municipality")
  resultTable <- as.data.frame(resultTable)
  
  ## current year provincie table 
 if( nrow(dataCurrentYear) > 0 ){
  tableCount <- data.table::dcast(dataCurrentYear, gemeente_afschot_locatie ~dataSource, value.var = "aantal", fill = 0)
 
  
  finalTable <-  merge(resultTable, tableCount, 
                       by.x = "municipality", by.y = "gemeente_afschot_locatie", all.x = TRUE, sort = FALSE)
  
  # sort back to original order
  finalTable <- finalTable[match( resultTable$municipality,   finalTable$municipality),]
 
  avaliableCols <- intersect(c("category", "count","municipality", "afschot", "waarnemingen.be"), grep(paste(c("category", "count","municipality", "afschot", "waarnemingen.be"), collapse = "|"),colnames( finalTable), value = TRUE)
  )
  finalTable <-  finalTable[, avaliableCols ]
 
  }else{
   finalTable <-    resultTable 
 }
  return( list("table" =  finalTable, "observed" = observedCities, "shot" = afschotCities  ))
}


#' kencijfer table module UI
#' @param id ID
#' @import shiny
#' @author yzhang
#' @export

kencijferModuleUI <- function(id) {
       
  ns <- NS(id)
  tagList(
    
    actionLink(inputId = "linkKencijferTabel", label = "Tabel kencijfers", class = "action-h3"),
  
    conditionalPanel(
      condition = "input.linkKencijferTabel % 2 == 0",
      fixedRow(
        uiOutput(ns("description")),
        column(
          4,
          wellPanel(
            uiOutput(ns("kencijferFilter")),
            conditionalPanel(
              condition = "input.dataSource_kencijfer.indexOf('waarnemingen.be') > -1", ns = ns,
              sliderInput(
                inputId = ns("thresholdWaarnemingen"),
                label = "Waarnemingen drempel",
                value = 1,
                min = 1,
                max = 10,
                step = 1,
                sep = ""
              ),
              
            ),
            conditionalPanel(
              condition = "input.dataSource_kencijfer.indexOf('afschot') > -1", ns = ns,
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
#' @param uiText data.frame
#' @inheritParams kencijferModuleUI
#' @import shiny
#' @importFrom DT JS formatStyle styleEqual
#' @author yzhang
#' @export

kencijferModuleServer <- function(id, input, output, session, kencijfersData, species, uiText){
 
   results <- reactiveValues()
 
  moduleServer(id,
               function(input, output, session) {
                 
                 ns <- session$ns
                 
                 observe({
                   
                
                   req(kencijfersData)
                   
                   output$kencijferFilter <- renderUI({
                     
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
                   
                   updateActionLink(session = session, inputId = "linkKencijferTabel",
                                    label = uiText[uiText$plotFunction == "F18_8",  "title"])
                   
                   
                   output$sliderAfschot <- renderUI({
                     
                     sliderInput(
                       inputId = ns("thresholdAfschot"),
                       label = "Afschot drempel",
                       value = 1,
                       min = 1,
                       max = max(10,max(kencijfersData()[(dataSource == "afschot") &(afschotjaar == input$jaar_kencijfer), "aantal"], na.rm = TRUE)),
                       step = 1,
                       sep = ""
                     )
                   })
                   
                 })
                 
                 
              
                 
                 observe({

                    req(input[["dataSource_kencijfer"]])
                    req(input[["jaar_kencijfer"]])
                    
                  
                   results$res <- reactive({
                     tabelKencijfers(data = kencijfersData(), 
                                     jaar = as.numeric(input[["jaar_kencijfer"]]),
                                     bron = ifelse(length(input[["dataSource_kencijfer"]]) == 2, "both", input[["dataSource_kencijfer"]]),
                                     thresholdWaarnemingen = input[["thresholdWaarnemingen"]],
                                     thresholdAfschot = input[["thresholdAfschot"]]) 
                     
                   })
                   
                   
   

                   #in order to collapse by grouping
                   resTableReactive <- reactive({
                     resTable <- results$res()$table[,c("category", "count", "municipality")]
                     resTable[resTable$category == "Totaal aantal gemeentes","municipality"] <- NA
                     resTable[,1] <- paste(  resTable[,"category"], resTable[,"count"], sep = ": ")
                     rownames(resTable) <- NULL
                     resTable
                   })

                   cityList <- resTableReactive()[,"municipality"] |> na.omit()
                   
                   
                   colorList <- ifelse((cityList %in% results$res()$observed) & (! cityList %in% results$res()$shot),
                                       "#ef8a62",
                                       ifelse(
                                         (cityList %in% results$res()$observed) & ( cityList %in% results$res()$shot), 
                                         "transparent", "#67a9cf") )
                   
                    
                   names(colorList) <- cityList

                   tb <- DT::datatable(
                     resTableReactive()[,c(1,3), drop = FALSE],
                     colnames = c("", ""),
                     rownames = FALSE,
                     extensions = 'RowGroup',
                     options = list(
                       rowGroup = list(dataSrc = 0),
                       pageLength = nrow(resTableReactive()),
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

                   if(length(cityList) > 0 & length(input[["dataSource_kencijfer"]]) == 2 ) {
                     tb <- tb |>
                       formatStyle(
                         columns = 2,
                         valueColumns = 2,
                         backgroundColor = styleEqual(unique(cityList), colorList)
                       )
                   }

                   output[["kencijfer_table"]] <- DT::renderDataTable(
                     tb
                   )
                   

                   ## section explaination
                   output$description <- renderUI({
                     
                  
                       description <- uiText[uiText$plotFunction == "F18_8","dash"] 
                       
                       if (length(description) > 0) tags$p(HTML(decodeText(text = description))) else tags$p()
                      
                     
                   })
                   
                   
                   ## download button   
                   
                   output$dataDownload <- downloadHandler(
                     
                     filename = function() {
                       paste("kencijfer-", species(), ".csv", sep="")
                     },
                     content = function(file) {
                       write.csv(  results$res()$table , file)
                     }
                   )
                   
                 })
                 
  
                 return(reactive(results$res()))
               })
}
