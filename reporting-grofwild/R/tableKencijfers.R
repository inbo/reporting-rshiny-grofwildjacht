
# data <- everGeoAll[provincie == "Vlaams Brabant"]
#' kencijfers summary table. The summary table of number of municipalities for two consective years. Filter is applied to the obsevation
#' data. Municipality is retained in the summary if there is any animal shot.
#' @param data The geo data contains aschot and waarnemingendata
#' @param bron data source, it should be one of the \code{"waarnemingen.be", "afschot", "both"}
#' @param jaar the year for which summary of municipalities' observed and shot animal stats
#' @param treshold threshold for number of animals observed
#' @return A list contains the summary table and the observed mulnicipalities 
#' @import data.table
#' @author Yingjie Zhang
#' @export
#' 

tabelKencijfers <- function(data, 
                            jaar = 2023, 
                            bron = "both",
                            threshold = c(1,10)){
  
  releventColumns <- c("afschotjaar", "provincie", "gemeente_afschot_locatie", "dataSource", "aantal")
  stopifnot(releventColumns %in% colnames(data))
  
  
  data <- data[!is.na(gemeente_afschot_locatie)]
  
  if(bron == "afschot")   threshold <- 0
  #bron <- if(bron == "both") c( "waarnemingen.be", "afschot" ) else bron
  
  yearRange <- range(data$afschotjaar)
  
  if(jaar == yearRange[1]) dataSubset <- data[afschotjaar == jaar]  else dataSubset <- data[afschotjaar %in% c(jaar, jaar-1)]
  
  dataSubset <- unique(dataSubset[, releventColumns, with = FALSE])
  
  
  if(bron == "both"){
    dataSubset <- dataSubset[(dataSource == "afschot" & aantal > 0) | (dataSource == "waarnemingen.be" & (aantal >= threshold[1] & aantal <= threshold[2]))]
  } else if(bron == "afschot"){
    
    
    dataSubset <- dataSubset[(dataSource == "afschot" & aantal > 0)]
    
  }else{
    dataSubset <- dataSubset[dataSource == "waarnemingen.be" & (aantal >= threshold[1] & aantal <= threshold[2])]
    
    
  }
  
  dataCurrentYear <-   dataSubset[afschotjaar == jaar]
  
  observedCities <- dataCurrentYear[dataSource != "afschot"][["gemeente_afschot_locatie"]]
  resultTable <- cbind("Totaal aantal gemeenten", length( unique( dataCurrentYear[["gemeente_afschot_locatie"]])),length( unique( dataCurrentYear[["gemeente_afschot_locatie"]])) )
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
  
  
  colnames(resultTable) <- NULL
  
  return( list("table" = resultTable, "observed" = observedCities ))
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
                inputId = ns("threshold"),
                label = "Waarnemingen drempel",
                value = c(1, 10),
                min = 1,
                max = 10,
                step = 1,
                sep = ""
              )
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
                                       value = max(kencijfersData()$afschotjaar),
                                       min = min(kencijfersData()$afschotjaar),
                                       max = max(kencijfersData()$afschotjaar),
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
                 })
                 
                 observe({
                  
                   req(input[["jaar_kencijfer"]])
                   req(input[["dataSource_kencijfer"]])
             
                   results$res <- reactive({
                     tabelKencijfers(data = kencijfersData(), 
                                     jaar = as.numeric(input[["jaar_kencijfer"]]),
                                     bron = ifelse(length(input[["dataSource_kencijfer"]]) == 2, "both", input[["dataSource_kencijfer"]]),
                                     threshold = input[["threshold"]])
                   })
                   
                   # in order to collapse by grouping
                   resTableReactive <- reactive({
                     resTable <- results$res()$table
                     resTable[,1] <- paste(  resTable[,1], resTable[,2], sep = ": ")
                     resTable[1,3] <- NA
                     resTable
                   })
                   
                   cityList <- resTableReactive()[,3] |> na.omit()
                   colorList <- ifelse(cityList %in% results$res()$observed, "grey", "transparent")
                   names(colorList) <- cityList
                   
                   tb <- DT::datatable(
                     resTableReactive()[,c(1,3), drop = FALSE],
                     colnames = c("", ""),
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
                   
                   if(length(cityList) > 0) {
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
                 
                   
                   ##
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
                       write.csv( resTableReactive() , file)
                     }
                   )
                   
                 })
                 
                 
                 
                 
                 return(reactive(results$res()))
               })
}