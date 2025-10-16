#' Summarize data for kencijfers
#' 
#' Aggregating over gemeente/provincie/dataSource/afschotjaar
#' Calculating total for selected unit
#' @param geoData data.table, geo data contains afschot and waarnemingendata
#' @inheritParams createTrendData
#' @return data.table
#' 
#' @author mvarewyck
#' @import data.table
#' @export
summarizeKencijferData <- function(geoData, biotoopData, 
  unit = c("absolute", "relative", "relativeDekking")) {
  
  # For R CMD check
  aantal <- gemeente_afschot_locatie <- provincie <- dataSource <- afschotjaar <- . <- NULL
  
  unit <- match.arg(unit)
  
  # Calculate sum per gemeente/provincie/dataSource/afschotjaar
  geoData <- geoData[ ,.(aantal= sum(aantal)), 
    by = .(gemeente_afschot_locatie, provincie, dataSource, afschotjaar)]
  
  
  if (grepl("relative", unit)) {
    
    areaVariable <- if (unit == "relative")
        "Area_km2" else
        "Area_hab_km2_bos"
    if ("year" %in% colnames(biotoopData))
      # add dekkingsgraad 100ha bos&natuur      
      geoData <- merge(geoData, biotoopData[, c("regio", areaVariable, "year")],
        by.x = c("gemeente_afschot_locatie", "afschotjaar"), 
        by.y = c("regio", "year")) else
      geoData <- merge(geoData, biotoopData[, c("regio", areaVariable)],
        by.x = "gemeente_afschot_locatie", by.y = "regio")
    
    geoData$aantal <- round(geoData$aantal/geoData[[areaVariable]], 2)
    geoData[[areaVariable]] <- NULL
    
  }
  
  geoData
  
}



#' Create summary table for kencijfers
#' 
#' The summary table of number of municipalities for selected year compared to reference period.
#' Benchmarking is applied to the observation and shot
#' data. Municipality is retained in the summary if there is any animal shot/observed.
#' @param data data.table, as returned by \code{\link{summarizeKencijferData}}
#' @param jaar the year for which summary of municipalities' observed and shot animal stats
#' @param period numeric vector of length 2. years for the reference period
#' @param bron character, data source. It should be one of \code{c("waarnemingen.be", "afschot", "both")}
#' @param ns function, for unique identifiers in the shiny module
#' @param thresholdWaarnemingen numeric. threshold for number of animals observed.
#' needs to be specified if \code{"bron"} is \code{"waarnemingen.be"} or \code{"both"}
#' @param thresholdAfschot numeric. threshold for number of animals shot.
#' needs to be specified if \code{"bron"} is \code{"afschot"} or \code{"both"}
#' @return A list containing the formatted table (html or pdf) and raw summary data (for download) 
#' @import data.table
#' @author yzhang
#' @export
#' @importFrom stats na.exclude

tableKencijfers <- function(data, jaar = 2023, period = c(jaar-1, jaar-5),
  bron = c("both", "waarnemingen.be", "afschot"),
  ns = function(x) x,
  thresholdWaarnemingen = 0,
  thresholdAfschot = 0){
  
  
  bron <- match.arg(bron)
  
  # For R CMD check
  gemeente_afschot_locatie <- afschotjaar <- dataSource <- current <- previous <- NULL
  
  relevantColumns <- c("afschotjaar", "provincie", "gemeente_afschot_locatie", "dataSource", "aantal")
  stopifnot(relevantColumns %in% colnames(data))
  
  dataSubset <- unique(data[!is.na(gemeente_afschot_locatie) &
        afschotjaar %in% c(jaar, period[1]:period[2]), 
      relevantColumns, with = FALSE])
  
  # Select current & reference period - average over reference period
  dataSubset <- dataSubset[, period := ifelse(afschotjaar == jaar, "current", "previous")]
  dataSubset <- data.table::dcast(dataSubset, gemeente_afschot_locatie + dataSource ~ period, value.var = "aantal",
    fill = NA, fun.agg = function(x) mean(x, na.rm = TRUE))
  
  # Filter bron
  currentGemeentes <- dataSubset[if (bron == "both")
      (dataSource == "afschot" & current >= thresholdAfschot) | 
          (dataSource == "waarnemingen.be" & current >= thresholdWaarnemingen) else if (bron == "afschot")
      dataSource == "afschot" & current >= thresholdAfschot else
      dataSource == "waarnemingen.be" & (current >= thresholdWaarnemingen), gemeente_afschot_locatie]
  previousGemeentes <- dataSubset[if (bron == "both")
        (dataSource == "afschot" & previous >= thresholdAfschot) | 
          (dataSource == "waarnemingen.be" & previous >= thresholdWaarnemingen) else if (bron == "afschot")
        dataSource == "afschot" & previous >= thresholdAfschot else
        dataSource == "waarnemingen.be" & (previous >= thresholdWaarnemingen), gemeente_afschot_locatie]
  
  resultTable <- data.frame(
    categorie = "Totaal aantal gemeentes", 
    aantal_categorie = length(unique(currentGemeentes)),
    gemeente = NA
  )
  
  # create current year summary table
  
  if (length(previousGemeentes) > 0) {
    
    common <- unique(intersect(currentGemeentes, previousGemeentes))
    new <- unique(na.exclude(setdiff(currentGemeentes, previousGemeentes)))
    old <- unique(na.exclude(setdiff(previousGemeentes, currentGemeentes)))
    
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
    
    if (length(currentGemeentes) != 0){
      
      resultTable <- rbind(resultTable, data.frame(
          categorie = "Dezelfde gemeentes",
          aantal_categorie = length(unique(currentGemeentes)), 
          gemeente = sort(unique(currentGemeentes)))
      )
    }
    
  }
  
  resultTable <- as.data.frame(resultTable)
  resultTable$categorie <- factor(resultTable$categorie, 
    levels = unique(resultTable$categorie))
  
  ## current year provincie table 
  if (!all(is.na(dataSubset$current))){
    
    tableCount <- data.table::dcast(dataSubset, gemeente_afschot_locatie ~ dataSource, 
      value.var = "current", fill = NA, drop = FALSE)
    
    finalTable <- merge(resultTable, tableCount, 
      by.x = "gemeente", by.y = "gemeente_afschot_locatie", all.x = TRUE, sort = FALSE)
    
    # sort back to original order
    finalTable <- finalTable[order(finalTable$categorie, finalTable$gemeente),]
    
    if ("waarnemingen.be" %in% colnames(finalTable))
      colnames(finalTable)[colnames(finalTable) == "waarnemingen.be"] <- "waarnemingen" else
      finalTable$waarnemingen <- NA      
    
    finalTable <- finalTable[, c("categorie", "aantal_categorie", "gemeente", "afschot", "waarnemingen")]
    
  } else {
    
    finalTable <- resultTable
    
  }
  
  rownames(finalTable) <- NULL

  # Format HTML table
  resTable <- finalTable[, c("categorie", "aantal_categorie", "gemeente")]
  resTable[,1] <- paste(resTable[, "categorie"], resTable[, "aantal_categorie"], sep = ": ")
  
  cityList <-  na.omit(resTable[,"gemeente"])
  
  observedCities <- dataSubset[dataSource == "waarnemingen.be" & current >= thresholdWaarnemingen, gemeente_afschot_locatie]
  afschotCities <- dataSubset[dataSource == "afschot" & current >= thresholdAfschot, gemeente_afschot_locatie]
  
  colorList <- ifelse((cityList %in% observedCities) & (!cityList %in% afschotCities),
    "#ef8a62",
    ifelse(
      (cityList %in% observedCities) & (cityList %in% afschotCities), 
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
  
  
  # Format PDF table
  simpleTable <- finalTable
  simpleTable$categorie <- as.character(simpleTable$categorie)
  simpleTable$aantal_categorie[duplicated(simpleTable$categorie)] <- ""
  simpleTable$categorie[duplicated(simpleTable$categorie)] <- ""
  simpleTable[simpleTable$categorie == "Totaal aantal gemeentes", c("gemeente", "afschot", "waarnemingen")] <- ""

  
  return(
    list(
      htmlTable = formattedTable,
      pdfTable = simpleTable,
      colorList = colorList,
      data = finalTable
    ))
  
  
}


#' Kencijfer table module UI
#' @inherit welcomeSectionUI
#' @author yzhang
#' @export
kencijferModuleUI <- function(id, uiText) {
  
  ns <- NS(id)
  
  title <- getOutputTitle(output = "kencijferUI", uiText = uiText)
  description <- getOutputDescription(output = "kencijferUI", uiText = uiText, 
    context = "description")
  
  tagList(
    
    actionLink(inputId = ns("linkKencijferTabel"), 
      label = paste("TABEL:", title), class = "action-h3"),
    
    conditionalPanel(
      condition = "input.linkKencijferTabel % 2 == 0", ns = ns,
      tagList(
        tags$p(HTML(description)),
        fixedRow(
          column(8,
            tags$div(
              style = "margin-bottom: 10px",
              withSpinner(DT::dataTableOutput(ns("kencijfer_table")))
            ),
            downloadButton(ns("dataDownload"), "Download data", class = "downloadButton")
          ),
          column(4,
            wellPanel(
              selectInput(inputId = ns("regionLevel"), label = "Regio-schaal",
                  choices = c(
                    "Vlaanderen" = "flanders",
                    "Provincie" = "provinces", 
                    "Gemeente" = "communes"
                  ),
                  selected = "provinces"),
              uiOutput(ns("region")),
              uiOutput(ns("filterYear")),
              uiOutput(ns("filterPeriod")),
              selectInput(inputId = ns("unit"), label = "Eenheid",
                choices = c(
                  "Aantal" = "absolute", 
                  "Aantal/100ha" = "relative", 
                  "Aantal/100ha bos & natuur" = "relativeDekking")),
              uiOutput(ns("filterSource")),
              conditionalPanel(
                condition = "input.bron.indexOf('waarnemingen.be') > -1", ns = ns,
                uiOutput(ns("sliderObserve"))
              ),
              conditionalPanel(
                condition = "input.bron.indexOf('afschot') > -1", ns = ns,
                uiOutput(ns("sliderAfschot"))
              )
            )
          )
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
#' @inheritParams summarizeKencijferData
#' @inheritParams kencijferModuleUI
#' @inheritParams reportingGrofwild-common-args
#' @import shiny
#' @importFrom DT JS formatStyle styleEqual
#' @author yzhang
#' @export

kencijferModuleServer <- function(id, input, output, session, kencijfersData, 
  biotoopData, spatialData, species){
  
  # For R CMD check
  afschotjaar <- aantal <- gemeente_afschot_locatie <- provincie <- . <- NULL
  
  results <- reactiveValues(
    observeThreshold = 1, 
    shotThreshold = 1
  )
  
  moduleServer(id,
    
    function(input, output, session) {
      
      ns <- session$ns
      
      
      output$region <- renderUI({
          
          req(input$regionLevel)
          
          if (input$regionLevel == "flanders")
            return(NULL)
          
          currentSpatial <- filterSpatial(
            allSpatialData = spatialData, 
            species = species(), 
            regionLevel = input$regionLevel, 
            year = NULL
          )
          
          regionChoices <- sort(unique(currentSpatial$NAAM))
          
          selectInput(inputId = ns("locaties"), label = "Regio('s)",
            choices = regionChoices,
            multiple = TRUE)
          
        })
      
      filterKencijfersData <- reactive({
          
          req(input$regionLevel)
          
          dataSingleEntry <- if (input$regionLevel != "flanders") {
              
              validate(need(input$locaties, "Gelieve regio('s) te selecteren"))
              filterGeo(data = kencijfersData(), regionLevel = input$regionLevel, 
                locaties = input$locaties, choseByID = FALSE)
              
            } else {     
              
              kencijfersData()
              
            }
          
          dataSingleEntry[ ,.(aantal= sum(aantal)), 
            by = .(gemeente_afschot_locatie, provincie, dataSource, afschotjaar)]
          
        })
      
      timeRange <- reactive({
          
          req(kencijfersData())
          c(min(kencijfersData()$afschotjaar), as.numeric(format(Sys.time(), "%Y")))
          
        })
      
      
      output$filterYear <- renderUI({
          
          div(class = "sliderBlank",
            sliderInput(inputId = ns("year"), label = "Geselecteerd jaar",
              value = config::get("defaultYear", file = system.file("config.yml", package = "reportingGrofwild")),
              min = min(timeRange()),
              max = max(timeRange()),
              step = 1,
              sep = ""))
          
        })
      
      output$filterPeriod <- renderUI({
          
          req(input$year)
          
          suppressWarnings(sliderInput(inputId = ns("period"), 
            label = "Referentieperiode", 
            value = c(input$year-5, input$year-1),
            min = min(timeRange()),
            max = max(timeRange()),
            step = 1,
            sep = ""))
          
        })
      
      
      output$filterSource <- renderUI({
          
          req(filterKencijfersData())
          
          dataSource <- unique(filterKencijfersData()$dataSource)
          req(length(dataSource) > 0)
          names(dataSource) <- gsub("\\..+", "", dataSource)
          
          selectInput(inputId = ns("bron"),
            label = "Databron(nen)",
            selected = dataSource,
            choices =  dataSource,
            multiple = TRUE)
          
        })
      
      kencijferSummarized <- reactive({
          
          req(filterKencijfersData())
          
          summarizeKencijferData(geoData = filterKencijfersData(),
            biotoopData = biotoopData(),
            unit = req(input$unit)
          )
          
        })
      
      output$sliderObserve <- renderUI({
          
          req(kencijferSummarized())
          req(input$year)
          valueChoices <- kencijferSummarized()[(dataSource == "waarnemingen.be") & (afschotjaar == input$year), aantal]
          if (Inf %in% valueChoices)
            valueChoices <- valueChoices[valueChoices != Inf]
          maxWaarnemingen <- max(if (input$unit == "absolute") 10 else 5, 
            suppressWarnings(max(valueChoices, na.rm = TRUE)))
          
          sliderInput(
            inputId = ns("thresholdWaarnemingen"),
            label = "Waarnemingen drempel",
            value = min(results$observeThreshold, maxWaarnemingen),
            min = if (input$unit == "absolute") 1 else 0,
            max = maxWaarnemingen,
            step = if (input$unit == "absolute") 1 else 0.1,
            sep = ""
          )
        })
      
      
      output$sliderAfschot <- renderUI({
          
          req(kencijferSummarized())
          req(input$year)
          valueChoices <- kencijferSummarized()[(dataSource == "afschot") & (afschotjaar == input$year), aantal]
          if (Inf %in% valueChoices)
            valueChoices <- valueChoices[valueChoices != Inf]
          maxSchot <- max(if (input$unit == "absolute") 10 else 5, 
            suppressWarnings(max(valueChoices, na.rm = TRUE)))
          
          sliderInput(
            inputId = ns("thresholdAfschot"),
            label = "Afschot drempel",
            value = min(results$shotThreshold, maxSchot),
            min = if (input$unit == "absolute") 1 else 0,
            max =  maxSchot,
            step = if (input$unit == "absolute") 1 else 0.1,
            sep = ""
          )
        })
      
      
      observeEvent(input$year, {
          
          req(input$thresholdAfschot)
          
          if (results$shotThreshold != input$thresholdAfschot)
            results$shotThreshold <- input$thresholdAfschot
          if (results$observeThreshold != input$thresholdWaarnemingen) 
            results$observeThreshold <- input$thresholdWaarnemingen
          
        })
      
      results$res <- reactive({
          
          req(input$year)
          req(input$period)
          
          tableKencijfers(
            data = req(kencijferSummarized()), 
            jaar = as.numeric(input$year),
            period = input$period,
            bron = if (is.null(input$bron)) 
                "both" else if (length(input$bron) == 2) 
                "both" else 
                input$bron,
            thresholdWaarnemingen = input$thresholdWaarnemingen,
            thresholdAfschot = input$thresholdAfschot,
            ns = ns
          ) 
          
        })
      
      output$kencijfer_table <- DT::renderDataTable(
        results$res()$htmlTable
      )
      
      
      ## download button 
      output$dataDownload <- downloadHandler(
        
        filename = function()
          nameFile(content = "kencijfer", species = species(), 
            year = input$year, fileExt = "csv"),
        content = function(file)
          write.csv(results$res()$data, file, row.names = FALSE)
      )
      
      
      return(reactive(results$res()))
    })
  
}
