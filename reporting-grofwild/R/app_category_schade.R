
#' Server function for an output (plot/table) of the 'schade' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export               
schadeOutputServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(), plot = reactiveVal(),
  subcategories = character(),
  schade_code, schade_gewas, schade_voertuig,
  outputs = character(), defaultTabs = NULL,
  schadeData, spatialData, biotoopData, 
  defaultYear, 
  schadeTypes, schadeCodes,
  uiText){
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## input
      results <- reactiveValues(renderedTabs = "Schade",
        serverOutput = NULL)
      
      # Filter data upon user choices
      results$schade_data <- reactive({
          req(schadeSelection())
          
          # Select species & code & exclude data before 2014
          toRetain <- 
            schadeData$wildsoort %in% req(specie()) &
            schadeData$schadeBasisCode %in% req(schadeSelection()$schade_code()) &
            schadeData$afschotjaar >= 2014
          
          # Filter gewas
          if ("GEWAS" %in% schadeSelection()$schade_code()) {
            otherCodes <- schadeSelection()$schade_code()[schadeSelection()$schade_code() != "GEWAS"]
            toRetain <- toRetain &
              (schadeData$schadeBasisCode %in% otherCodes |
                schadeData$schadeCode %in% schadeSelection()$schade_gewas())
          }
          
          # Filter voertuig
          if ("VRTG" %in% schadeSelection()$schade_code()) {
            otherCodes <- schadeSelection()$schade_code()[schadeSelection()$schade_code() != "VRTG"]
            toRetain <- toRetain &
              (schadeData$schadeBasisCode %in% otherCodes |
                schadeData$schadeCode %in% schadeSelection()$schade_voertuig())
          }
          
          return(schadeData[toRetain, ])
        })
      
      results$schade_timeRange <- reactive(
        range(results$schade_data()$afschotjaar)
      ) 
      
      # Max date highlight 
      output$maxDateHighlight <- renderUI({
          wellPanel(class = "well-white", 
            div(style = "text-align: center; font-size: 18px;",
              HTML(getOutputDescription(output = "maxDate_highlight", uiText = uiText, context = "description", maxDate = max(schadeData$afschot_datum, na.rm = TRUE)))
            )
          )
        })
      
      ## Selection species
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## General selection - Beheer
      
      observe({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = ns("schade_topbar")
              ),
              switch(as.character(subcategory()), 
                "schade-vlaanderen" = list(
                  showTime = TRUE,
                  showDataSource = c("schade")
                ),
                "schade-regio" = list(
                  showTime = TRUE,
                  showRegion = TRUE,
                  showDataSource = c("schade")
                ),
                "schade-type-gewas" = list(
                  showTime = TRUE,
                  showRegion = TRUE,
                  summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
                  showDataSource = c("schade") # Include rapporteer
                ),
                "schade-type-schade" = list(
                  showTime = TRUE,
                  showRegion = TRUE,
                  summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
                  showDataSource = c("schade")
                ),
                "schade-seizoen" = list(
                  hideGeneralFilters = FALSE    # General schade filters should be shown
                ),
                "schade-kosten" = list(
                  hideGeneralFilters = FALSE    # General schade filters should be shown
                )
              )
            )
            
            # include plot/table in UI
            output[["topbar_filtering"]] <- renderUI(do.call(generalSelectionUI, args))
            
          }
          
        })
      
      schadeSelection <- reactive({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = "schade_topbar",
                subcategory = subcategory,
                includeSchadeFilters = TRUE,
                schade_code = schade_code, 
                schade_gewas = schade_gewas, 
                schade_voertuig = schade_voertuig, 
                schadeSources = loadMetaSchade()$sources
              ),
              switch(as.character(subcategory()), 
                "schade-vlaanderen" = list(
                  timeRange = results$schade_timeRange
                ),
                "schade-regio" = list(
                  regionLevels = c(
                    "Vlaanderen" = "flanders",
                    "Provincie" = "provinces", 
                    "Faunabeheerzones" = "faunabeheerzones",
                    "Gemeente" = "communes",
                    "Gemeente per Faunabeheerzone" = "fbz_gemeentes",
                    "5x5 UTM" = "utm5"
                  ), 
                  regionLevelSelected = "provinces",
                  allRegionsSelected = TRUE,
                  data = reactive(spatialData),
                  timeRange = results$schade_timeRange
                ),
                "schade-type-gewas" = list(
                  regionLevels = c(1:4),
                  regionLevelSelected = "provinces",
                  allRegionsSelected = TRUE,
                  data = results$schade_data,
                  summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
                  timeRange = results$schade_timeRange
                ),
                "schade-type-schade" = list(
                  regionLevels = c(1:4),
                  regionLevelSelected = "provinces",
                  allRegionsSelected = TRUE,
                  data = results$schade_data,
                  summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
                  timeRange = results$schade_timeRange
                ),
                "schade-seizoen" = list(),
                "schade-kosten" = list()
              )
            )
            
            do.call(generalSelectionServer, args)
            
          }
          
        })
        
      
      ## Main panel
      
      ## Tab content with selected plot/table
      
      outputServer <- reactiveVal(NULL)
      
      # Create plot - UI side
      observe({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {
            
            # create the plot/table
            ui <- switch(as.character(subcategory()), 
              "schade-vlaanderen" = {
                tagList(
                  if ("tableSchadeSummaryUI" %in% outputs)
                    wellPanel(class = "well-white", tableSchadeSummaryUI(
                        id = ns("plot1"), 
                        uiText = uiText, specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "tableSchadeSummaryUI" %in% plot())
                      )),
                  if ("trendYearFlandersUI-schade" %in% outputs)
                    wellPanel(class = "well-white", trendYearFlandersUI(
                        id = ns("plot2"),
                        type = "wildschade",
                        includeOptions = FALSE,
                        uiText = uiText,
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "trendYearFlandersUI-schade" %in% plot())
                      )),
                )
              }, 
              "schade-regio" = {
                tagList(
                  if ("countYearProvinceUI-schade" %in% outputs)
                    wellPanel(class = "well-white", countYearProvinceUI(
                        id = ns("plot3"), 
                        uiText = uiText, 
                        plotFunction = "countYearProvinceUI-schade",
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "countYearProvinceUI-schade" %in% plot())
                      )),
                  if ("mapFlandersUI-schade" %in% outputs)
                    wellPanel(class = "well-white", mapFlandersUI(
                        id = ns("plot4"), showRegion = FALSE,
                        type = "schade", plotDetails = "region",
                        uiText = uiText,
                        doHide = !(plot() == defaultTabs$plot || "mapFlandersUI-schade" %in% plot())
                      )),
                  if ("mapSchadeUI" %in% outputs)
                    wellPanel(class = "well-white", mapSchadeUI(
                        id = ns("plot5"), 
                        filterSource = FALSE, filterTime = FALSE,
                        filterVariable = TRUE,
                        variableChoices = 
                          c("Type schade" = "schadeCode",
                            "Seizoen" = "season",
                            "Jaar" = "afschotjaar"
                          ),
                        doHide = !(plot() == defaultTabs$plot || "mapSchadeUI" %in% plot())
                      ))
                )
              },
              "schade-type-gewas" = {
                tagList(
                  if ("countYearSchadeUI-gewas" %in% outputs)
                    wellPanel(class = "well-white", countYearSchadeUI(
                        id = ns("plot6"),
                        uiText = uiText, context = "description",
                        type = "gewas", specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "countYearSchadeUI-gewas" %in% plot())
                      )),
                  if ("tableGewasUI" %in% outputs)
                    wellPanel(class = "well-white", tableGewasUI(
                        id = ns("plot7"), 
                        uiText = uiText, context = "description",
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "tableGewasUI" %in% plot())
                      ))
                )
              },
              "schade-type-schade" = {
                tagList(
                  if ("countYearSchadeUI-wildschade" %in% outputs)
                    wellPanel(class = "well-white", countYearSchadeUI(
                        id = ns("plot8"), 
                        uiText = uiText, context = "description",
                        type = "schade", specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "countYearSchadeUI-wildschade" %in% plot())
                      )),
                  if ("tableSchadeUI" %in% outputs)
                    wellPanel(class = "well-white", tableSchadeUI(
                        id = ns("plot9"), 
                        uiText = uiText, context = "description",
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "tableSchadeUI" %in% plot())
                      ))
                )
              },
              "schade-seizoen" = {
                tagList(
                  if ("countYearSchadeUI-seizoen" %in% outputs)
                    wellPanel(class = "well-white", countYearSchadeUI(
                        id = ns("plot10"), 
                        uiText = uiText, context = "description",
                        type = "seizoen", specie = specie(),
                        summarizeBy = c("Aantal" = "count", "Percentage" = "percent"),
                        showTime = TRUE, showDataSource = "schade",
                        regionLevels = c(1:4),
                        doHide = !(plot() == defaultTabs$plot || "countYearSchadeUI-seizoen" %in% plot())
                      ))
                )
              },
              "schade-kosten" = {
                tagList(
                  if ("barCostUI" %in% outputs)
                    wellPanel(class = "well-white", barCostUI(
                        id = ns("plot11"), 
                        uiText = uiText, context = "description",
                        specie = specie(), showTime = TRUE,
                        typeMelding = c("Landbouw" = "landbouw"),
                        regionLevels = c(1:4),
                        doHide = !(plot() == defaultTabs$plot || "barCostUI" %in% plot())
                      ))
                )
              }
            )
            
            # include plot/table in UI
            output[["output"]] <- renderUI(ui)
            
            # activate server-side update
            outputServer(subcategory())
            
          }
          
        })
      
      # Server
      observeEvent(outputServer(), ignoreNULL = TRUE, {
          
          req(schadeSelection())
          
          results$serverOutput <- switch(as.character(outputServer()), 
            "schade-vlaanderen" = {
              list(
                plot1 = if ("tableSchadeSummaryUI" %in% outputs)
                  tableSchadeSummaryServer(
                    id = "plot1", 
                    data = results$schade_data, 
                    schadeTypes = schadeTypes, schadeCodes = schadeCodes,
                    preSelected = schadeSelection
                  ),
                plot2 = if ("trendYearFlandersUI-schade" %in% outputs)
                  trendYearFlandersServer(
                    id = "plot2", 
                    geoData = results$schade_data, 
                    allSpatialData = spatialData, 
                    biotoopData = biotoopData, 
                    species = specie,
                    type = "wildschade",
                    uiText = uiText,
                    preSelected = schadeSelection
                  )
              )
            },
            "schade-regio" = {
              list(
                plot1 = if ("mapFlandersUI-schade" %in% outputs)
                  mapFlandersServer(
                    id = "plot4", 
                    uiText = uiText,
                    defaultYear = defaultYear,
                    species = specie,
                    type = "schade",
                    geoData = results$schade_data,
                    biotoopData = biotoopData,
                    allSpatialData = spatialData,
                    sourceChoices = loadMetaSchade()$sources,
                    preSelected = schadeSelection
                  ),
                plot2 = if ("countYearProvinceUI-schade" %in% outputs)
                  countYearProvinceServer(
                    id = "plot3",
                    data = results$schade_data,
                    allRegionsSelected = TRUE,
                    timeRange = results$schade_timeRange,
                    preSelected = schadeSelection
                  ),
                plot3 = if ("mapSchadeUI" %in% outputs)
                  mapSchadeServer(
                    id = "plot5", 
                    schadeData = results$schade_data,
                    allSpatialData = reactive(spatialData),
                    timeRange = results$schade_timeRange,
                    defaultYear = defaultYear,
                    species = specie,
                    borderRegion = "provinces",
                    uiText = uiText,
                    type = "schade",
                    preSelected = schadeSelection
                  )
              )
            },
            "schade-type-gewas" = {
              list(
                plot1 = if ("countYearSchadeUI-gewas" %in% outputs)
                  countYearSchadeServer(
                    id = "plot6",
                    data = results$schade_data,
                    type = "SoortNaam", 
                    timeRange = results$schade_timeRange,
                    fullNames = schadeCodes,
                    preSelected = schadeSelection
                  ),
                plot2 = if ("tableGewasUI" %in% outputs)
                  tableGewasServer(
                    id = "plot7",
                    data = results$schade_data,
                    timeRange = results$schade_timeRange,
                    variable = "SoortNaam",
                    allRegionsSelected = TRUE,
                    preSelected = schadeSelection
                  ) 
              )
            },
            "schade-type-schade" = {
              list(
                plot1 = if ("countYearSchadeUI-wildschade" %in% outputs)
                  countYearSchadeServer(
                    id = "plot8",
                    data = results$schade_data,
                    type = "schadeCode", 
                    timeRange = results$schade_timeRange,
                    fullNames = schadeCodes,
                    preSelected = schadeSelection
                  ),
                plot2 = if ("tableSchadeUI" %in% outputs)
                  tableSchadeServer(
                    id = "plot9",  
                    data = results$schade_data,
                    timeRange = results$schade_timeRange,
                    schadeChoices = schadeSelection()$schade_code,
                    schadeChoicesVrtg = schadeSelection()$schade_voertuig,
                    schadeChoicesGewas = schadeSelection()$schade_gewas,
                    datatable = TRUE,
                    fullNames = c(schadeTypes, schadeCodes),
                    allRegionsSelected = TRUE,
                    preSelected = schadeSelection
                  )
              )
            },
            "schade-seizoen" = {
              list(
                plot1 = if ("countYearSchadeUI-seizoen" %in% outputs)
                  countYearSchadeServer(
                    id = "plot10",
                    data = results$schade_data,
                    type = "season", 
                    allRegionsSelected = TRUE,
                    timeRange = results$schade_timeRange,
                    preSelected = schadeSelection
                  )
              )
            },
            "schade-kosten" = {
              list(
                plot1 = if ("barCostUI" %in% outputs)
                  barCostServer(
                    id = "plot11",
                    data = results$schade_data,
                    allRegionsSelected = TRUE,
                    yVar = "schadeBedrag",
                    timeRange = reactive(c(min(results$schade_data()$afschotjaar, na.rm = TRUE), max(results$schade_data()$afschotjaar, na.rm = TRUE))),
                    preSelected = schadeSelection
                  )  
              )
            }
          )
          
          # re-set in case plot selected via tab after/before category card
          outputServer(NULL)
        })
      
        observe({
            req(as.character(subcategory()) == "schade-regio")
            req(results$serverOutput$plot1)
            p <- results$serverOutput$plot1()
            req(p)
            req(p$selectedRegions)
            req(p$selectedRegions())
            
            updateSelectInput(session,
              inputId = "schade_topbar-region",
              selected = p$selectedRegions()
            )
          })
      
      return(list(
          specie = reactive(specie()),
          schade_code = schadeSelection()$schade_code,
          schade_gewas = schadeSelection()$schade_gewas,
          schade_voertuig = schadeSelection()$schade_voertuig
        ))
      

  })
  
}
