
#' Server function for an output (plot/table) of the 'beheer' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @import data.table
#' @author lcougnaud
#' @export
beheerOutputServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(), plot = reactiveVal(),
  subcategories = character(),
  outputs = character(), defaultTabs = NULL,
  ecoData, geoData, openingstijdenData, spatialData, biotoopData,
  defaultYear,
  uiText){
  
  # For R CMD check
  afschotplan_nummer <- afschot_datum <- provincie <- FaunabeheerZone <- NULL
  wildsoort <- afschotjaar <- . <- NULL  
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## input
      results <- reactiveValues(
        serverOutput = NULL
      )
      
      results$ecoData <- reactive(
        ecoData[which(ecoData$wildsoort == specie()), ]
      )

      results$geoData <- reactive({
          req(geoData)
          geoData[which(geoData$wildsoort == specie()), ]
        })
      
      results$timeRange <- reactive(range(results$ecoData()$afschotjaar))
      results$openingstijdenData <- reactive(
        openingstijdenData[openingstijdenData$Soort == specie(), ]
      )
      results$openingstijd <- reactive({
          # for Ree: openingseason contains more year than in the data
          # for Wildboar: openingseason contains less year than in the data
          
          # so retains the years when data and opening season specified
          # and doesn't retain the last year (because not full)
          
          if (specie() %in% c("Ree", "Wild zwijn")) {
            openingstijd <- c(
              max(
                min(results$ecoData()$afschotjaar), 
                min(results$openingstijdenData()$Jaar)
              ),
              min(
                max(results$ecoData()$afschotjaar), 
                max(results$openingstijdenData()$Jaar)
              )
            )
          }
        })
      
      # Enrich data with FBZ
      results$combinedData <- reactive(
        merge(
          x = results$ecoData(), 
          y = results$geoData()[, c("ID", "FaunabeheerZone", "gemeente_afschot_locatie")], 
          by = "ID"
        )
      )
      
      # Plot: Percentage jaarlijkse afschot
      results$labeltypes <- reactive({
          req(results$openingstijdenData())
          types <- loadMetaEco(species = specie())$labeltype
          if (length(types) == 1 && specie() == types)
            return(c("alle" = "all")) else 
            return(types)
        })
      
      results$leeftijdtypes <- reactive(
        c(loadMetaEco(species = specie())$leeftijd_comp_inbo, "Onbekend")
      )
      
      results$jachttypes <- reactive({
          choices <- unique(results$combinedData()$jachtmethode_comp)
          if (any(is.na(choices)))
            choices[is.na(choices)] <- "onbekend"
          sort(choices)
        })
      
      results$drukjachtData <- reactive({
          colsGeo <- c("afschotplan_nummer", "postcode_afschot_locatie", 
            "FaunabeheerZone", "gemeente_afschot_locatie"
          )
          drukjachtData <- as.data.table(merge(
              x = results$ecoData()[
                results$ecoData()$jachtmethode_comp %in% "Drukjacht", 
                c("ID", "afschot_datum", "afschotjaar", "provincie", "wildsoort")
              ], 
              y = results$geoData()[, c("ID", colsGeo)], 
              by = "ID", all.x = TRUE
            ))
          # Keep most prevalent province/FBZ per afschotplan_nummer & date
          ## overwrite with most prevalent province/FBZ
          drukjachtData <- drukjachtData[,':='(
              provincie = names(which.max(table(provincie))), 
              FaunabeheerZone = names(which.max(table(FaunabeheerZone)))),
            by = c("afschotplan_nummer", "afschot_datum")]
          drukjachtData <- unique(drukjachtData, by = c("afschotplan_nummer", "afschot_datum"))
          drukjachtData[, .(afschotplan_nummer, afschot_datum, provincie, FaunabeheerZone, wildsoort, afschotjaar)]
          
          validate(need(nrow(drukjachtData) > 0, "Geen data beschikbaar"))
          return(drukjachtData)
        })
      
      # Max date highlight 
      output$maxDateHighlight <- renderUI({
          
          maxDate <- max(ecoData$afschot_datum, na.rm = TRUE)
          
          text <- getOutputDescription(
            output = paste0(gsub(" ", "-", tolower(specie())), "_", subcategory(), "_maxDateHighlight"), 
            uiText = uiText, context = "description", maxDate = maxDate)
          
          if (is.null(text)) {
            text <- getOutputDescription(
              output = paste0(gsub(" ", "-", tolower(specie())), "_", strsplit(subcategory(), "-")[[1]][[1]], "_maxDateHighlight"), 
              uiText = uiText, context = "description", maxDate = maxDate)
            
            if (is.null(text)) {
              text <- getOutputDescription(output = "maxDateHighlight", uiText = uiText, context = "description", maxDate = maxDate)
            }
          }
          
          req(nchar(text) > 0)
          
          wellPanel(class = "well-white", 
            div(style = "text-align: center; font-size: 18px;",
              HTML(text)
            )
          )
        })
      
      ## Sidebar panel
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## General selection - Beheer
      
      observe({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = ns("beheer_topbar")
              ),
              switch(as.character(subcategory()), 
                "beheer-vlaanderen" = list(
                  hideGeneralFilters = TRUE
                ),
                "beheer-regio" = list(
                  showTime = TRUE,
                  showRegion = TRUE
                ),
                "beheer-leeftijdcategorie" = list(
                  showType = TRUE,
                  showRegion = TRUE,
                  showDataSource = c("leeftijd")
                ),
                "beheer-jachtmethode" = list(
                  showTime = TRUE, 
                  showRegion = TRUE,
                  showInterval = TRUE
                ),
                "beheer-afschotplan" = list(
                  hideGeneralFilters = TRUE
                )
              )
            )
            
            # include plot/table in UI
            output[["topbar_filtering"]] <- renderUI(do.call(generalSelectionUI, args))
            
          }
          
        })
      
      beheerSelection <- reactive({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = "beheer_topbar",
                subcategory = subcategory
              ),
              switch(as.character(subcategory()), 
                "beheer-vlaanderen" = list(),
                "beheer-regio" = list(
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
                  timeRange = reactive(if (specie() == "Edelhert")
                      c(2008, max(results$ecoData()$afschotjaar)) else 
                      c(min(results$openingstijd()[1], results$timeRange()[1]), max(results$openingstijd()[2], results$timeRange()[2])))
                ),
                "beheer-leeftijdcategorie" = list(
                  regionLevels = c(1:4),
                  allRegionsSelected = TRUE,
                  types = results$leeftijdtypes,
                  labelTypes = "Leeftijdscategorie",
                  multipleTypes = TRUE,
                  data = results$combinedData
                 ),
                "beheer-jachtmethode" = list(
                  regionLevels = c(1:2, 4),
                  regionLevelSelected = "provinces",
                  allRegionsSelected = TRUE,
                  intervals = c("Per jaar", "Per maand", "Per kwartaal", "Per twee weken"),
                  timeRange = reactive(if (specie() == "Wild zwijn")
                        c(min(2014, min(results$drukjachtData()$afschotjaar)), max(results$timeRange()[2], max(results$drukjachtData()$afschotjaar))) else 
                        c(2014, results$timeRange()[2])
                  )
                ),
                "beheer-afschotplan" = list()
              )
            )
            
            do.call(generalSelectionServer, args)
            
          }
          
        })
      
      
      
      ## Main panel
      
      # Tab content with selected plot/table
      
      outputServer <- reactiveVal(NULL)
      
      # Create plot - UI side
      observe({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {
            
            ui <- switch(as.character(subcategory()), 
              "beheer-vlaanderen" = {
                tagList(
                  if ("trendYearFlandersUI" %in% outputs)
                    wellPanel(class = "well-white", trendYearFlandersUI(
                        id = ns("plot"),
                        uiText = uiText,
                        includeOptions = TRUE,
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "trendYearFlandersUI" %in% plot())
                      ))
                )
              },
              "beheer-regio" = {
                tagList(
                  if ("mapFlandersUI" %in% outputs)
                    wellPanel(class = "well-white", mapFlandersUI(
                        id = ns("plot"), showRegion = FALSE,
                        type = "beheer", plotDetails = "region", uiText = uiText, 
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "mapFlandersUI" %in% plot())
                      )),
                  if ("countYearProvinceUI-afschot" %in% outputs)
                    wellPanel(class = "well-white", countYearProvinceUI(
                        id = ns("plot"), 
                        uiText = uiText, 
                        plotFunction = "countYearProvinceUI-afschot",
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "countYearProvinceUI-afschot" %in% plot())
                      )),
                  if ("yearlyShotAnimalsUI" %in% outputs)
                    wellPanel(class = "well-white", yearlyShotAnimalsUI(
                        id = ns("plot"), showType = TRUE, showYear = TRUE,
                        uiText = uiText, context = "description",
                        doHide = !(plot() == defaultTabs$plot || "yearlyShotAnimalsUI" %in% plot())
                      ))
                )
              },
              "beheer-leeftijdcategorie" = {
                tagList(
                  if ("countYearShotUI-leeftijd_comp" %in% outputs)
                    wellPanel(class = "well-white", countYearShotUI(
                        id = ns("plot"), groupVariable = "leeftijd_comp", showInterval = TRUE, showType = TRUE,
                        uiText = uiText, context = "description", specie = specie(), showTime = TRUE,
                        doHide = !(plot() == defaultTabs$plot || "countYearShotUI-leeftijd_comp" %in% plot())
                      )),
                  if ("tableProvinceUI" %in% outputs)
                    wellPanel(class = "well-white", tableProvinceUI(
                        id = ns("plot"),
                        uiText = uiText, context = "description", specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "tableProvinceUI" %in% plot())
                      )) 
                )
              },
              "beheer-jachtmethode" = {
                tagList(
                  if ("countYearShotUI-jachtmethode_comp" %in% outputs)
                    wellPanel(class = "well-white", countYearShotUI(
                        id = ns("plot"), groupVariable = "jachtmethode_comp", showType = TRUE,
                        uiText = uiText, context = "description", specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "countYearShotUI-jachtmethode_comp" %in% plot())
                      )),
                  if ("countYearShotUI-wettelijk_kader" %in% outputs)
                    wellPanel(class = "well-white", countYearShotUI(
                        id = ns("plot2"), groupVariable = "wettelijk_kader", showType = TRUE,
                        uiText = uiText, context = "description", specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "countYearShotUI-wettelijk_kader" %in% plot())
                      )),
                  if ("countYearShotUI-periode" %in% outputs)
                    wellPanel(class = "well-white", countYearShotUI(
                        id = ns("plot3"), groupVariable = "periode", showType = TRUE,
                        uiText = uiText, context = "description", specie = specie(), 
                        showSchemeringType = TRUE,
                        doHide = !(plot() == defaultTabs$plot || "countYearShotUI-ymoment_dag" %in% plot())
                      )),
                  if ("F04_3" %in% outputs)
                    wellPanel(class = "well-white", countYearProvinceUI(
                        id = ns("plot"), 
                        uiText = uiText, specie = specie(),
                        plotFunction = "F04_3", showCombinatie = TRUE,
                        doHide = !(plot() == defaultTabs$plot || "F04_3" %in% plot())
                      ))
                )
              },
              "beheer-afschotplan" = {
                tagList(
                  if ("afschotAanvraagReewild" %in% outputs)
                    wellPanel(class = "well-white", 
                      requestAfschotReewildUI(id = ns("afschotAanvraagReewild"), uiText = uiText, context = "description",
                      doHide = !(plot() == defaultTabs$plot || "afschotAanvraagReewild" %in% plot())))
                )
              }
            
            )
            
            # include plot/table in UI
            output[["output"]] <- renderUI(ui)
            
            # activate server-side update
            outputServer(subcategory())
            
          }
          
        })
      
      # Create plot - server side
      observeEvent(outputServer(), ignoreNULL = TRUE, {
          
          req(beheerSelection())
          
          results$serverOutput <- switch(as.character(outputServer()), 
            "beheer-vlaanderen" = {
              list(
                plot1 = if ("trendYearFlandersUI" %in% outputs)
                  trendYearFlandersServer(
                    id = "plot", 
                    geoData = results$geoData,
                    allSpatialData = spatialData, 
                    biotoopData = biotoopData, 
                    species = specie,
                    uiText = uiText,
                    preSelected = beheerSelection
                  )
              )
            },
            "beheer-regio" = {
              list(
                plot1 = if ("mapFlandersUI" %in% outputs)
                  mapFlandersServer(
                    id = "plot",
                    uiText = uiText,
                    defaultYear = defaultYear,
                    species = specie,
                    type = "beheer",
                    geoData = results$geoData,
                    biotoopData = biotoopData,
                    allSpatialData = spatialData,
                    preSelected = beheerSelection
                  ),
                plot2 = if ("countYearProvinceUI-afschot" %in% outputs)
                  countYearProvinceServer(
                    id = "plot",
                    data = results$combinedData,
                    allRegionsSelected = TRUE,
                    preSelected = beheerSelection
                  ),
                plot3 = if ("yearlyShotAnimalsUI" %in% outputs)
                  yearlyShotAnimalsServer(
                    id = "plot", 
                    data = results$combinedData, 
                    timeRange = results$openingstijd, 
                    type = results$labeltypes, 
                    openingstijdenData = results$openingstijdenData,
                    preSelected = beheerSelection
                  )
              )
            },
            "beheer-leeftijdcategorie" = {
              list(
                plot1 = if ("countYearShotUI-leeftijd_comp" %in% outputs)
                  countYearShotServer(
                    id = "plot",
                    data = results$combinedData,
                    timeRange = results$timeRange,
                    groupVariable = "leeftijd_comp",
                    types = results$jachttypes,
                    preSelected = beheerSelection
                  ),
                plot2 = if ("tableProvinceUI" %in% outputs)
                  tableProvinceServer(
                    id = "plot",
                    data = results$combinedData,
                    categorie = "leeftijd",
                    timeRange = results$timeRange,
                    preSelected = beheerSelection
                  )
              )
            },
            "beheer-jachtmethode" = {
              list(
                plot1 = if ("countYearShotUI-jachtmethode_comp" %in% outputs)
                  countYearShotServer(
                    id = "plot",
                    data = results$combinedData,
                    groupVariable = "jachtmethode_comp",
                    types = results$jachttypes,
                    preSelected = beheerSelection
                  ),
                plot2 = if ("countYearShotUI-wettelijk_kader" %in% outputs)
                  countYearShotServer(
                    id = "plot2",
                    data = results$combinedData,
                    groupVariable = "wettelijk_kader",
                    types = results$jachttypes,
                    preSelected = beheerSelection
                  ),
                plot3 = if ("countYearShotUI-periode" %in% outputs)
                  countYearShotServer(
                    id = "plot3",
                    data = results$combinedData,
                    groupVariable = "periode",
                    types = results$jachttypes,
                    preSelected = beheerSelection
                  ),
                plot4 = if ("F04_3" %in% outputs)
                  countYearProvinceServer(
                    id = "plot", 
                    data = results$drukjachtData,
                    preSelected = beheerSelection
                  )
              )
            },
            "beheer-afschotplan" = {
              list(
                plot1 = if ("afschotAanvraagReewild" %in% outputs)
                  requestAfschotReewildServer(id = "afschotAanvraagReewild", 
                    data = results$combinedData)
              )
            }
          )
          
          # re-set in case plot selected via tab after/before category card
          outputServer(NULL)
        })
        
      observe({
          req(as.character(subcategory()) == "beheer-regio")
          req(results$serverOutput$plot1)
          p <- results$serverOutput$plot1()
          req(p)
          req(p$selectedRegions)
          req(p$selectedRegions())
          
          updateSelectInput(session,
            inputId = "beheer_topbar-region",
            selected = p$selectedRegions()
          )
        })

      
      return(list(
          specie = specie
        ))
      
    })
  
}