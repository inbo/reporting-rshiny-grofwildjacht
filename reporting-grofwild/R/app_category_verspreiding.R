
#' Server function for an output (plot/table) of the 'verspreiding' Category page
#' @inheritParams reportingGrofwild-common-args
#' @return reactive value with name of selected specie
#' @import shiny
#' @author lcougnaud
#' @export
verspreidingOutputServer <- function(id, 
  specie = reactiveVal(), subcategory = reactiveVal(), plot = reactiveVal(),
  subcategories = character(),
  outputs = character(), defaultTabs = NULL,
  ecoData, geoData, spatialData, waarnemingenData, biotoopData,
  defaultYear,
  uiText){
  
  # For R CMD check
  wildsoort <- NULL
  
  moduleServer(id, function(input, output, session){  
      
      ns <- session$ns
      
      ## input
      results <- reactiveValues()
      
      # F17_1 plot
      results$geoData <- reactive({
          req(geoData)
          geoData[which(geoData$wildsoort == specie()), ]
        })
      
      # Restrict all to same date
      waarnemingenData <- waarnemingenData[
        waarnemingenData$afschotjaar <= 
          format(max(ecoData$afschot_datum, na.rm = TRUE), "%Y"), 
      ]
      
      # Combine waarnemingen.be & afschot
      results$geoDataAll <- reactive({
          rbind(
            # waarnemingen
            data.table::as.data.table(waarnemingenData),
            # afschot
            results$geoData(),
            fill = TRUE
          )
        })
      
      # Fetch all datasources of kencijfer data
      results$databronnen <- reactive({
          req(results$geoDataAll())
          
          dataSource <- unique(results$geoDataAll()$dataSource)
          req(length(dataSource) > 0)
          names(dataSource) <- gsub("\\..+", "", dataSource)
          
        })
      
      results$timeRange <- reactive({
          
          req(results$geoDataAll())
          
          kencijferData <- results$geoDataAll()[wildsoort == specie()]
          c(min(kencijferData$afschotjaar), as.numeric(format(Sys.time(), "%Y")))
          
        })
      
      # Max date highlight 
      output$maxDateHighlight <- renderUI({
          req(specie() != "Wolf")
          wellPanel(class = "well-white", 
            div(style = "text-align: center; font-size: 18px;",
              HTML(getOutputDescription(output = "maxDate_highlight", uiText = uiText, context = "description", maxDate = max(ecoData$afschot_datum, na.rm = TRUE)))
            )
          )
        })
      
      ## Sidebar panel
      
      specieSidebarServer(id = "sidebar", specie = specie)
      
      ## General selection - Verspreiding
      
      observe({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = ns("verspreiding_topbar")
              ),
              switch(as.character(subcategory()), 
                "verspreiding-huidig" = list(
                  showYear = TRUE,
                  showRegion = TRUE,
                  showType = TRUE,
                  showUnit = TRUE,
                  hideGeneralFilters = if (specie() == "Wolf") TRUE else FALSE
                ),
                "verspreiding-toekomstig" = list(
                  hideGeneralFilters = TRUE
                )
              )
            )
            
            # include plot/table in UI
            output[["topbar_filtering"]] <- renderUI(do.call(generalSelectionUI, args))
            
          }
          
        })
      
      verspreidingSelection <- reactive({
          
          req(subcategory())
          req(plot())
          
          if (subcategory() %in% subcategories) {   
            
            args <- c(
              list(
                id = "verspreiding_topbar"
              ),
              switch(as.character(subcategory()), 
                "verspreiding-huidig" = list(
                  regionLevels = c(1:4),
                  regionLevelSelected = "provinces",
                  allRegionsSelected = TRUE,
                  types = isolate(results$databronnen),
                  labelTypes = "Databron(nen)",
                  multipleTypes = TRUE,
                  units = c("Aantal" = "absolute", "Aantal/100ha" = "relative", 
                    "Aantal/100ha bos & natuur" = "relativeDekking"),
                  timeRange = isolate(results$timeRange),
                  data = reactive(isolate(results$geoDataAll()[wildsoort == specie()]))),
                "verspreiding-toekomstig" = list()
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
            
            # create the plot/table
            ui <- switch(as.character(subcategory()), 
              "verspreiding-huidig" = {
                tagList(
                  if ("F17_1" %in% outputs)
                    wellPanel(class = "well-white", mapFlandersUI(
                        id = ns("plot1"), 
                        showRegion = FALSE,
                        showCombine = FALSE, type = "dash",
                        mapScaleChoices = c("Gemeente" = "communes", "5x5 UTM" = "utm5"),
                        regionChoices = c(
                          "Vlaanderen" = "flanders",
                          "Provincie" = "provinces", 
                          "Faunabeheerzones" = "faunabeheerzones",
                          "Gemeente" = "communes"
                        ),
                        plotDetails = "", uiText = uiText,
                        doHide = !(plot() == defaultTabs$plot || "F17_1" %in% plot())
                      )),
                  if ("kencijferUI" %in% outputs)
                    wellPanel(class = "well-white", kencijferModuleUI(
                        id = ns("plot2"), 
                        uiText = uiText,
                        doHide = !(plot() == defaultTabs$plot || "kencijferUI" %in% plot())
                      )),
                  if ("mapLocationWolvesUI" %in% outputs)
                    wellPanel(class = "well-white", mapLocationWolvesUI(
                        id = ns("plot3"), showYear = TRUE,
                        uiText = uiText, context = "description",
                        doHide = !(plot() == defaultTabs$plot || "mapLocationWolvesUI" %in% plot())
                      )),
                  if ("mapUTMWolvesUI" %in% outputs)
                    wellPanel(class = "well-white", mapUTMWolvesUI(
                        id = ns("plot4"), showYear = TRUE,
                        uiText = uiText, context = "description",
                        doHide = !(plot() == defaultTabs$plot || "mapUTMWolvesUI" %in% plot())
                      )),
                  if ("mapDispersersWolvesUI" %in% outputs)
                    wellPanel(class = "well-white", mapDispersersWolvesUI(
                        id = ns("plot5"), 
                        uiText = uiText, context = "description",
                        doHide = !(plot() == defaultTabs$plot || "mapDispersersWolvesUI" %in% plot())
                      ))
                )
              },
              "verspreiding-toekomstig" = {
                tagList(
                  if ("mapSpreadUI" %in% outputs)
                    wellPanel(class = "well-white", mapSpreadUI(
                        id = ns("plot6"), 
                        uiText = uiText, context = "description",
                        specie = specie(),
                        doHide = !(plot() == defaultTabs$plot || "mapSpreadUI" %in% plot())
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
      
      # Create plot - server side
      observeEvent(outputServer(), ignoreNULL = TRUE, {
          
          req(verspreidingSelection())
          
          switch(as.character(outputServer()), 
            "verspreiding-huidig" = {
              c(
                if ("F17_1" %in% outputs)
                  mapFlandersServer(
                    id = "plot1",
                    defaultYear = defaultYear,
                    species = specie,
                    type = "dash",
                    geoData = results$geoDataAll,
                    allSpatialData = spatialData,
                    biotoopData = biotoopData,
                    hideGlobeDefault = FALSE,
                    countVariable = "aantal",
                    sourceChoices = c("waarnemingen.be", "afschot"),
                    uiText = uiText,
                    preSelected = verspreidingSelection
                  ),
                if ("kencijferUI" %in% outputs)
                  kencijferModuleServer(
                    id = "plot2",
                    kencijfersData = reactive(results$geoDataAll()[wildsoort == specie()]),
                    biotoopData = reactive(biotoopData$communes),
                    spatialData = spatialData,
                    species = specie,
                    preSelected = verspreidingSelection
                  ),
                if ("mapLocationWolvesUI" %in% outputs)
                  mapLocationWolvesServer(
                    id = "plot3",  
                    data = reactive(wolfPuntenData),
                    variable = "Status",
                    preSelected = verspreidingSelection
                  ),
                if ("mapUTMWolvesUI" %in% outputs)
                  mapUTMWolvesServer(
                    id = "plot4",  
                    data = reactive(wolfHokkenData),
                    preSelected = verspreidingSelection
                  ),
                if ("mapDispersersWolvesUI" %in% outputs)
                  mapDispersersWolvesServer(
                    id = "plot5",  
                    data = reactive(wolfOverzichtData),
                    preSelected = verspreidingSelection
                  )
              )
            },
            "verspreiding-toekomstig" = {
              c(
                if ("mapSpreadUI" %in% outputs)
                  mapSpreadUI = mapSpreadServer(
                    id = "plot6",
                    allSpatialData = spatialData,
                    species = specie(),
                    type = "F17_4",
                    preSelected = verspreidingSelection
                  )
              )
            }
          )
          
          # re-set in case plot selected via tab after/before category card
          outputServer(NULL)
        })
      
      return(list(
          specie = specie
        ))
      
    })
  
}