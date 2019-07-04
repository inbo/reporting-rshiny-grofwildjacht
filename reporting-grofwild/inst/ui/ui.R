shinyUI(
		
		bootstrapPage(
				
				## For debugging
				uiOutput("debug"),
				
				
				## Header
				## ------
				
				tags$head(
						tags$meta(charset = "utf-8"),
						tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
						tags$title("Grofwildjacht"),
						tags$link(rel = "stylesheet",
								href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
								integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
								crossorigin="anonymous"),
						tags$link(rel = "stylesheet",
								href = "style.css")
				),
				
				
				
				## Body
				## ------
				
				# Shape data source + contact e-mail
				tags$body(
						
						tags$nav(class = "navbar navbar-default",
								tags$div(class="container",
										tags$ul(class="nav navbar-nav navbar-right",
												tags$li(tags$a(href="http://www.geopunt.be/download?container=referentiebestand-gemeenten&title=Voorlopig%20referentiebestand%20gemeentegrenzen#", target="_blank", "Geodata bron")),
												tags$li(tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie", target="_blank", "Contact"))
										)
								)
						),
						
						tags$div(class = "container",
								
								tags$div(align = "center",
										h1("Welkom op de informatiepagina rond grofwildjacht"),
										h1("van het Instituut voor Natuur- en Bosonderzoek (INBO)")
								),
								
								tags$p(class = "lead", "Op deze pagina kunt u de afschotgegevens voor elk van de vier bejaagde grofwildsoorten binnen het Vlaams gewest van de laatste jaren raadplegen."),
								
								tags$p("De bron voor de kaarten, figuren en tabellen wordt gevormd door het",
										tags$a(href = "https://www.natuurenbos.be/e-loket", "E-loket van het Agentschap voor Natuur en Bos", target = "_blank"),
										"gecombineerd met door het INBO uitgevoerde metingen op ingezamelde stalen (onderkaken en baarmoeders)."),
								
								tags$p("Bovenaan deze pagina krijgt u, in functie van de gekozen jachtwildsoort, de regio-schaal en de gewenste jaren en periodes,", tags$br(),"een kaart met de geografische spreiding van het afschot en een figuur met de evolutie van het afschot de laatste jaren.",
										"Daaronder kunnen verdere grafieken worden aangeklikt, die telkens kunnen worden aangepast naargelang de informatie die u zelf wenst te visualiseren."),
								
								tags$p(paste0("Onderstaande tabellen en figuren zijn gebaseerd op de beschikbare gegevens op ",format(max(as.Date(ecoData$afschot_datum), na.rm = T), "%d/%m/%Y") , 
												". Een deel van de data van het voorbije kwartaal kunnen dus mogelijk nog niet opgenomen zijn in de dataset.")
								),
								tags$p("Indien u fouten zou ontdekken of merkt dat data ontbreken gelieve dit dan te melden via een email naar",
										tags$a(href="mailto:faunabeheer@inbo.be?SUBJECT=Grofwildjacht web applicatie", target="_blank", "faunabeheer@inbo.be")
								)
						
						),
						
						# Select species
						tags$div(class = "container", align = "center",
								radioButtons(inputId = "showSpecies", label = "", inline = TRUE,
										choiceValues = list("Wild zwijn", "Ree", "Damhert", "Edelhert"),
										choiceNames = list(
												HTML("<p><b>Wild zwijn</b></p><img src='wildZwijn.jpeg' width = '400px' title = 'Foto: Vildaphoto'>"),
												HTML("<p><b>Ree</b></p><img src='ree.jpeg' width = '400px' title = 'Foto: Vildaphoto'>"),
												HTML("<p><b>Damhert</b></p><img src='damhert.jpeg' width = '400px' title = 'Foto: Vildaphoto'>"),
												HTML("<p><b>Edelhert</b></p><img src='edelhert.jpeg' width = '400px' title = 'Foto: Vildaphoto'>"))
								)
						),
						
						# TODO create nice buttons? https://stackoverflow.com/questions/44841346/adding-an-image-to-shiny-action-button
#						tags$button(
#								id = "web_button",
#								class = "btn action_button",
#								label = "Wild zwijn",
#								img(src = "wildZwijn.jpeg", width = "400px")
#						),
						
						
						# Map with according line plot
						
						tags$div(class = "container",
								
								h2("Landkaart"),
								
								## countMap: all species
								wellPanel(
										fixedRow(
												column(4, selectInput(inputId = "map_regionLevel", label = "Regio-schaal",
																choices = c("Vlaanderen" = "flanders",
																		"Provincie" = "provinces", "Gemeente" = "communes"),
																selected = "communes")),
												column(8, uiOutput("map_region"))
										),
										
										fixedRow(
												column(6,
														uiOutput("map_year"),
														selectInput(inputId = "map_legend", "Legende (kaart)",
																choices = c("Bovenaan rechts" = "topright",
																		"Onderaan rechts" = "bottomright",
																		"Bovenaan links" = "topleft",
																		"Onderaan links" = "bottomleft",
																		"<geen>" = "none"))
												),
												column(6, uiOutput("map_time"))
										),
										
										actionLink(inputId = "map_globe", label = "Voeg landkaart toe",
												icon = icon("globe"))
								
								),
								
								
								uiOutput("map_title"),
								withSpinner(leafletOutput("map_spacePlot")),
								tags$br(),
								downloadButton("map_download", "Download figuur"),
								downloadButton("map_downloadData", "Download data"),
								
								fixedRow(
										column(6,
												h3("Referentie (Vlaanderen)"),
												plotModuleUI(id = "map_timePlotFlanders", height = "400px"),
												optionsModuleUI(id = "map_timePlotFlanders", exportData = TRUE,
														doWellPanel = FALSE)
										),
										column(6,
												uiOutput("map_timeTitle"),
												plotModuleUI(id = "map_timePlot", height = "400px"),
												optionsModuleUI(id = "map_timePlot", exportData = TRUE,
														doWellPanel = FALSE)
										)
								)
						
						),
						
						tags$hr(),
						
						
						# Show user input module per plot
						
						tags$div(class = "container",
								
								h2("Extra Figuren en Tabellen"),
								
								## tableProvince for "leeftijd": wild zwijn and ree
								conditionalPanel("input.showSpecies == 'Wild zwijn' || input.showSpecies == 'Ree'", {
											
											tagList(
													actionLink(inputId = "linkTable1",
															label = h3("TABEL: Gerapporteerd afschot per regio en per leeftijdscategorie")),
													conditionalPanel("input.linkTable1 % 2 == 1",
															
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "table1", showYear = TRUE, exportData = TRUE),
																			tags$p("Het gerapporteerd aantal geschoten dieren per provincie en per leeftijdscategorie voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.",
																					"Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald.")
																	),
																	column(8, tableModuleUI(id = "table1"))
															
															),
															tags$hr()
													)
											)
											
										}),
								
#			# tableProvince for "type": ree
#			conditionalPanel("input.showSpecies == 'Ree'", {
#						
#						tagList(
#								
#								actionLink(inputId = "linkTable2",
#										label = h3("TABEL: Gerapporteerd afschot per regio en per type")),
#								conditionalPanel("input.linkTable2 % 2 == 1",
#										fixedRow(
#												
#												column(4,
#														optionsModuleUI(id = "table2", showYear = TRUE, exportData = TRUE),
#														tags$p("Het gerapporteerd aantal geschoten dieren per provincie en per labeltype voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.")
#												),
#												column(8, tableModuleUI(id = "table2"))
#										
#										),
#										tags$hr()
#								),
#								
#								actionLink(inputId = "linkTable3",
#										label = h3("TABEL: Percentage gerealiseerd afschot per regio en per type")),
#								conditionalPanel("input.linkTable3 % 2 == 1",
#										
#										fixedRow(
#												
#												column(4,
#														optionsModuleUI(id = "table3", showYear = TRUE, exportData = TRUE),
#														tags$p("Realisatiegraad per provincie en per labeltype voor het geselecteerde jaar in combinatie met de trend over de voorbije 1, 5 of 10 jaren.")
#												),
#												column(8, tableModuleUI(id = "table3"))
#										
#										),
#										tags$hr()
#								),
#								
#								
#								actionLink(inputId = "linkPlot4b",
#										label = h3("FIGUUR: Percentage gerealiseerd afschot per jaar en per type")),
#								conditionalPanel("input.linkPlot4b % 2 == 1",
#										fixedRow(
#												
#												column(4,
#														optionsModuleUI(id = "plot4b",
#																summarizeBy = c("Aantal" = "count",
#																		"Percentage" = "percent"),
#																showTime = TRUE,
#																showType = TRUE, regionLevels = 1:2,
#																exportData = TRUE),
#														tags$p("Evolutie van de realisatiegraad doorheen de jaren voor geselecteerde regio('s) en labeltype.")
#												),
#												column(8, plotModuleUI(id = "plot4b"))
#										
#										),
#										tags$hr()
#								)
#						)
#						
#					}),
								
								
								## countYearProvince: all species
								actionLink(inputId = "linkPlot1",
										label = h3("FIGUUR: Gerapporteerd aantal per jaar en per regio")),
								conditionalPanel("input.linkPlot1 % 2 == 1",
										fixedRow(
												
												column(4,
														optionsModuleUI(id = "plot1", showTime = TRUE, exportData = TRUE),
														tags$p("Het gerapporteerd aantal geschoten en dood gevonden dieren per jaar in de verschillende provincies voor de geselecteerde periode (cijfers geven het totaal aantal voor dat jaar weer).")
												),
												column(8, plotModuleUI(id = "plot1"))
										
										),
										tags$hr()
								),
								
								
								## countAgeCheek & countYearAge & percentageYearlyShotAnimals
								## countAgeGender & boxAgeWeight
								## - Wild zwijn and Ree
								conditionalPanel("input.showSpecies == 'Wild zwijn' || input.showSpecies == 'Ree'", {
											
											tagList(
													actionLink(inputId = "linkPlot2", label =
																	h3("FIGUUR: Leeftijdscategorie op basis van onderkaak & meldingsformulieren")),
													conditionalPanel("input.linkPlot2 % 2 == 1",
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot2", showTime = TRUE, exportData = TRUE),
																			tags$p("Vergelijking tussen de leeftijd zoals aangeduid op het meldingsformulier en de leeftijd bepaald op basis van een ingezamelde onderkaak")
																	),
																	column(8, plotModuleUI(id = "plot2"))
															
															),
															tags$hr()
													),
													
													
													actionLink(inputId = "linkPlot3",
															label = h3("FIGUUR: Afschot per jaar en per leeftijdscategorie (o.b.v. onderkaak)")),
													conditionalPanel("input.linkPlot3 % 2 == 1",
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot3",
																					summarizeBy = c("Aantal (alle data)" = "count",
																							"Percentage (enkel ingezamelde onderkaken)" = "percent"),
																					showTime = TRUE,
																					regionLevels = 1:2, exportData = TRUE),
																			tags$p("Evolutie van de verdeling van het afschot over de verschillende leeftijdscategorieën doorheen de jaren op basis van de ingezamelde onderkaak."),
																			conditionalPanel("input.showSpecies == 'Ree'",
																					"Voor mannelijke reeën zijn de leeftijdscategorieën 'jongvolwassen' en 'volwassen' uitgesloten.")
																	),
																	column(8, plotModuleUI(id = "plot3"))
															),
															tags$hr()
													),
													
													
													actionLink(inputId = "linkPlot4",
															label = h3("FIGUUR: Percentage jaarlijks afschot")),
													conditionalPanel("input.linkPlot4 % 2 == 1",
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot4",
																					showTime = TRUE, showYear = TRUE,
																					showType = TRUE, exportData = TRUE),
																			tags$p("Procentuele verdeling van het afschot (voor ree per labeltype) doorheen het openingsseizoen van het geselecteerde jaar in verhouding tot de verdeling in de geselecteerde referentieperiode.",
																					"Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald.")
																	),
																	column(8, plotModuleUI(id = "plot4"))
															
															),
															tags$hr()
													),
													
													
													actionLink(inputId = "linkPlot5",
															label = h3("FIGUUR: Geslachtsverdeling binnen het afschot per leeftijdscategorie")),
													conditionalPanel("input.linkPlot5 % 2 == 1",
															
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot5", showTime = TRUE, exportData = TRUE),
																			tags$p("Geslachtsverdeling per leeftijdscategorie voor de geselecteerde periode. Voor everzwijn werd de leeftijdscategorie bepaald op basis van de ingezamelde onderkaak. Voor ree is dit de leeftijdscategorie vermeld op het meldingsformulier.")
																	),
																	column(8, plotModuleUI(id = "plot5"))
															
															),
															tags$hr()
													),
													
													
													actionLink(inputId = "linkPlot6",
															label = h3("FIGUUR: Leeggewicht per leeftijdscategorie (INBO of Meldingsformulier) en geslacht")),
													conditionalPanel("input.linkPlot6 % 2 == 1",
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot6", showTime = TRUE, showType = TRUE,
																					regionLevels = 1:2, exportData = TRUE),
																			tags$p("Verdeling van de leeggewichten per leeftijdscategorie en per geslacht voor alle gegevens uit de geselecteerde periode en regio('s).",
																					"Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald."),
																			conditionalPanel("input.showSpecies == 'Ree'",
																					"Voor ree: geen data beschikbaar voor 2014.")
																	),
																	column(8, plotModuleUI(id = "plot6"))
															
															),
															tags$hr()
													)
											
											)
											
										}),
								
								
								## boxAgeGenderLowerJaw
								## - Ree
								conditionalPanel("input.showSpecies == 'Ree'", {
											
											tagList(
													
													actionLink(inputId = "linkPlot7",
															label = h3("FIGUUR: Onderkaaklengte per leeftijdscategorie (INBO of Meldingsformulier) en geslacht")),
													conditionalPanel("input.linkPlot7 % 2 == 1",
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot7", showTime = TRUE, showType = TRUE,
																					regionLevels = 1:2, exportData = TRUE),
																			tags$p("Verdeling van de onderkaaklengte per leeftijdscategorie en per geslacht voor alle gegevens uit de geselecteerde periode en regio('s).",
																					"Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald.")),
																	column(8, plotModuleUI(id = "plot7"))
															
															),
															tags$hr()
													),
													
													h3("Bio-indicatoren"),
													tags$p("Bio-indicatoren zijn ecologische parameters, die betrekking hebben op de relatie tussen een populatie en de draagkracht van het gebied en gevoelig zijn voor veranderingen in populatieaantallen en/of in de draagkracht van het gebied.",
															"In dit geval dus de relatie tussen het aantal reeën in een gebied en de draagkracht van dat gebied. Voor ree werd aangetoond dat van zodra de draagkracht van een gebied wordt benaderd dit zich vertaalt in kleinere reekitsen (lichtere gewichten en kortere onderkaken), een lager percentage drachtige geiten en smalreeën en in gemiddeld kleinere worpen."),
													actionLink(inputId = "linkPlot8",
															label = h3("FIGUUR: Onderkaaklengte per jaar")),
													conditionalPanel("input.linkPlot8 % 2 == 1",
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot8", showTime = TRUE, showType = TRUE,
																					regionLevels = 1:2, exportData = TRUE,
																					showDataSource = TRUE),
																			tags$p("Verdeling van de onderkaaklengte voor alle gegevens uit de geselecteerde periode, regio('s) en type(s).",
																					"Indien de leeftijdscategorie van INBO (o.b.v. onderkaak) gekend is, wordt deze gebruikt, anders wordt de leeftijdscategorie volgens het meldingsformulier bepaald.")
																	),
																	column(8, plotModuleUI(id = "plot8"))
															),
															tags$hr()
													),
													
													actionLink(inputId = "linkPlot9", label = h3("FIGUUR: Gewicht per jaar")),
													conditionalPanel("input.linkPlot9 % 2 == 1",
															fixedRow(
																	
																	column(4,
																			optionsModuleUI(id = "plot9",
																					showTime = TRUE, showType = TRUE,
																					regionLevels = 1:2, exportData = TRUE),
																			tags$p("Evolutie van de gemodelleerde leeggewichten (met 95% betrouwbaarheidsinterval) doorheen de geselecteerde jaren voor de gekozen regio en types."),
																			tags$p("Observaties met leeggewicht < 5kg of > 25kg zijn niet opgenomen in de figuur.")
																	),
																	column(8, plotModuleUI(id = "plot9"))
															
															),
															tags$hr()
													)
											
#                          actionLink(inputId = "linkPlot10",
#                              label = h3("FIGUUR: Gerapporteerd aantal embryo's voor vrouwelijke reeën per jaar")
#                          ),
#                          conditionalPanel("input.linkPlot10 % 2 == 1",
#                              fixedRow(
#
#                                  column(4,
#                                      optionsModuleUI(id = "plot10", showTime = TRUE, showType = TRUE,
#                                          regionLevels = 1:2, exportData = TRUE),
#                                      tags$p("Evolutie van het gerapporteerd aantal embryo's per geschoten dier doorheen de geselecteerde jaren voor de gekozen regio en types. Voor 2014 kon nul embryo's niet ingevuld worden, waardoor er geen onderscheid gemaakt kon worden tussen niet drachtig en niet ingevuld."),
#                                      tags$p("Observaties met meer dan 3 embryo's zijn niet opgenomen in de figuur.")),
#                                  column(8, plotModuleUI(id = "plot10"))
#
#                              )
#                          )
											
											)
											
										})
						
						
						),
						
						## Footer
						## ------
						
						
						tags$footer(tags$div(class="container", tags$img(src="logo.png")))
				
				)
		)
)
