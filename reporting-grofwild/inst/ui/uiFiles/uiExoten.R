# UI file for exoten indicators / checklist
# 
# Author: Eva Adriaensen
###############################################################################


tagList(
        
    
    tags$div(class = "container",
        
        
        
        tags$div(align = "center",
            h1("Welkom op de informatiepagina rond exoten"),
            h1("van het Instituut voor Natuur- en Bosonderzoek (INBO)")
        ),
        
        tags$p(class = "lead", "Op deze pagina kan je ..."),
        
        tags$p(tags$b(tags$u("Gebruiksinfo:")), tags$br(),
            tags$ul(
                tags$li("..."), 
                tags$li("...") 
            )
        ),        
        tags$h2("Keuze menu"),
        wellPanel(
            fixedRow(
                # Select time range
                column(6, uiOutput("exoten_timeOptions")),
                
                # Select regio
                column(6, uiOutput("exoten_regionOptions")),
                
            ),
            fixedRow(
                # show n
                column(4, textOutput("nrows"))

                )),
        wellPanel(
            fixedRow(
                # Select bron
                column(4, uiOutput("exoten_bronOptions")),
                
                # Select kingdom
                column(4, uiOutput("exoten_kingdomOptions")),
                
                column(4, uiOutput("exoten_pw1Options"))
                
#                # show taxa subchoices
#                column(4, tagList(uiOutput("exoten_phylumChoices"),
#                                  uiOutput("exoten_classChoices"),
#                                  uiOutput("exoten_orderChoices"),
#                                  uiOutput("exoten_familyChoices")))
  
                ),
                
            fixedRow(column(4, textOutput("nrowsFinal"))
                )
        ),
        
        
    
    ),
    
    tags$div(class = "container",
        
        uiOutput("exoten_titleSoortenPerJaar"),        
        plotModuleUI(id = "exoten_soortenPerJaar"),
        tags$br(),
        
        uiOutput("exoten_titleSoortenCumulatiefPlot"),        
        plotModuleUI(id = "exoten_soortenCumulatiefPlot"),
        tags$br()
        
        
    )
        
)
