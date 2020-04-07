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
                column(4, uiOutput("exoten_timeOptions")),
                
                # Select type schade
                column(4, uiOutput("exoten_regionList"))#,
#                
#                # Select gewas & voertuig
#                column(4, uiOutput("schade_subcode"))
            )
        ),
        
        textOutput("nrows")
    
    )
)
