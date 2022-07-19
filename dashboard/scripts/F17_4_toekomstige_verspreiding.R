##Packages
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(htmlTable)
library(shinythemes)
library(shinydashboard)
########################################################################################

##Shapefiles
Pixelsmap <- readOGR("./dashboard/input/spatial/Pixels_ModelOutput_toekomst_verspr_2022.shp")
Pixelsmap <- spTransform(Pixelsmap, 
                         CRS("+proj=longlat +datum=WGS84"))

Gemeentemap <- readOGR("./dashboard/input/spatial/Municipalities_ModelOutput_toekomst_verspr_2022.shp")
Gemeentemap <- spTransform(Gemeentemap, 
                           CRS("+proj=longlat +datum=WGS84"))
########################################################################################

##Lay out
ui <- navbarPage(
  theme = shinytheme("spacelab"),
  title= "Verspreidingsmodel everzwijn",
  header= shinydashboard::box(width = 12, 
        img(src='./Rshiny/Input/inbo.png', align= 'left', height = 50),
        img(src='./Rshiny/Input/sciensano.png', align= 'right', height = 50)),
  tabPanel(
    title = "Model output", 
    "Voor het verspreidingsmodel werden 2 verschillende benaderingen voor startpopulaties gebruikt. De startlocatie o.b.v. exacte pixels is gebasseerd op een dataset van alle waarnemingen en afschotdata van everzwijnen met exacte coördinaten. De dataset met exacte coördinaten vormen slechts een deel van de gehele dataset van everwijnwaarnemingen en afschotdata, de meeste gegevens zijn beschikbaar op niveau van een gemeente. Daarom is de startlocatie o.b.v. optimaal habitat is gebasseerd op alle habitatpixels met 'hoge geschiktheid' in gemeentes waar everzwijnen werden geschoten. Voor de modelresultaten kan 1 van beide types startpopulaties worden gekozen.",
    br(),
    radioButtons("MODstartpop", "Startpopulatie",
                 c("Exacte pixels" = "Exacte_pixels",
                   "Optimaal habitat" = "Optimaal_habitat")),
    fluidRow(column(width = 6,
                    leafletOutput("Pixel_mod", height = 600)),
             column(width = 6,
                    leafletOutput("Gemeente_mod", height = 600)))),
  tabPanel(
    title = "AVP risico", width = 12, 
    "Voor het verspreidingsmodel werden 2 verschillende benaderingen voor startpopulaties gebruikt. De startlocatie o.b.v. exacte pixels is gebasseerd op een dataset van alle waarnemingen en afschotdata van everzwijnen met exacte coördinaten. De dataset met exacte coördinaten vormen slechts een deel van de gehele dataset van everwijnwaarnemingen en afschotdata, de meeste gegevens zijn beschikbaar op niveau van een gemeente. Daarom is de startlocatie o.b.v. optimaal habitat is gebasseerd op alle habitatpixels met 'hoge geschiktheid' in gemeentes waar everzwijnen werden geschoten.Voor de modelresultaten kan 1 van beide types startpopulaties worden gekozen.",
    radioButtons("AVPstartpop", "Startpopulatie",
                       c("Exacte pixels" = "Exacte_pixels",
                         "Optimaal habitat" = "Optimaal_habitat")),
    fluidRow(column(width = 6,
                    leafletOutput("Pixel_avp", height = 600)),
             column(width = 6,
                    leafletOutput("Gemeente_avp", height = 600))))
)
########################################################################################

##Server
server <- function(input, output) {
  output$Pixel_mod <- renderLeaflet({
    
    ###1. Pixel map model output 
    ####1.1 Background map habitat classes
    Pixelsmap$Hbtt_ct <- factor(Pixelsmap$Hbtt_ct, 
                                          levels = c("Hoge Geschiktheid","Geschikt", 
                                                     "Lage geschiktheid","Niet geschikt"))
    pal_hab <- colorFactor(
      palette = c("darkgreen", "green", "yellow", "grey"), 
      domain = Pixelsmap$Hbtt_ct)
    
    Output_finalyear <-leaflet(Pixelsmap) %>%
      setView(lng = 4.403268, lat  = 51.094453, zoom = 8) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 1,
                  fillOpacity = 0.5,
                  fillColor =  ~pal_hab(Hbtt_ct))
    
    ####1.2 Model output exacte pixels 
    if(input$MODstartpop == "Exacte_pixels") { 
      Pixelsshape <- subset(Pixelsmap, !is.na(Pixelsmap@data$Mdl_EP_))
      Pixelsshape$Mdl_EP_ <- as.factor(Pixelsshape$Mdl_EP_)
      pal_mod <- colorFactor(
        palette = c("deepskyblue1", "deepskyblue3", "deepskyblue4"), 
        domain = Pixelsshape$Mdl_EP_,
        na.color = NA) 
      Pixel_tot_ID_start <- subset(Pixelsmap, Pixelsmap@data$Strt_EP == 2019)
      pal_start <- colorFactor(
        palette = c("black"), 
        domain = Pixel_tot_ID_start$Strt_EP,
        na.color = NA)  

      Output_finalyear <- Output_finalyear %>%
        addPolygons(data = Pixelsshape,
                    stroke = FALSE,
                    smoothFactor = 1,
                    fillOpacity = 1,
                    fillColor =  ~pal_mod(Mdl_EP_)) %>%
        addPolygons(data = Pixel_tot_ID_start,
                    stroke = FALSE,
                    smoothFactor = 1,
                    fillOpacity = 1,
                    fillColor =  ~pal_start(Strt_EP))%>%
        addLegend("bottomright", pal = pal_mod, values = ~Mdl_EP_,
                  title = "Waarschijnlijkheid verspreiding",
                  opacity = 1,
                  na.label = "")  %>%
        addLegend("bottomleft", pal = pal_hab, values = ~Hbtt_ct,
                  title = "Habitatsgeschiktheid",
                  opacity = 1) %>%
        addLegend("bottomright", pal = pal_start, values = ~Strt_EP,
                  title = "Startlocatie",
                  opacity = 1)
    }
    
    ####1.3 Model output Optimaal habitat 
    if(input$MODstartpop == "Optimaal_habitat") { 
      Pixelsshape <- subset(Pixelsmap, !is.na(Pixelsmap@data$Mdl_OH_))
      Pixelsshape$Mdl_OH_ <- as.factor(Pixelsshape$Mdl_OH_)
      pal_mod <- colorFactor(
        palette = c("deepskyblue1", "deepskyblue3", "deepskyblue4"), 
        domain = Pixelsshape$Mdl_OH_,
        na.color = NA) 
      Pixel_tot_ID_start <- subset(Pixelsmap, Pixelsmap@data$Strt_OH == 2019)
      pal_start <- colorFactor(
        palette = c("black"), 
        domain = Pixel_tot_ID_start$Strt_OH,
        na.color = NA)  

      Output_finalyear <- Output_finalyear %>%
        addPolygons(data = Pixelsshape,
                    stroke = FALSE,
                    smoothFactor = 1,
                    fillOpacity = 1,
                    fillColor =  ~pal_mod(Mdl_OH_)) %>%
        addPolygons(data = Pixel_tot_ID_start,
                    stroke = FALSE,
                    smoothFactor = 1,
                    fillOpacity = 1,
                    fillColor =  ~pal_start(Strt_OH))%>%
        addLegend("bottomright", pal = pal_mod, values = ~Mdl_OH_,
                  title = "Waarschijnlijkheid verspreiding",
                  opacity = 1,
                  na.label = "")  %>%
        addLegend("bottomleft", pal = pal_hab, values = ~Hbtt_ct,
                  title = "Habitatsgeschiktheid",
                  opacity = 1) %>%
        addLegend("bottomright", pal = pal_start, values = ~Strt_OH,
                  title = "Startlocatie",
                  opacity = 1)
    }
    Output_finalyear
  })
  
  ####1.4 Model output Gemeentes exacte pixels 
  output$Gemeente_mod <- renderLeaflet({
    if(input$MODstartpop == "Exacte_pixels") { 
      Gemeentemap <- subset(Gemeentemap, 
                            !is.na(Gemeentemap@data$M_EP_A_))
      pal <- colorFactor(
        palette = c('green', 'yellow', 'orange', 'red', 'darkgrey'),
        domain = Gemeentemap$M_EP_A_)
      Gemeentemap_model <-leaflet(Gemeentemap) %>%
        addTiles() %>%
        addPolygons(smoothFactor = 1,
                    fillOpacity = 0.5,
                    fillColor =  ~pal(M_EP_A_),
                    weight = 0.75,
                    color = 'black') %>%
        addLegend("bottomright", pal = pal, values = ~M_EP_A_,
                  title = "Waarschijnlijkheid",
                  opacity = 1)
    }
   
     ####1.5 Model output Gemeentes Optimaal habitat 
    if(input$MODstartpop == "Optimaal_habitat") { 
      Gemeentemap <- subset(Gemeentemap, 
                            !is.na(Gemeentemap@data$M_OH_A_)) 
      pal <- colorFactor(
        palette = c('green', 'yellow', 'orange', 'red', 'darkgrey'),
        domain = Gemeentemap$M_OH_A_)
      Gemeentemap_model <-leaflet(Gemeentemap) %>%
        addTiles() %>%
        addPolygons(smoothFactor = 1,
                    fillOpacity = 0.5,
                    fillColor =  ~pal(M_OH_A_),
                    weight = 0.75,
                    color = 'black') %>%
        addLegend("bottomright", pal = pal, values = ~M_OH_A_,
                  title = "Waarschijnlijkheid",
                  opacity = 1)
    }
    Gemeentemap_model
  })
  
  ###2. AVP risico maps 
  output$Pixel_avp <- renderLeaflet({

   ####2.1 AVP Pixelniveau Exacte pixels 
       if(input$AVPstartpop == "Exacte_pixels") {     
      Pixelsmap <- subset(Pixelsmap, 
                            !is.na(Pixelsmap@data$Rsc_ExP))
      Pixelsmap$Rsc_ExP <- factor (Pixelsmap$Rsc_ExP, 
                                            levels = c("Hoog risico","Gemiddeld risico", 
                                                       "Laag risico","Verwaarloosbaar risico"))
      factpal2 <- colorFactor(
        palette = c('red','orange', 'green','white'),
        domain = Pixelsmap$Rsc_ExP)
      pig_pixel_map <-leaflet(Pixelsmap) %>%
        addTiles() %>%
        addPolygons(stroke = FALSE,
                    smoothFactor = 1,
                    fillOpacity = 0.5,
                    fillColor =  ~factpal2(Rsc_ExP)) %>%
        addLegend("topleft", pal = factpal2, values = ~Rsc_ExP,
                  title = "Legende",
                  opacity = 1)
    }   
    
    ####2.2 AVP Pixelniveau Optimaal habitat 
    if(input$AVPstartpop == "Optimaal_habitat") {     
      Pixelsmap <- subset(Pixelsmap, 
                          !is.na(Pixelsmap@data$Rsc_OpH))
      Pixelsmap$Rsc_OpH <- factor (Pixelsmap$Rsc_OpH, 
                                   levels = c("Hoog risico","Gemiddeld risico", 
                                              "Laag risico","Verwaarloosbaar risico"))
      factpal2 <- colorFactor(
        palette = c('red','orange', 'green','white'),
        domain = Pixelsmap$Rsc_OpH)
      pig_pixel_map <-leaflet(Pixelsmap) %>%
        addTiles() %>%
        addPolygons(stroke = FALSE,
                    smoothFactor = 1,
                    fillOpacity = 0.5,
                    fillColor =  ~factpal2(Rsc_OpH)) %>%
        addLegend("topleft", pal = factpal2, values = ~Rsc_OpH,
                  title = "Legende",
                  opacity = 1)
    }
    pig_pixel_map
  })
  
  output$Gemeente_avp <- renderLeaflet({
    
      ####2.3 AVP Gemeentes exacte pixels 
    if(input$AVPstartpop == "Exacte_pixels") {     
      Gemeentemap$M_EP__G_ <- factor (Gemeentemap$M_EP__G_, 
                                             levels = c("Hoog risico","Gemiddeld risico", 
                                                        "Laag risico","Verwaarloosbaar risico"))
       pal <- colorFactor(
        palette = c('red','orange', 'green','white'),
        domain = Gemeentemap$M_EP__G_)
      Gemeentemap_AVP <-leaflet(Gemeentemap) %>%
        addTiles() %>%
        addPolygons(smoothFactor = 1,
                    fillOpacity = 0.5,
                    fillColor =  ~pal(M_EP__G_),
                    weight = 0.75,
                    color = 'black') %>%
        addLegend("topleft", pal = pal, values = ~M_EP__G_,
                  title = "Risico klasse",
                  opacity = 1) 
    }
    
    ####2.3 AVP Gemeentes optimaal habitat 
    if(input$AVPstartpop == "Optimaal_habitat") {     
      Gemeentemap$M_OH__G_ <- factor (Gemeentemap$M_OH__G_, 
                                      levels = c("Hoog risico","Gemiddeld risico", 
                                                 "Laag risico","Verwaarloosbaar risico"))
      pal <- colorFactor(
        palette = c('red','orange', 'green','white'),
        domain = Gemeentemap$M_OH__G_)
      Gemeentemap_AVP <-leaflet(Gemeentemap) %>%
        addTiles() %>%
        addPolygons(smoothFactor = 1,
                    fillOpacity = 0.5,
                    fillColor =  ~pal(M_OH__G_),
                    weight = 0.75,
                    color = 'black') %>%
        addLegend("topleft", pal = pal, values = ~M_OH__G_,
                  title = "Risico klasse",
                  opacity = 1) 
    }
    Gemeentemap_AVP
  })
}
########################################################################################

##App uitvoeren
shinyApp(ui, server)

