
library(reportingGrofwild)
library(leaflet)
library(sp)

dataDir <- system.file("extdata", package = "reportingGrofwild")

provinceData <- readShapeData(zipFile = file.path(dataDir, "provinces.zip"))
plot(provinceData)
provinceData@data

leaflet(provinceData) %>%  
    addPolygons(
        weight = 1, 
        color = "white",
        fillColor = "black",
        fillOpacity = 0.8,
        layerId = seq_len(nrow(provinceData@data)),
        group = "region"
    ) 


communeData <-  readShapeData(zipFile = file.path(dataDir, "communes.zip"))
plot(communeData)
head(communeData@data)

leaflet(communeData) %>%  
    addPolygons(
        weight = 1, 
        color = "white",
        fillColor = "black",
        fillOpacity = 0.8,
        layerId = seq_len(nrow(communeData@data)),
        group = "region"
    )

table(substr(communeData$NISCODE, start = 1, stop = 1))
provinceData@data
