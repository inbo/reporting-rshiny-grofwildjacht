
library(oaPlots)
library(oaColors)

# e.g. try to compare 2012 v 2014 in Limburg

# data
voeren <- c(1, 7, 12, 26, 52, 28, 29, 34, 68)
limburg <- c(NA, 20, 28, 68, 91, 190, 400, 444, 
    410)
antwerpen <- c(NA, NA, NA, 3, NA, NA, 8, 15, 38)
vlaamsBrabant <- c(NA, 1, 2, 1, 1, 1, 4, 6, 0)
oostVlanderen <- c(NA, 1, 1, 2, 1, 2, 0, 2, 0)
westVlanderen <- c(20, 15, 7, 5, 8, 41, 78, 7)

data <- rbind.data.frame(voeren, limburg, antwerpen, 
    vlaamsBrabant, oostVlanderen, westVlanderen)
colnames(data) <- 2006:2014
rownames(data) <- c("Voeren", "Limburg", "Antwerpen", 
    "Vlaams-Brabant", "Oost-Vlaanderen", "West-Vlaanderen")

data



# plot
colPal <- oaPalette(nrow(data))

pdf("graph1.pdf", width = 10, height = 10)
prepLegend(side = "bottom")
oaTemplate(xlim = c(1, 9), ylim = c(0, max(data, na.rm = TRUE)), 
    ylab = "Aantal everzwijnen (afschot en valwild)", 
    xlab = "Jaar", xgrid = 1:9, xlabels = 2006:2014)
for(i in 1:nrow(data))
  points(1:ncol(data), 
      data[i, ], type = "b", pch = 19, col = colPal[i])
par(xpd = NA)
addLegend(x = 0.5, y = 0.8, pch = 19, lwd = 2, 
    legend = c("Voeren", "Limburg", "Antwerpen", 
        "Vlaams Brabant", "Oost Vlanderen", "West Vlanderen"), 
    col = colPal)
dev.off()



# plotly
plotData <- reshape(data = data, varying = list(as.character(2006:2014)), 
    v.names = "aantal", ids = rownames(data), direction = "long")
plotData$time <- as.character(2006:2014)[plotData$time]
rownames(plotData) <- NULL
plot_ly(x = plotData$time, y = plotData$aantal, color = plotData$id,
        type = "scatter", mode = "lines+markers") %>% 
    layout(xaxis = list(title = "Jaar"), 
        yaxis = list(title = "Aantal everzwijnen (afschot en valwild)"))  