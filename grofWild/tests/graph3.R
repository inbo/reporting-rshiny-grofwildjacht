library(oaPlots)
library(oaColors)

# data

# min, median, max, new
vec <- c(2, 5, 16, 8, 
    4, 10, 15, 5, 
    6, 12, 14, 7, 
    7, 9, 12, 6, 
    7.5, 8.5, 13, 5.5, 
    7.6, 7.9, 17, 11.5, 
    6.7, 8, 17, 13, 
    3, 6.5, 12, 6, 
    3.5, 5, 6, 4.5, 
    3.8, 12.5, 17.5, 7.7, 
    3, 6.2, 9, 10.7, 
    3, 5, 7.5, 14.5)
data <- matrix(vec, ncol = 4, byrow = TRUE)
colnames(data) <- c("min", "median", "max", "new")
data <- data.frame(data)

makeData <- function(data) {
  dataNew <- matrix(0, ncol = 10, nrow = 12)
  colnames(dataNew) <- 2005:2014
  for(i in 1:10) {
    for(j in 1:12) {
      
      dataNew[j, i] <- runif(1, min = data$min[j], max = data$max[j])
      
    }
  }
  return(dataNew)
}

dataNew <- makeData(data)
rownames(dataNew) <- c("Januari", "Februari", 
    "Maart", "April", "Mei", "Juni", "Juli", "Augustus", 
    "September", "Oktober", "November", "December")

# plot
pdf("graph3.pdf", width = 10, height = 10)
prepLegend(side = "bottom")
oaTemplate(xlim = c(1, 12), ylim = c(min(data), max(data)), 
    ylab = "Percentage jaarlijkse afschot", 
    xlab = "Maand", xgrid = 1:12, xlabels = c("Januari", "Februari", 
        "Maart", "April", "Mei", "Juni", "Juli", "Augustus", 
        "September", "Oktober", "November", "December"))
for(i in 1:nrow(data)) {
  segments(x0 = i, y0 = data$min[i], y1 = data$max[i], lwd = 2.5)
  segments(x0 = i-0.1, x1 = i+0.1, y0 = data$min[i], lwd = 2.5)
  segments(x0 = i-0.1, x1 = i+0.1, y0 = data$median[i], lwd = 2.5)
  segments(x0 = i-0.1, x1 = i+0.1, y0 = data$max[i], lwd = 2.5)
  points(x = i, y = data$new[i], pch = 19, col = oaColors("blue"), 
      cex = 2.5)
}
par(xpd = NA)
addLegend(x = 0.5, y = 0.5, pch = c(19, NA), lwd = c(NA, 2), 
    legend = c("New Data", "Old Data (Min, Median, Max)"), 
    col = c(oaColors("blue"), "black"))
dev.off()