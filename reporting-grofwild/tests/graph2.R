
library(oaPlots)
library(oaColors)

# data
d2006 <- c(2, 3)
d2007 <- c(sample.int(5, size = 15, replace = TRUE), 
    sample.int(5, size = 2, replace = TRUE) + 5, 
    sample.int(5, size = 1, replace = TRUE) + 10, 
    sample.int(10, size = 0, replace = TRUE) + 20, 
    sample.int(10, size = 0, replace = TRUE) + 30)
d2008 <- c(sample.int(5, size = 14, replace = TRUE), 
    sample.int(5, size = 2, replace = TRUE) + 5, 
    sample.int(5, size = 1, replace = TRUE) + 10, 
    sample.int(10, size = 1, replace = TRUE) + 20, 
    sample.int(10, size = 0, replace = TRUE) + 30)
d2009 <- c(sample.int(5, size = 12, replace = TRUE), 
    sample.int(5, size = 5, replace = TRUE) + 5, 
    sample.int(5, size = 0, replace = TRUE) + 10, 
    sample.int(10, size = 1, replace = TRUE) + 20, 
    sample.int(10, size = 1, replace = TRUE) + 30)
d2010 <- c(sample.int(5, size = 8, replace = TRUE), 
    sample.int(5, size = 4, replace = TRUE) + 5, 
    sample.int(5, size = 1, replace = TRUE) + 10, 
    sample.int(10, size = 0, replace = TRUE) + 20, 
    sample.int(10, size = 2, replace = TRUE) + 30)
d2011 <- c(sample.int(5, size = 11, replace = TRUE), 
    sample.int(5, size = 1, replace = TRUE) + 5, 
    sample.int(5, size = 4, replace = TRUE) + 10, 
    sample.int(10, size = 1, replace = TRUE) + 20, 
    sample.int(10, size = 3, replace = TRUE) + 30)
d2012 <- c(sample.int(5, size = 10, replace = TRUE), 
    sample.int(5, size = 4, replace = TRUE) + 5, 
    sample.int(5, size = 5, replace = TRUE) + 10, 
    sample.int(10, size = 3, replace = TRUE) + 20, 
    sample.int(10, size = 6, replace = TRUE) + 30)
d2013 <- c(sample.int(5, size = 11, replace = TRUE), 
    sample.int(5, size = 5, replace = TRUE) + 5, 
    sample.int(5, size = 8, replace = TRUE) + 10, 
    sample.int(10, size = 1, replace = TRUE) + 20, 
    sample.int(10, size = 8, replace = TRUE) + 30)
d2014 <- c(sample.int(5, size = 12, replace = TRUE), 
    sample.int(5, size = 6, replace = TRUE) + 5, 
    sample.int(5, size = 6, replace = TRUE) + 10, 
    sample.int(10, size = 1, replace = TRUE) + 20, 
    sample.int(10, size = 8, replace = TRUE) + 30)


dataVals <- c(d2006, d2007, d2008, d2009, d2010, d2011, d2012, d2013, 
   d2014)
dataYears <- c(rep(2006, length(d2006)), 
    rep(2007, length(d2007)),
    rep(2008, length(d2008)),
    rep(2009, length(d2009)),
    rep(2010, length(d2010)),
    rep(2011, length(d2011)),
    rep(2012, length(d2012)),
    rep(2013, length(d2013)),
    rep(2014, length(d2014))
        )
        
data <- cbind.data.frame(dataYears, dataVals)
colnames(data) <- c("Year", "Value")
data$Year <- factor(data$Year)

provinces <- c(rep("Antwerpen", 70), 
    rep("Vlaams Brabant", 65), 
    rep("Oost-Vlanderen", 65), 
    rep("Limburg", 44), 
    rep("West-Vlaanderen", 64)
    )

data$Province <- sample(provinces, nrow(data))
    

# plot

library(ggplot2)
# Basic dot plot
pdf("graph2.pdf", width = 10, height = 10)
p <- ggplot(data, aes(x=Year, y=Value, color=factor(Province))) + 
    geom_dotplot(binaxis='y', stackdir='center', 
        stackratio = 0.6, dotsize = 0.5)
p
p + stat_summary(fun.y=mean, geom="point", shape=18,
    size=8, color="red")
dev.off()

##
#dataDir <- system.file("extdata", package = "grofWild")
#
## extract the 'countsData' object from the package
#load(file.path(dataDir, "countsData.RData"))
#
#head(countsData$commune)
##countsData$commune$year <- factor(countsData$commune$year)
#nCommunesPerSpecieYearCount <- ddply(countsData$commune, c("specie", "year", "counts"), nrow)
#nCommunesPerSpecieYearCount$year <- factor(nCommunesPerSpecieYearCount$year)
#dataPlot <- subset(nCommunesPerSpecieYearCount, specie == "moeflon")
#dataPlot$year <- factor(dataPlot$year)
#
## scatterplot of number of communes by number of animals colored by year (as graph2 but scatterplot)
#ggplot(data = dataPlot, aes(x = counts, y = V1, color = year)) +
#	geom_line() +
#	labs(x = "aantal dieren", y = "aantal gemeenten")
#
## number of animals by 
##ggplot(
##	data = subset(countsData$commune, specie = "moeflon"),
##	aes(x = year, y = counts, color = NAAM)) +
##	geom_line() + guides(color = FALSE)
#
## for provinces
#nProvincesPerSpecieYearCount <- ddply(countsData$province, c("specie", "year", "counts"), nrow)
#nProvincesPerSpecieYearCount$year <- factor(nProvincesPerSpecieYearCount$year)
#dataPlot <- subset(nProvincesPerSpecieYearCount, specie == "moeflon")
#dataPlot$year <- factor(dataPlot$year)
#
## scatterplot of number of provinces by number of animals colored by year (as graph2 but scatterplot)
#ggplot(data = dataPlot, aes(x = counts, y = V1, color = year)) +
#	geom_line() +
#	labs(x = "aantal dieren", y = "aantal gemeenten")
#
#countsData$province$year <- factor(countsData$province$year)
#
## histogram
#ggplot(data = countsData$province) + 
#	geom_histogram(aes(x = counts, fill = year, alpha = 0.2), 
#	binwidth = 1, position = 'identity')
#
#ggplot(data = countsData$province) + 
#	geom_freqpoly(aes(x = counts, col = year, alpha = 0.3), 
#		binwidth = 5)
#
#
#gg <- ggplot(
#	data = subset(countsData$province, specie = "moeflon")) + 
#	geom_density(aes(x = counts, col = year)) + 
#	theme_bw() +
#	labs(x = "aantal specie", ylab = "density aantal gemeenten",
#		title = "Distributie van aantal gemeenten per aantal moeflon")
#ggplotly(gg)
#
#countsData$commune$year <- factor(countsData$commune$year)
#pdf("graph2.pdf")
#ggplot(
#	data = subset(countsData$commune, specie = "moeflon"),
#	aes(x = counts, col = year)) + 
#	geom_density(aes(y = ..count..)) + 
#	theme_bw() +
#	labs(x = "aantal specie", y = "aantal gemeenten",
#		title = "Distributie van aantal gemeenten per aantal moeflons")
#dev.off()


