
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

data$Province <- sample( provinces, nrow(data))
    

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

