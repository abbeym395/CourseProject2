## Plot 3 using ggplot2

library(ggplot2)

plot3 <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    baltimore <- NEI[NEI$fips == "24510", ]
    # baltimore <- split(baltimore, baltimore$year)
    # baltimore <- baltimore[c("1999", "2008")]
    # baltimore <- do.call("rbind", baltimore)
    
    p <- ggplot(baltimore, aes(factor(year), log10(Emissions)))
    p <- p + facet_grid(.~type)
    p <- p + geom_boxplot()
    print(p)
}