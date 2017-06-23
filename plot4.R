## Plot 4

library(ggplot2)

plot4 <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    rows <- grep("[Cc]oal", SCC$EI.Sector)
    coal <- SCC[rows, ]
    coalrows <- which(NEI$SCC %in% coal$SCC )
    coalentries <- NEI[coalrows,]$Emissions
    
    byYear <- split(NEI$Emissions, NEI$year)
    sums <- lapply(byYear, sum, na.rm=TRUE)
    
    df <- do.call(rbind.data.frame, sums)
    df <- cbind(as.numeric(names(sums)), df)
    names(df) <- c("year", "USsum")
    
    
    p <- ggplot(data = df, aes(year, USsum)) + geom_point(shape=1, size=3)
    p <- p+ geom_smooth(method=lm, se=FALSE)
    print(p)
    
}