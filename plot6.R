## Plot 6

plot6 <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    rows <- grep("On-Road", SCC$EI.Sector)
    road <- SCC[rows, ]
    roadrows <- which(NEI$SCC %in% road$SCC )
    roadentries <- NEI[roadrows,]
    
    baltiroad <- roadentries[roadentries$fips == "24510", ]
    LAroad <- roadentries[roadentries$fips == "06037", ]
    
    baltiByYear <- split(baltiroad$Emissions, baltiroad$year)
    baltisums <- lapply(baltiByYear, sum, na.rm=TRUE)
    
    LAByYear <- split(LAroad$Emissions, LAroad$year)
    LAsums <- lapply(LAByYear, sum, na.rm=TRUE)
    
    baltidf <- do.call(rbind.data.frame, baltisums)
    baltidf <- cbind(as.numeric(names(baltisums)), baltidf, rep("baltimore"))
    names(baltidf) <- c("year", "sum", "location")
    
    LAdf <- do.call(rbind.data.frame, LAsums)
    LAdf <- cbind(as.numeric(names(LAsums)), LAdf, rep("LA"))
    names(LAdf) <- c("year", "sum", "location")
    
    df <- rbind(baltidf, LAdf)
    
    p <- ggplot(data = df, aes(year, log10(sum))) + geom_point(shape=1, size=3)
    p <- p + facet_grid(.~location)
    p <- p + geom_smooth(method = lm, se = FALSE)
    print(p)
    
}