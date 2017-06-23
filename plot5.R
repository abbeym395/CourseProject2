## Plot 5

plot5 <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    rows <- grep("On-Road", SCC$EI.Sector)
    road <- SCC[rows, ]
    roadrows <- which(NEI$SCC %in% road$SCC )
    roadentries <- NEI[roadrows,]
    
    baltiroad <- roadentries[roadentries$fips == "24510", ]
    
    byYear <- split(baltiroad$Emissions, baltiroad$year)
    sums <- lapply(byYear, sum, na.rm=TRUE)
    
    df <- do.call(rbind.data.frame, sums)
    df <- cbind(as.numeric(names(sums)), df)
    names(df) <- c("year", "baltisum")
    
    
    p <- ggplot(data = df, aes(year, baltisum)) + geom_point(shape=1, size=3)
    p <- p+ geom_smooth(method=lm, se=FALSE)
    print(p)
}