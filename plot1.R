## Plot 1 code

plot1 <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    byYear <- split(NEI$Emissions, NEI$year)
    sums <- lapply(byYear, sum)
    
    years <- unique(NEI$year)
    
    plot(years, sums)
    abline(lm(unlist(sums) ~ years))
}