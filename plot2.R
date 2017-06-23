## Plot 2 code      fips == "24510"

plot2 <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    baltimore <- NEI[NEI$fips == "24510", ]
    byYear <- split(baltimore$Emissions, baltimore$year)
    
    sums <- lapply(byYear, sum)
    years <- unique(baltimore$year)
    
    plot(years, sums)
    abline(lm(unlist(sums) ~ years))
}