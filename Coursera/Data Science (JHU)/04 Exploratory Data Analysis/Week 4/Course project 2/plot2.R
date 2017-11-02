# Reading data
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")      
}

baltimore <- subset(NEI, fips == "24510")

# Calculate total(sum) emissions by year for the Baltimore City
total <- with(baltimore, tapply(Emissions, year, sum))

png(file = "plot2.png")

# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (fips == "24510") from 1999 to 2008?
barplot(total, names.arg = names(total),
     xlab = "Years",
     ylab = "Amount of PM2.5 emitted, in tons",
     main = "Total emissions in the Baltimore city from 1999 to 2008")

dev.off()
