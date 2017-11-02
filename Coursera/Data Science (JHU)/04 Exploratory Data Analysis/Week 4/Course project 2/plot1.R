# Reading data
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")      
} 

# Calculate total(sum) emissions by year
total <- with(NEI, tapply(Emissions, year, sum))

# Supress showing amount of PM2.5 in exponential notation (e.g 7e+06)
options(scipen=2)

png(file = "plot1.png")

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission
# from all sources for each of the years 1999, 2002, 2005, and 2008.
barplot(total, names.arg = names(total),
        xlab = "Years",
        ylab = "Amount of PM2.5 emitted, in tons",
        main = "Total emissions from in the US from 1999 to 2008")

dev.off()
