#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate)
#across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data
#from the directory specified in the 'directory' argument
#and returns the mean of the pollutant across all of the monitors,
#ignoring any missing values coded as NA. A prototype of the function is as follows

library(stringr)
pollutantmean <- function(directory, pollutant, id = 1:332) {
        pollutant_values <- numeric()
        
        for (i in id) {
                formatted_id <- str_pad(i, 3, side = "left", pad = 0)
                
                full_path <- paste(directory, "/", formatted_id, ".csv", sep = "" )
                
                monitor <- read.csv(full_path)
                
                pollutant_file_values <- monitor[[pollutant]]
                good_pollutant_file_values <- pollutant_file_values[!is.na(pollutant_file_values)]
                pollutant_values <- c(good_pollutant_file_values, pollutant_values)
        }
        print(mean(pollutant_values))
}

# Quiz Questions(1-4)
# What value is returned by the following call to pollutantmean()?
# You should round your output to 3 digits.
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
