# Write a function that reads a directory full of files 
#and reports the number of completely observed cases in each data file.
#The function should return a data frame where the first column is the name of the file
#and the second column is the number of complete cases.

library(stringr)

complete <- function(directory, id = 1:332){
        # example of binding all the files into one data frame
        #       monitors <- dir(directory, pattern = '\\.csv', full.names = TRUE)
        #       chosen_monitors <- monitors[id]
        #       tables <- lapply(chosen_monitors, read.csv)
        #       monitors_table <- do.call(rbind, tables)
        
        df <- data.frame("id"=numeric(0), "nobs"=numeric(0))
        
        # read files
        for (i in id) {
                formatted_id <- str_pad(i, 3, side = "left", pad = 0)
                full_path <- paste(directory, "/", formatted_id, ".csv", sep = "" )
                monitor <- read.csv(full_path)
                
                complete_cases <- monitor[complete.cases(monitor), ]
                df[nrow(df)+1, ] <- c(i, nrow(complete_cases))
        }
        df
}

# Quiz questions. What value is printed at end of the following code samples?
#5
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

#6
cc <- complete("specdata", 54)
print(cc$nobs)

#7
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])






