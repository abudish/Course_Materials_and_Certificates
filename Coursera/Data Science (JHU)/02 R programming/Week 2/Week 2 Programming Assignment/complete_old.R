# Write a function that reads a directory full of files 
#and reports the number of completely observed cases in each data file.
#The function should return a data frame where the first column is the name of the file
#and the second column is the number of complete cases.

library(stringr)
library(data.table)

complete <- function(directory, id = 1:332){
        # example of binding all the files into one data frame
        #       monitors <- dir(directory, pattern = '\\.csv', full.names = TRUE)
        #       chosen_monitors <- monitors[id]
        #       tables <- lapply(chosen_monitors, read.csv)
        #       monitors_table <- do.call(rbind, tables)
        
        # create 2 empty vectors, one for ids and another for complete cases
        #ids <- numeric()
        #nobs <- numeric()
        df <- data.frame("id"=numeric(0), "nobs"=numeric(0))
        
        # read files
        for (i in id) {
                formatted_id <- str_pad(i, 3, side = "left", pad = 0)
                
                full_path <- paste(directory, "/", formatted_id, ".csv", sep = "" )
                
                monitor <- read.csv(full_path)
                complete_cases <- monitor[complete.cases(monitor), ]
                #nobs <- c(nrow(complete_cases), nobs)
                #ids <- c(i, ids )
                #df <- rbind(df, c(i, nrow(complete_cases)))
                df[nrow(df)+1, ] <- c(i, nrow(complete_cases))
        }
        
        # create a data frame from ids and complete cases vector
        #df <- data.frame(ids, nobs)
        #names(df) < c("id", "nobs")
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

#8 
complete("specdata", c(2, 4, 8, 10, 12))

complete("specdata", 30:25)






