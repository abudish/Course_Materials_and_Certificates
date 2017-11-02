#Write a function that takes a directory of data files and a threshold for complete cases
#and calculates the correlation between sulfate and nitrate for monitor locations
#where the number of completely observed cases (on all variables)
#is greater than the threshold. 
#The function should return a vector of correlations for the monitors
#that meet the threshold requirement. If no monitors meet the threshold requirement,
#then the function should return a numeric vector of length 0. 
corr <- function(directory, threshold = 0) {
        # initialize an empty vector of correlations
        correlations <- c(numeric())
        
        # extract ids of monitors where 
        df <- complete(directory)
        df_required <- subset(df, nobs != 0 & nobs > threshold)
        id <- df_required[["id"]]
        # nobs <- df[["nobs"]]
        # requirement <- nobs == threshold
        # id_bad <- df[["id"]][requirement]
        # df[["nobs"]][requirement] # remove ids where values equal 0
         
        for (i in id) {
                formatted_id <- str_pad(i, 3, side = "left", pad = 0)
                full_path <- paste(directory, "/", formatted_id, ".csv", sep = "" )
                monitor <- read.csv(full_path)

                complete_cases <- monitor[complete.cases(monitor), ]
                correl <- cor(complete_cases[["sulfate"]], complete_cases[["nitrate"]])
                correlations <- c(correl, correlations)
                
        }
        correlations
}
#Quiz: What value is printed at end of the following code?
#8
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

#9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

#10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

