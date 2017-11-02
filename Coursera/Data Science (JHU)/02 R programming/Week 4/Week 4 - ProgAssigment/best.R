# Write a function called best that take two arguments: the 2-character 
# abbreviated name of a state and an outcome name. 
# The function reads the outcome-of-care-measures.csv file and returns
# a character vector with the name of the hospital that has the best (i.e. lowest)
# 30-day mortality for the specified outcome in that state. 
# The hospital name is the name provided in the Hospital.Name variable.
# The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
# Hospitals that do not have data on a particular outcome
# should be excluded from the set of hospitals when deciding the rankings.

# Handling ties.
# If there is a tie for the best hospital for a given outcome, 
# then the hospital names should be sorted in alphabetical order 
# and the first hospital in that set should be chosen 
# (i.e. if hospitals “b”, “c”,and “f” are tied for best,
# then hospital “b” should be returned).

#library(dplyr)


best <- function(state, outcome) {
        
        # reading file
        outcome_file <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        # subsetting only the columns that we need
        df <- outcome_file[, c(2, 7, 11, 17, 23)]
        names(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        states <- unique(df[, "state"])
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if ( !(state %in% states) ) {
                stop("invalid state")
        }
        
        if ( !(outcome %in% outcomes) ){
                stop("invalid outcome")
        }
        
        # Filter only needed columns
        df_filtered <- df[, c("hospital", "state", outcome)]
        
        # Removing rows with NAs:
        df_clean <- na.omit(df_filtered)
        
        # Sort to have hospital with the lowest 30-day death rate in the top of a state
        # that's how we avoid ties
        sorted <- df_clean[order(df_clean$state, df_clean[[outcome]], df_clean$hospital), ]
        
        # Return hospital name in that state with lowest 30-day death rate - 1st row of state subset
        sorted[sorted$state == state, "hospital"][1]
}


###
# Quiz questions
###

#1
# best("SC", "heart attack")

#2
# best("NY", "pneumonia")

#3
# best("AK", "pneumonia")