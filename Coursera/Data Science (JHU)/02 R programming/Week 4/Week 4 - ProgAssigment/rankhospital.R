# code is similar to best function. For more details on this task see related PDF

rankhospital <- function(state, outcome, num = "best") {
        
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
        
        # Sort to have hospital with the lowest 30-day death rate in the top for each state
        # that's how we avoid ties
        sorted <- df_clean[order(df_clean$state, df_clean[[outcome]], df_clean$hospital), ]
        
        # Return hospital name in that state with the given rank 30-day death rate
        state_subset <- sorted[sorted$state == state, "hospital"]
        
        # checking rank
        if (num == "best") {
                state_subset[1] 
        }
        else if(num == "worst") {
                state_subset[length(state_subset)]
        }
        else {
                state_subset[num]
        }
}

###
# Quiz questions
###

#4
# rankhospital("NC", "heart attack", "worst")

#5
# rankhospital("WA", "heart attack", 7)

#6
# rankhospital("TX", "pneumonia", 10)

#7
# rankhospital("NY", "heart attack", 7)