

rankall <- function(outcome, num = "best") {
        
        # reading file
        outcome_file <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        # subsetting only the columns that we need
        df <- outcome_file[, c(2, 7, 11, 17, 23)]
        names(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        ## Check that outcome is valid
        states <- unique(df[, "state"])
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
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
        
        ## For each state, find the hospital of the given rank
        split_data <- split(sorted, sorted$state)
        
        # write a function to apply on list of data frames
        hospitalNameFunction <- function(df) {

                # Return hospital name in that state with the given rank 30-day death rate
                hospitals_subset <- df[, "hospital"]

                # checking rank
                if (num == "best") {
                        hospitals_subset[1]
                }
                else if(num == "worst") {
                        hospitals_subset[length(hospitals_subset)]
                }
                else {
                        hospitals_subset[num]
                }

        }
        
        # apply function to return named list of hospitals by states
        named_hospitals <- sapply(split_data, hospitalNameFunction)
        data.frame(hospital=named_hospitals,
                   state=names(named_hospitals),
                   row.names=names(named_hospitals) )
        
        
}

###
# Quiz questions
###

#8
# r <- rankall("heart attack", 4)
# as.character(subset(r, state == "HI")$hospital)

#9
# r <- rankall("pneumonia", "worst")
# as.character(subset(r, state == "NJ")$hospital)

#10
# r <- rankall("heart failure", 10)
# as.character(subset(r, state == "NV")$hospital)