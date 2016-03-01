## Determines the nth ranking a hospital within a state based on the mortality
## rate of an outcome.

## The input 'state' is a character vector of length 1 that denotes the
## specified state in which to search for the best hospital

## The input 'outcome' is a character vector of length 1 that specifies
## the type of outcome to rank the hospital by.  The outcome can be 
## "heart attack", "heart failure", or "pneumonia".

## The input 'num' is the given rank.  The default value is "best", which 
## is rank #1.

## In the event of a tie, the hospital that comes first alphabetically
## is ranked higher.


rankhospital <- function(state, outcome, num = "best") {
    
    ## Reads data from outcome-of-care-measures.csv in the working directory
    data <- read.csv("outcome-of-care-measures.csv")
    outcomelist <- c("heart attack","heart failure","pneumonia")
    staterows <- as.character(data$State) == state
    targetOutcome <- which(outcome == outcomelist)
    
    ## Check that the state and outcome inputs are valid
    if (any(staterows) == FALSE) {
        message("invalid state")
        stop()
    }
    else if (length(targetOutcome) == 0) {
        message("invalid outcome")
        stop()
    }
    
    ## Return the hospital name in that state with the lowest 30-day 
    ## death rate for that particular outcome
    
    rows <- which(staterows) # Returns the row indicies of the correct states
    hospitals <- data$Hospital.Name[rows]
    
    if (targetOutcome == 1) {  ## Subsets data for just heart attacks
        mortalityRate <- data[rows,11]
    } else if (targetOutcome == 2) {  ## Subsets data for just heart failure
        mortalityRate <- data[rows,17]
    } else if (targetOutcome == 3) {  ## Subsets data for just pneumonia
        mortalityRate <- data[rows,23]
    }
    
    ## Creates a single data.frame "targetData" that contains the mortality
    ## rates for the outcome and the corresponding hospitals
    targetData <- data.frame(hospitals,mortalityRate)
    targetData$mortalityRate <- as.numeric(as.character(targetData$mortalityRate))
    
    ## Ignores the hospitals with 'NA' data points
    targetData <- targetData[!is.na(targetData$mortalityRate),]
    
    ## Orders the hospitals alphabetically
    rankings <- order(targetData$mortalityRate, targetData$hospitals)
    
    if (num == "best") {
        print(as.character(targetData$hospitals[rankings[1]]))
    } else if (num == "worst") {
        print(as.character(targetData$hospitals[rankings[length(rankings)]]))
    } else if (num > length(rankings)) {
        print(NA)
    } else {
        print(as.character(targetData$hospitals[rankings[num]]))
    }
    
}