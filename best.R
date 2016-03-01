## Finds the best hospital in the state in regards the minimal mortality
## for a certain outcome (defined as "heart attack", "heart failure",
## or "pneumonia") within a 30-day period.

## The input 'state' is a character vector of length 1 that denotes the
## specified state in which to search for the best hospital

## The input 'outcome' is a character vector of length 1 that specifies
## the type of outcome to rank the hospital by.  The outcome can be 
## "heart attack", "heart failure", or "pneumonia".

## In the event of a tie, the hospital that comes first alphabetically
## is returned.

best <- function(state, outcome) {
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
    minVal <- min(targetData$mortalityRate)
    
    ## Finds the minimum mortality and its corresponding hospital
    n <- which(targetData$mortalityRate == minVal)
    bestHospitals <- targetData[n,]
    
    ## Orders the hospitals alphabetically
    ranked <- order(bestHospitals$hospitals)
    print(as.character(bestHospitals$hospitals[ranked[1]]))
}