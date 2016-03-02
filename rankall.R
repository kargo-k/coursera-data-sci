## Returns the nth ranked hospital in each state based on the given outcome

## The input 'outcome' is a character vector of length 1 that specifies
## the type of outcome to rank the hospital by.  The outcome can be 
## "heart attack", "heart failure", or "pneumonia".

## The input 'num' is the given rank.  The default value is "best", which 
## is rank #1.

## In the event of a tie, the hospital that comes first alphabetically
## is ranked higher.


rankall <- function(outcome, num = "best") {
    
    ## Reads data from outcome-of-care-measures.csv in the working directory
    data <- read.csv("outcome-of-care-measures.csv")
    outcomelist <- c("heart attack","heart failure","pneumonia")
    targetOutcome <- which(outcome == outcomelist)
    
    ## Check that the outcome input is valid
    if (length(targetOutcome) == 0) {
        message("invalid outcome")
        stop()
    }
    
    state <- unique(data$State)
    state <- state[order(state)]
    targetData <- data.frame(data$Hospital.Name, data$State)
    
    ## Return the hospital name in that state with the lowest 30-day 
    ## death rate for that particular outcome
    
    if (targetOutcome == 1) {  ## Subsets data for just heart attacks
        targetData <- data.frame(targetData, data[,11])
    } else if (targetOutcome == 2) {  ## Subsets data for just heart failure
        targetData <- data.frame(targetData, data[,17])
    } else if (targetOutcome == 3) {  ## Subsets data for just pneumonia
        targetData <- data.frame(targetData, data[,23])
    }
    
    ## Creates a single data.frame "targetData" that contains the mortality
    ## rates for the outcome and the corresponding hospitals
    names(targetData) <- c("Hospital", "State", "mortalityRate")
    targetData$mortalityRate <- as.numeric(as.character(targetData$mortalityRate))
    
    ## Ignores the hospitals with 'NA' data points
    targetData <- targetData[!is.na(targetData$mortalityRate),]
    sortedData <- targetData[order(targetData$State, targetData$mortalityRate,
                                   targetData$Hospital),]
    
    ## Splits sorted data into a list by state
    stateData <- split(sortedData,sortedData$State)
    
    if (num == "best") {
        num <- 1
        numHospital <- sapply(stateData, function(x) as.character(x[num,1]))
    } else if (num == "worst") {
        num <- as.numeric(sapply(stateData, nrow))
        numHospital <- num
        for (i in 1:length(num)) {
            subData <- stateData[[i]]
            numHospital[i] <- as.character(subData[num[i],1])
        }
    } else {
        numHospital <- sapply(stateData, function(x) as.character(x[num,1]))
    }
    
    hospital = numHospital
    rm('numHospital')
    
    ## returns data.frame of hospitals with given rank in each state
    result <- data.frame(hospital,state, row.names = state)

}