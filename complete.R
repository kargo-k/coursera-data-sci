complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the
    ## location of the CSV files
    
    ## 'id' is an integer vector indicating the monitoring ID numbers
    ## to be used
    
    ## Return the data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the 
    ## number of complete cases
    
    library(stringr)
    ## loads a library to properly pad the id numbers into the file format
    nobs = rep(0, length(id))
    
    for(i in seq_along(id)) {
        
        monitorid = str_pad(id[i], 3, side = "left", pad = "0")
        filename = paste(directory, "/", monitorid, ".csv", sep = "")
        rawdata = read.csv(filename, header = TRUE)
        ## reads the CSV files
        
        nobs_i = sum(complete.cases(rawdata))
        ## counts the number of complete cases within the file
        nobs[i] = nobs_i
        ## stores the count into the initialized vector
        
    }
    data.frame(id,nobs)
    ## combines the id and the number of complete cases into a data frame
}
