pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character of vector length 1 indicating 
    ## the location of the CSV files
    
    ## 'pollutant' is a character of vector length 1 indicating
    ## the name of the pollutant for which we will calculate the mean;
    ## either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors listed in
    ## the 'id' vector (ignoring NA values)
    ## Note: do not round the results!
    
    library(stringr)
    alldata = numeric()
    
    for(i in seq_along(id)) {
        
        monitorid = str_pad(id[i], 3, side = "left", pad = "0")
        filename = paste(directory, "/", monitorid, ".csv", sep = "")
        rawdata = read.csv(filename, header = TRUE)
        
        if(pollutant == "nitrate") {
            subdata = rawdata$nitrate
        } else if(pollutant == "sulfate") {
            subdata = rawdata$sulfate
        } else {
            break
        }
        
        cleandata = subdata[!is.na(subdata)]
        alldata = c(alldata,cleandata)
        
    }
    mean(alldata)
}