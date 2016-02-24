corr <- function(directory, threshold = 0) {
    ## 'directory' is a character of vector length 1 indicating 
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all variables) 
    ## required to compute the correlation between nitrate and
    ## sulfate; the default is set to 0
    
    ## Returns a numeric vector of correlation
    
    a = complete(directory)
    above_threshold = which(a$nobs >= threshold)
    ## returns the ids of the monitors that meet the threshold requirement
    
    cor_values = rep(NA,length(above_threshold))
    ## creates empty vector in which to store the cor values
    
    for(i in seq_along(above_threshold)) {
        
        monitorid = str_pad(above_threshold[i], 3, side = "left", pad = "0")
        filename = paste(directory, "/", monitorid, ".csv", sep = "")
        rawdata = read.csv(filename, header = TRUE)
        ## reads each data file that meets threshold requirement
        
        cor_values[i] = cor(rawdata$nitrate,rawdata$sulfate, 
                            use = "pairwise.complete.obs")
        ## uses only complete pairs within the dataset
   
    }
    
    cor_values
    
}