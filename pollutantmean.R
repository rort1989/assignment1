pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        s <- vector("numeric", length = length(id))
        nacount <- vector("numeric", length = length(id))
        for (i in 1:length(id)){
                idx <- sprintf("%03d.csv",id[i])
                filename <- file.path(directory,idx)       
                data <- read.csv(file = filename)
                s[i] <- sum(data[,c(pollutant)],na.rm = TRUE)
                nacount[i] <- sum(!is.na(data[,c(pollutant)]))
        }
        m <- sum(s)/sum(nacount)  # make sure you state the last line as the value returned
}