complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        df <- data.frame(id = id, nobs = numeric(length(id)))
        for (i in 1:length(id)){
                idx <- sprintf("%03d.csv",id[i])
                filename <- file.path(directory,idx)       
                data <- read.csv(file = filename)
                df$nobs[i] <- sum(complete.cases(data))
        }
        df
}