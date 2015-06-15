corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        df <- complete(directory)
        id <- which(df$nobs > threshold)
        c <- numeric(0)
        if (length(id) >= 1){
                c <- numeric(length(id))
                for (i in 1:length(id)){
                        idx <- sprintf("%03d.csv",id[i])
                        filename <- file.path(directory,idx)       
                        data <- read.csv(file = filename)
                        datacomplete <- data[complete.cases(data),]
                        c[i] <- cor(datacomplete$sulfate, datacomplete$nitrate)
                }
        }
        c
}