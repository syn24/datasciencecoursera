corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  id = 1:332
  #headid = 1:33
  fulldir <- paste(getwd(), directory, sep="/")
  #print(fulldir)
  result <- c()
  
  for (var in id) {
    add <- ""
    if (nDigits(var) == 1) { add <- "00"}
    if (nDigits(var) == 2) { add <- "0"}
    filedir <- paste(fulldir, paste(add, var, ".csv", sep=""), sep="/")
    polluts <- read.csv(filedir, TRUE, sep=",")
    #print(nrow(polluts))
    
    polluts_complete <- polluts[complete.cases(polluts),]
    num <- nrow(polluts_complete)
    #print(num)
    
    #print(sum(polluts_complete$sulfate))
    #print(sum(polluts_complete$nitrate))
    #print(cor(polluts_complete$sulfate, polluts_complete$nitrate))
    #for(i in 1:nrow(polluts)) {       # for-loop over rows
    #  if(!is.na(polluts$sulfate[i]) && !is.na(polluts$nitrate[i])) {
    #    num <- num +1;
    #  }
    #}
    
    #print(num)
    if (num > threshold) {
      result <- c(result, cor(polluts_complete$sulfate, polluts_complete$nitrate))
      #df <- rbind(df, new_row) 
    }
  }
  if (length(result) == 0 ) result <- vector('numeric')
  result
}