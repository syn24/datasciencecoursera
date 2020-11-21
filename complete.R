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
  
  fulldir <- paste(getwd(), directory, sep="/")
  #print(fulldir)
  result <- c()
  
  
  df <- data.frame(id=character(), 
                   nobs=character()) 

  for (var in id) {
    add <- ""
    if (nDigits(var) == 1) { add <- "00"}
    if (nDigits(var) == 2) { add <- "0"}
    #print(add)
    filedir <- paste(fulldir, paste(add, var, ".csv", sep=""), sep="/")
    #print(filedir)
    polluts <- read.csv(filedir, TRUE, sep=",")
    
    num <- 0
    
    for(i in 1:nrow(polluts)) {       # for-loop over rows
      if(!is.na(polluts$sulfate[i]) && !is.na(polluts$nitrate[i])) {
        num <- num +1;
      }
    }
    new_row <- c(var, num)
    df <- rbind(df, new_row) 
    
  }

  col_headings <- c("id","nobs")
  names(df) <- col_headings
  df
}

complete("specdata", 1)