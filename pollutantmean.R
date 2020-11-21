nDigits <- function(x) nchar( trunc( abs(x) ) )

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
 
  fulldir <- paste(getwd(), directory, sep="/")
  #print(fulldir)
  
  result <- c()
  
  for (var in id) {
    add <- ""
    if (nDigits(var) == 1) { add <- "00"}
    if (nDigits(var) == 2) { add <- "0"}
    #print(add)
    filedir <- paste(fulldir, paste(add, var, ".csv", sep=""), sep="/")
    #print(filedir)
    polluts <- read.csv(filedir, TRUE, sep=",")
    #print(polluts)
    if (pollutant == "sulfate") { pollno <- 2 } else { pollno <- 3}
    column <- polluts[[pollno]]
    #print(class(column))
    result <- c(result, na.omit(column))
  }
  #print(result)
  #print(class(result))
  #result <- as.numeric(result)
  mean(result)
}

pollutantmean("specdata", "sulfate", 1:5)

