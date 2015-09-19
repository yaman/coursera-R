pollutantmean <- function(directory, pollutant, id = 1:332) {
  rawData <- 0
  for (file in normalizedFiles(directory,id)) { 
    rawData <- read.csv(file)  
  }
  
}

normalizedFiles <- function(directory, ids = 1:332){
  normalizedIds <- character(length(ids))
  
  for(i in 1:length(ids)){
    id <- ids[i]
    tempId <- "00"
    if(id < 10){
      tempId <- paste(directory,"/" ,"00", toString(id),".csv" ,sep="")
    }else if(id >= 10 && id<100){
      tempId <- paste(directory,"/" ,"0", toString(id), ".csv",sep="")
    }else if(id>=100){
      tempId <-  paste(directory,"/" , toString(id), ".csv",sep="")
    }
    normalizedIds[i] <- tempId
  }
  normalizedIds
}