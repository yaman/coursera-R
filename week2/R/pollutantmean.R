pollutantmean <- function(directory, pollutant, id = 1:332) {
  rawData = c() 
  for (file in normalizedFiles(directory,id)) { 
    
    tableData <- read.csv(file) 
    column <- 2
    switch(pollutant,
           "sulfate"={ column <- 2 },
           "nitrate"={ column <- 3 }
           )
    tempData <- tableData[, column]
    usefulData <- complete.cases(tempData)
    usefulData <-tempData[usefulData]
   # print(usefulData[1:10])
    for (i in 1:length(usefulData)) 
      rawData <-c(rawData,usefulData[[i]])
    
  } 
  meanValue <- as.double(sprintf("%.3f", mean(rawData)))
  
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