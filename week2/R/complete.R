complete <- function(directory, id = 1:332) {
  idList <- c()
  caseCount <- c()
  for (fileId in id) { 
    file <- normalizedFile(directory,fileId) 
    cases <- read.csv(file) 
    completeCases <- complete.cases(cases)
    caseData <- cases[completeCases,][]
    idList <- c(idList, fileId)
    caseCount <- c(caseCount, nrow(caseData)) 
  } 
   
  rawData <- data.frame(id = idList, nobs = caseCount) 
  rawData
}


normalizedFile <- function(directory, id){ 
    tempId <- "00"
    if(id < 10){
      tempId <- paste(directory,"/" ,"00", toString(id),".csv" ,sep="")
    }else if(id >= 10 && id<100){
      tempId <- paste(directory,"/" ,"0", toString(id), ".csv",sep="")
    }else if(id>=100){
      tempId <-  paste(directory,"/" , toString(id), ".csv",sep="")
    }
    tempId
}