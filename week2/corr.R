corr <- function(directory, threshold = 0) {
  correlations <- c()
  files <- list.files(directory,full.names = TRUE)
  for (file in files) {
    cases <- read.csv(file)
    completeCases <- complete.cases(cases)
    caseData <- cases[completeCases,][]
    
    if (nrow(caseData) > threshold) {
      correlations <- c(correlations, cor(caseData[,2], caseData[,3]))
    }
  } 
  correlations
}