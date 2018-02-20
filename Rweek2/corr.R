corr <- function (directory, threshold=0){
  csv_files<-list.files(directory,full.names = TRUE)
  v <- vector(mode = "numeric", length = 0)
  
  
  
  for (i in 1:length(csv_files)) {
    
    
    x <- read.csv(csv_files[i])
    
    
    csum <- sum(complete.cases(x))
    
    if (csum > threshold) {
      
      
      xSulfate <- x[which(!is.na(x$sulfate)), ]
      
      
      xPollutant <- xSulfate[which(!is.na(xSulfate$nitrate)), ]
      
      
      v <- c(v, cor(xPollutant$sulfate, xPollutant$nitrate))
      
    }
  }
  
  return(v)
}
  
