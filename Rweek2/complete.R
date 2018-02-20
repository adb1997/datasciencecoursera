complete <- function(directory, id=1:332){
  csv_files<-list.files(directory,full.names = TRUE)
  
  z<-data.frame()
  
  for(i in id){
    x<-read.csv(csv_files[i])
    nobs<-sum(complete.cases(x))
    y<-data.frame(i,nobs)
    z<-rbind(z,y)
  }
  
  colnames(z)<-c('id','nobs')
  return(z)  
  
}
complete("specdata", 1)

