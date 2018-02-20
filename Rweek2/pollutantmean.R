pollutantmean<-function(directory,pollutant,id = 1:332){
  csv_files<-list.files(directory,full.names = TRUE)
  
  z<-data.frame()
  
  for (i in id){
    x<-read.csv(csv_files[i])
    z<-rbind(z,x)
      }
  return(mean(z[,pollutant],na.rm=TRUE))
  
}

pollutantmean("specdata", "sulfate", 1:10)