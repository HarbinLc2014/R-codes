complete <- function(directory,id = 1:332) {
length<-length(id)
frame<-data.frame(id=NULL,nobs=NULL)
for(i in id){ 
   datafiles <- mapply(function(x) paste(directory,'/',sprintf("%03d",x),'.csv',sep=""),i)

vector<-c(i,sum(complete.cases(read.csv(datafiles))))
frame<-rbind(frame,vector)
}
colname<-c("id","nobs")
colnames(frame)<-colname
frame
}