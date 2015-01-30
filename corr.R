corr<- function(directory,threshold=0){
i<-1
result<-vector(mode="logical",length=0)
for(filenames in dir(directory))
{
datafiles<-paste(directory,'/',filenames,sep="")
datas<-read.csv(datafiles)
if(sum(complete.cases(datas))>threshold){
m<-datas$sulfate
n<-datas$nitrate
result[i]<-cor(m,n,use="pairwise.complete.obs")
i<-i+1
}
else{

}
}

return(result)
}