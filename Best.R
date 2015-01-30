best<-function(State,Outcome){
caremeasures<-read.csv("outcome-of-care-measures.csv")
ValidState<-unique(caremeasures[,7])
ValidOutcome<-c("heart attack","pneumonia","heart failure")

if(!State%in%ValidState)
{
stop("invalid state")
}
if(!Outcome%in%ValidOutcome)
{
stop("invalid outcome")
}
outcome1<-colnames(caremeasures)[11]
outcome2<-colnames(caremeasures)[17]
outcome3<-colnames(caremeasures)[23]
outcomenames<-c(outcome1,outcome2,outcome3)
result1<-caremeasures[caremeasures$State==State,]
coln<-outcomenames[match(Outcome,ValidOutcome)]
result2<-which.min(as.double(result1[,coln]))
result1[result2,"Hospital.Name"]
}