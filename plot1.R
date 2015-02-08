plot1<-function(){
rs<-read.csv("1.csv",sep=";",colClasses="character")
rs1<-filter(rs,month(dmy(Date))==2&year(dmy(Date))==2007)
rs2<-filter(rs1,day(dmy(Date))==1 | day(dmy(Date))==2)
rs3<-select(rs2,Global_active_power)
hist(as.numeric(rs3$Global_active_power),col="red",main="Global Active Power")

}