#Question3

rs1<-read.csv("GDP.csv",skip=4,nrow=231)
rs2<-read.csv("Country.csv")
rs3<-merge(rs1,rs2,by.x="X",by.y="CountryCode",all=TRUE)
head(arrange(rs3,desc(X.1)),13)

#Question4

rs4<-filter(rs3,Income.Group=="High income: nonOECD")
rs5<-filter(rs3,Income.Group=="High income: OECD")
mean(rs4$X.1)
mean(rs5$X.1)

#Question5

table(rs$X.1<=38,rs$Income.Group=="Lower middle income")