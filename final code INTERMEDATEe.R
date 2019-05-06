#initial steps- data reading and viewing
setwd("C:\\INTERMEDIATE\\Intermediate")
getwd()
dir()
pdata<-read.csv("tmprrtdffu2.csv")
View(pdata)
head(pdata)
pdata[]
pdata[][is.na(pdata[])] <- 0
View(pdata)
#package installations and library read
library(ggplot2)
install.packages(VIM)
library(VIM)
#checking purity of data
a<-aggr(pdata)
a

#checking uniqueness of variables in data
d<-unique(pdata$OFFENSE_CODE_GROUP)
d
o<-unique(pdata$REPORTING_AREA)
o
head(d,10)
head(o,10)
e<-unique(pdata$DISTRICT)
e
f<-unique(pdata$OFFENSE_CODE)
f
head(e,10)
head(f,10)

#visualizations and EDA where required

ggplot(pdata, aes(x=DISTRICT)) +
  geom_bar()
ggplot(pdata, aes(x=MONTH)) +
  geom_bar()
ggplot(pdata, aes(x=YEAR)) +
  geom_bar()
ggplot(pdata, aes(x=HOUR)) +
  geom_bar()
ggplot(pdata, aes(x=DAY_OF_WEEK)) +
  geom_bar()



l<-sort(table(pdata$STREET),decreasing = TRUE)[3:12]
l
m<-sort(table(pdata$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]
m
n<-sort(table(pdata$REPORTING_AREA),decreasing=TRUE)[2:11]
n


headdata<-head(pdata,10)
View(headdata)



headdata$newcolumn<-c(30319,28046,21559,20916,19293,18462,17720,15427, 13117,13102 )
headdata$newcolumn1<-c("Larceny","Medical Assistance","Investigate Person","Other","Drug Violation", "Simple Assault", "Vandalism","Verbal Disputes","Towed","Investigate Property")
View(headdata)
library(ggplot2)
ggplot(headdata, aes(x=newcolumn1, y=newcolumn))+
  geom_bar(stat = "identity") + 
  coord_flip()+
  labs(y = "Type of offense", x = "Count",title ="top 10 offense count")


  
headdata$newcolumn2<-c(3740,4107,4861,5175,5445,5468,5597,5928, 8405,9143 )
headdata$newcolumn3<-c("columbia rd","hyde park ave","commonwalth ave","centre st","harrison ave", "mass ave", "Tremont st","dorchester ave","boylston","blue hill ave")
library(ggplot2)
ggplot(headdata,aes(x=newcolumn3,y=newcolumn2))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="count", x="area name",title="top areas involved")




#considering only 2016 data and showing highest offense codes reported

library(tidyverse)

ggg<-filter(pdata,YEAR==2016)
ggg
View(ggg)
table(ggg$OFFENSE_CODE_GROUP)
ocg1<-sort(table(ggg$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]
ocg1

headdata$newcolumn4<-c(7920,6982,5766,5542,5302,5067,4744,4100,3521,3360)
headdata$newcolumn5<-c("larceny","medical assistance","Investigate Person","Other","Drug Violation", "Vandalism", "simple assault","verbal disputes","motor vehicle larency","investigate property")

ggplot(headdata,aes(x=newcolumn5,y=newcolumn4))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="area name", x="count",title="2016 highest reported areas")

sum(headdata$newcolumn4)




#2017

aaa<-filter(pdata,YEAR==2017)
aaa
View(aaa)
ocg2<-sort(table(aaa$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]
ocg2
headdata$newcolumn6<-c(7825,7820,6662,5328,4900,4840,4748,4438,3981,3954)
headdata$newcolumn7<-c("medical assistance","larceny","Investigate Person","Other","simple assault", "Vandalism", "drug violation","verbal disputes","investigate property","towed")
ggplot(headdata,aes(x=newcolumn7,y=newcolumn6))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="area name", x="count",title="2017 highest reported areas")



sum(headdata$newcolumn6)






#2018
bbb<-filter(pdata,YEAR==2018)
bbb
View(bbb)
ocg3<-sort(table(bbb$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]
ocg3
headdata$newcolumn8<-c(8241,8072,5586,5453,5256,4793,4424,4247,3753,3709)
headdata$newcolumn9<-c("medical assistance","larceny","Other","Investigate Person","simple assault","Drug Violation","verbal disputes", "Vandalism","investigate property","towed")

ggplot(headdata,aes(x=newcolumn9,y=newcolumn8))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="area name", x="frequency",title="2018 highest reported areas")


sum(headdata$newcolumn8)




#as we see larceny is occuring most often in all the years, we further dig deeper to see what kind of larceny happens the most for all the 3 years
ldff<-filter(pdata,OFFENSE_CODE_GROUP=='Larceny')
View(ldff)


ggplot(ldff, aes(x=(OFFENSE_DESCRIPTION))) +
  coord_flip()+
  geom_bar()

ggplot(ldff, aes(x=OFFENSE_DESCRIPTION, y=MONTH))+
  geom_point(alpha=0.2) + 
  coord_flip()+
  labs(y = "Type of offense", x = "Count",title ="top 10 offense count")





#data frame creation

tb<-table(pdata$OFFENSE_CODE_GROUP)
tb
tdf<-as.data.frame(tb)
tdf$Var1 <- NULL
View(tdf)


offensedata <- ts(tdf,start=c(100))
offensedata

attributes(offensedata)
summary(offensedata)
cycle(offensedata)
plot(offensedata,main="offense count",xlab="",ylab="frequency")
aggregate(offensedata) # count
abc <- aggregate(offensedata, FUN=mean) # mean

#time series forecasting

install.packages("forecast")
library(forecast)

model <- auto.arima(abc,ic='aic',trace = TRUE,seasonal = FALSE)
model 
plot.ts(model$residuals,main="auto arima model")
Acf(ts(model$residuals))
Pacf(ts(model$residuals))
attributes(model)

#testing the model
Box.test(model$residuals,lag = 3, type = 'Ljung-Box')
Box.test(model$residuals,lag = 14, type = 'Ljung-Box')

offense_forecast <- forecast(abc,12)
library(ggplot2)
autoplot(offense_forecast)
accuracy(offense_forecast)


