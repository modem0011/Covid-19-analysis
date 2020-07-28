dataset<-read.csv(file.choose())
dataset

library(Hmisc)
describe(dataset)  


dataset$Date 
dataset$India 
dataset$Italy

library(lubridate)
dataset$Date<-dmy(dataset$Date)

par(mfrow=c(2,2))
plot(dataset$Date,dataset$India,xlab = "Date",ylab = "corona cases",main = "India")
plot(dataset$Date,dataset$Brazil,xlab = "Date",ylab = "corona cases",main = "Brazil")
plot(dataset$Date,dataset$Norway,xlab = "Date",ylab = "corona cases",main="Norway")
plot(dataset$Date,dataset$Italy,xlab = "Date",ylab = "corona cases",main = "Italy")            

ts(dataset$India,frequency = 1)
ts(dataset)
par(mfrow=c(1,1))
plot(ts(dataset$India,frequency = 1))



A<-data.frame (dataset$Date, dataset$India,  dataset$Italy, dataset$Norway, dataset$Israel, dataset$Iraq, dataset$Spain, dataset$Brazil, dataset$Belgium, dataset$Sweden)
B<-sapply(A, max)
mean(B)


par(mfrow=c(2,3))
boxplot(dataset$India,main="India",col="red")
boxplot(dataset$Italy,main="Italy",col="green")
boxplot(dataset$Spain,main="Spain",col="pink")
boxplot(dataset$Thailand,main="Thailand",col="blue")
boxplot(dataset$Iraq,main="Iraq",col="yellow")
boxplot(dataset$Russia,main="Russia",col="skyblue")


x<-ts(dataset$India)
model<-HoltWinters(x,beta = F,gamma = F)
model
plot(model)

x1<-ts(dataset$India)
x2<-ts(dataset$Italy)
x3<-ts(dataset$Norway)
x4<-ts(dataset$Sweden)

model1<-HoltWinters(x1,beta = F,gamma = F)
model2<-HoltWinters(x2,beta = F,gamma = F)
model3<-HoltWinters(x3,beta = F,gamma = F)
model4<-HoltWinters(x4,beta = F,gamma = F)

par(mfrow=c(2,2))
plot(model1,main = "India")
plot(model2,main="Italy")
plot(model3,main = "Norway")
plot(model4,main="Sweden")


x1<-ts(dataset$India)
x2<-ts(dataset$Norway)
x3<-ts(dataset$Sweden)
x4<-ts(dataset$Italy)
x5<-ts(dataset$Brazil)
x6<-ts(dataset$Belgium)
x7<-ts(dataset$Israel)
x8<-ts(dataset$Iraq)
x9<-ts(dataset$Spain)
x10<-ts(dataset$Malaysia)

model1<-HoltWinters(x1,beta = F,gamma = F)
model2<-HoltWinters(x2,beta = F,gamma = F)
model3<-HoltWinters(x3,beta = F,gamma = F)
model4<-HoltWinters(x4,beta = F,gamma = F)
model5<-HoltWinters(x5,beta = F,gamma = F)
model6<-HoltWinters(x6,beta = F,gamma = F)
model7<-HoltWinters(x7,beta = F,gamma = F)
model8<-HoltWinters(x8,beta = F,gamma = F)
model9<-HoltWinters(x9,beta = F,gamma = F)
model10<-HoltWinters(x10,beta = F,gamma = F)

library(forecast)
forecast(model1,4)
forecast(model2,4)
forecast(model3,4)
forecast(model4,4)
forecast(model5,4)
forecast(model6,4)
forecast(model7,4)
forecast(model8,4)
forecast(model9,4)
forecast(model10,4)


par(mfrow=c(3,3))
plot(forecast(model1,4),col = "red",main = "India")
plot(forecast(model2,4),col="green",main = "Norway")
plot(forecast(model3,4),col="blue",main="Sweden")
plot(forecast(model4,4),col="brown",main = "Italy")
plot(forecast(model5,4),col="dark green",main="Brazil")
plot(forecast(model6,4),col = "black",main = "Belgium")
plot(forecast(model7,4),col="cyan",main="Israel")
plot(forecast(model8,4),col = "dark red",main="Iraq")
plot(forecast(model9,4),col="orange",main = "Spain")

par(mfrow=c(2,2))
plot(c(66,67),c(887,987),xlim = c(66,67),ylim = c(100,1500),"b",col="green", ylab="Actual vs Predict",main = "India" )
lines(c(66,67),c(777,797),"b",col="red")
plot(c(66,67),c(3771,4015),xlim = c(66,67),ylim = c(1000,5000),"b",col="green", ylab="Actual vs Predict",main = "Norway" )
lines(c(66,67),c(3544,3616),"b",col="red")
plot(c(66,67),c(86498,92472),xlim = c(66,67),ylim = c(50000,100000),"b",col="green", ylab="Actual vs Predict",main = "Italy" )
lines(c(66,67),c(78192,79769),"b",col="red")
plot(c(66,67),c(3069,3447),xlim = c(66,67),ylim = c(1000,5000),"b",col="green", ylab="Actual vs Predict",main = "Sweden" )
lines(c(66,67),c(2987,3048),"b",col="red" )


library(Metrics)
rmse(c(887,987),c(777,797))          # india
rmse(c(3771,4015),c(3544,3616))      #Norway
rmse(c(3069,3447),c(2987,3048))      #Sweden


model1<-HoltWinters(x1,beta = F,gamma = F)
model1
a=forecast(model1,2)
a
model1<-HoltWinters(x1,beta = F,gamma = F,alpha=0.7)
a=forecast(model1,2)
a
model1<-HoltWinters(x1,beta = F,gamma = F,alpha=1)
a=forecast(model1,2)
a

x1<-ts(dataset$India)
x2<-ts(dataset$Norway)
x3<-ts(dataset$Sweden)
x4<-ts(dataset$Italy)
x5<-ts(dataset$Brazil)
x6<-ts(dataset$Belgium)
x7<-ts(dataset$Israel)
x8<-ts(dataset$Iraq)
x9<-ts(dataset$Spain)
x10<-ts(dataset$Malaysia)

model1<-auto.arima(x1)
model2<-auto.arima(x2)
model3<-auto.arima(x3)
model4<-auto.arima(x4)
model5<-auto.arima(x5)
model6<-auto.arima(x6)
model7<-auto.arima(x7)
model8<-auto.arima(x8)
model9<-auto.arima(x9)
model10<-auto.arima(x10)

model1<-auto.arima(x1)       # India
model1

par(mfrow=c(2,2))
plot(c(66,67),c(887,987),xlim = c(66,67),ylim = c(100,1500),"b",col="green", ylab="Actual vs Predict",main = "India" )
lines(c(66,67),c(860,950),"b",col="red")
plot(c(66,67),c(3771,4015),xlim = c(66,67),ylim = c(1000,5000),"b",col="green", ylab="Actual vs Predict",main = "Norway" )
lines(c(66,67),c(3660,3964),"b",col="red")
plot(c(66,67),c(86498,92472),xlim = c(66,67),ylim = c(50000,100000),"b",col="green", ylab="Actual vs Predict",main = "Italy" )
lines(c(66,67),c(81167,87321),"b",col="red")
plot(c(66,67),c(3069,3447),xlim = c(66,67),ylim = c(1000,5000),"b",col="green", ylab="Actual vs Predict",main = "Sweden" )
lines(c(66,67),c(3176,3495),"b",col="red" )


library(Metrics)
rmse(c(887,987),c(860,950))             # India
rmse(c(3771,4015),c(3660,3964))         #Norway
rmse(c(3069,3447),c(3176,3495))         # Sweden




a<-dataset$India[35:65]
x1<-ts(a)
model1<-auto.arima(x1)
forecast(model1,4)
b<-dataset$Italy[35:65]
x2<-ts(b)
model2<-auto.arima(x2)
forecast(model2,4)



a<-dataset$Italy[35:65]
x1<-ts(a)
model1<-auto.arima(x1)
forecast(model1,4)
model1<-arima(x1,order=c(1,1.3,0))
forecast(model1,4)
model1<-arima(x1,order=c(1,2,0.3))
forecast(model1,4)


x1<-detrend(dataset$India,"constant")
a<-ts(x1)
model1<-auto.arima(a)
forecast(model1,4)

x1<-detrend(dataset$India,"linear")
a<-ts(x1)
model1<-auto.arima(a)
forecast(model1,4)

x1<-dataset$India
a<-ts(x1)
model1<-auto.arima(a)
forecast(model1,4)                                            



x1<-ts(dataset$India[20:65])
x2<-ts(dataset$Italy[20:65])
x3<-ts(dataset$Sweden[20:65])
x4<-ts(dataset$Norway[20:65])
x5<-ts(dataset$Spain[20:65])

model1<-auto.arima(x1)
model2<-auto.arima(x2)
model3<-auto.arima(x3)
model4<-auto.arima(x4)
model5<-auto.arima(x5)

forecast(model1,7)
forecast(model2,7)
forecast(model3,7)
forecast(model4,7)
forecast(model5,7)




