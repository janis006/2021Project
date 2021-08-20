library(fpp2)
library(GGally)
library(forecast)
library(readxl)
library(MASS)
library(xts)
library(datetime)
library(tsoutliers)

# import data
UCData <- read_excel("ImportData/UCData.xlsx")
UCData <- na.omit(UCData) 

# split into training/test dataset
train.ar<-head(UCData, 24)
test.ar<-tail(UCData, 6)

# start creating ts object
declaration<-ts(data.frame(train.ar[,1],train.ar[,5]), frequency = 12, start = c(2018,9))

# prepare the whole ts
declaration.ts<-ts(data.frame(UCData[,1],UCData[,5]), frequency = 12, start = c(2018,9))

#####################################################################################################
### forecast for declaration
Acf(declaration[,2], main = "ACF for Declaration")
Box.test(declaration[,2],lag=10,type="Ljung-Box")
declaration[,2] %>% tsdisplay(main="Time plot and ACF and PACF plots for the Declaration Data")

# check AICc value
fit1 <- Arima(declaration[,2], order=c(1,0,0))
summary(fit1)
fit2 <- Arima(declaration[,2], order=c(0,0,1))
summary(fit2)

fit3 <- Arima(declaration[,2], order=c(1,0,1))
summary(fit3)
fit4 <- Arima(declaration[,2], order=c(2,0,0))
summary(fit4)
fit5 <- Arima(declaration[,2], order=c(0,0,2))
summary(fit5)
fit6 <- Arima(declaration[,2], order=c(0,0,0))
summary(fit6)

fit <- Arima(declaration[,2], order=c(0,0,1))
checkresiduals(fit)

plot(cbind(declaration.ts[,2], forecast(fit,h=6)$mean), plot.type = "single", ylim=c(-250000,1400000),
     ylab = "UC Declaration", xaxt='n', main="Forecasts from the ARIMA(0,0,1) For the Declaration")
lines(declaration.ts[,2])
lines(forecast(fit,h=6)$mean, type = "l", col = "blue")
lines(forecast(fit,h=6)$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(fit,h=6)$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                              "Forecasts", "95% Confidence bands"), lty = c(1,1,2,2), 
       col = c("black", "blue", "red", "red"), bty = "n")

#########################################
### considering outlier ### 
declaration.outliers <- tso(declaration[,2],types = c("AO","LS","TC"))
plot(declaration.outliers)

declaration.outliers$outliers

declaration_adj <- declaration.outliers$yadj

Box.test(declaration_adj,lag=10,type="Ljung-Box")
declaration_adj %>% tsdisplay(main="Time plot and ACF and PACF plots for the Adjusted Declaration Data")

# check AICc value
fit1 <- Arima(declaration_adj, order=c(0,0,0))
summary(fit1)
fit2 <- Arima(declaration_adj, order=c(1,0,0))
summary(fit2)
fit3 <- Arima(declaration_adj, order=c(0,0,1))
summary(fit3)

adj.declarationfit <- Arima(declaration_adj, order=c(0,0,0))
checkresiduals(adj.declarationfit)

# prepare for ts plot
plot(cbind(declaration.ts[,2], declaration_adj, forecast(adj.declarationfit,h=6)$mean), plot.type = "single", ylim=c(0,1400000), 
     ylab = "Adjusted UC Declaration", xaxt='n', main="Forecasts from ARIMA(0,0,0) For Adjusted Declaration")
lines(declaration.ts[,2])
lines(declaration_adj, col = "purple")
lines(forecast(adj.declarationfit,h=6)$mean, type = "l", col = "blue")
lines(forecast(adj.declarationfit,h=6)$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(adj.declarationfit,h=6)$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topright", legend = c("Observed data", 
                              "Forecasts", "95% Confidence bands", "Outlier Adjusted"), lty = c(1,1,2,1), 
       col = c("black", "blue", "red", "purple"), bty = "n")







