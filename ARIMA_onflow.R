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
onflow<-ts(data.frame(train.ar[,1],train.ar[,2]), frequency = 12, start = c(2018,9))

# prepare the whole ts
onflow.ts<-ts(data.frame(UCData[,1],UCData[,2]), frequency = 12, start = c(2018,9))

#####################################################################################################
### forecast for declaration
Box.test(onflow[,2],lag=10,type="Ljung-Box")
onflow[,2] %>% tsdisplay(main="Time plot and ACF and PACF plots for the Onflow Data")

# just to check AICc value
fit1 <- Arima(onflow[,2], order=c(1,0,0))
summary(fit1)
fit2 <- Arima(onflow[,2], order=c(0,0,1))
summary(fit2)

fit3 <- Arima(onflow[,2], order=c(1,0,1))
summary(fit3)
fit4 <- Arima(onflow[,2], order=c(2,0,0))
summary(fit4)
fit5 <- Arima(onflow[,2], order=c(0,0,2))
summary(fit5)
fit6 <- Arima(onflow[,2], order=c(0,0,0))
summary(fit6)

fit <- Arima(onflow[,2], order=c(0,0,1))
checkresiduals(fit)

plot(cbind(onflow.ts[,2], forecast(fit,h=6)$mean), plot.type = "single", ylim=c(-300000,1400000),
     ylab = "UC Onflow", xaxt='n', main="Forecasts from the ARIMA(0,0,1) For the Onflow")
lines(onflow.ts[,2])
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
onflow.outliers <- tso(onflow[,2],types = c("AO","LS","TC"))
plot(onflow.outliers)

onflow.outliers$outliers

onflow_adj <- onflow.outliers$yadj

Box.test(onflow_adj,lag=10,type="Ljung-Box")
onflow_adj %>% tsdisplay(main="Time plot and ACF and PACF plots for the Adjusted Onflow Data")

# just to check AICc value
fit1 <- Arima(onflow_adj, order=c(0,0,0))
summary(fit1)
fit2 <- Arima(onflow_adj, order=c(1,0,0))
summary(fit2)
fit3 <- Arima(onflow_adj, order=c(0,0,1))
summary(fit3)

adj.onflowfit <- Arima(onflow_adj, order=c(0,0,0))
checkresiduals(adj.onflowfit)

# prepare for ts plot
plot(cbind(onflow.ts[,2], onflow_adj, forecast(adj.onflowfit,h=6)$mean), plot.type = "single", ylim=c(0,1400000), 
     ylab = "Adjusted UC Declaration", xaxt='n', main="Forecasts from ARIMA(0,0,0) For Adjusted Onflow")
lines(onflow.ts[,2])
lines(onflow_adj, col = "purple")
lines(forecast(adj.onflowfit,h=6)$mean, type = "l", col = "blue")
lines(forecast(adj.onflowfit,h=6)$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(adj.onflowfit,h=6)$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                              "Forecasts", "95% Confidence bands", "Outlier Adjusted"), lty = c(1,1,2,1), 
       col = c("black", "blue", "red", "purple"), bty = "n")




