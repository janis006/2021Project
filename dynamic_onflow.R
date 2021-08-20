library(fpp2)
library(GGally)
library(forecast)
library(readxl)
library(MASS)
library(xts)
library(datetime)

# import data
DynData <- read_excel("ImportData/Dynamic.xlsx")
DynData <- na.omit(DynData) 

DynData.sel <- DynData[c("onflow", "death", "GDP")]

# split into training/test dataset
train.dy<-head(DynData.sel, 24)
test.dy<-tail(DynData.sel, 6)

# start creating ts object
DynData.sel_ts<-ts(data.frame(train.dy), frequency = 12, start = c(2018,9))

# prepare the whole ts
onflow.ts<-ts(data.frame(DynData.sel[,1]), frequency = 12, start = c(2018,9))

################################################################################
# dynamic regression model
# one variable xreg: death
(dyn.fit <- auto.arima(DynData.sel_ts[,"onflow"], xreg=DynData.sel_ts[,"death"]))
checkresiduals(dyn.fit)

# forcast
dyn.fcast <- forecast(dyn.fit, xreg=rep(mean(DynData.sel_ts[,"death"]),6))

plot(cbind(onflow.ts, forecast(dyn.fit, xreg=rep(mean(DynData.sel_ts[,"death"]),6))$mean),      
     plot.type = "single", ylim=c(-100000,1250000),
     ylab = "UC Onflow", xaxt='n', main="Forecasts from Regression with ARIMA(0,0,0) errors For the Onflow")
lines(onflow.ts)
lines(forecast(dyn.fit, xreg=rep(mean(DynData.sel_ts[,"death"]),6))$mean, type = "l", col = "blue")
lines(forecast(dyn.fit, xreg=rep(mean(DynData.sel_ts[,"death"]),6))$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(dyn.fit, xreg=rep(mean(DynData.sel_ts[,"death"]),6))$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                             "Forecasts", "95% Confidence bands"), lty = c(1,1,2,2), 
       col = c("black", "blue", "red", "red"), bty = "n")

#############################
# one variable xreg: GDP
(dyn.fit1 <- auto.arima(DynData.sel_ts[,"onflow"], xreg=DynData.sel_ts[,"GDP"]))
checkresiduals(dyn.fit1)

# forcast
plot(cbind(onflow.ts, forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$mean), 
     plot.type = "single", ylim=c(-200000,1400000),
     ylab = "UC Declaration", xaxt='n', main="Forecasts from Regression with ARIMA(0,0,0) errors For Onflow")
lines(onflow.ts)
lines(forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$mean, type = "l", col = "blue")
lines(forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                             "Forecasts", "95% Confidence bands"), lty = c(1,1,2,2), 
       col = c("black", "blue", "red", "red"), bty = "n")


