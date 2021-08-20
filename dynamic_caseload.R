library(fpp2)
library(GGally)
library(forecast)
library(readxl)
library(MASS)
library(xts)
library(datetime)

# import data
DynData <- read_excel("ImportData/diffCase.xlsx")
DynData <- na.omit(DynData) 

# transfer to ts object at first
DynData.ts<- ts(data.frame(DynData), frequency = 12, start = c(2018,8))
# take differncing
DynData.sel <- DynData.ts %>% diff()

# split into training/test dataset
train.dy<-head(DynData.sel, 24)
test.dy<-tail(DynData.sel, 6)

# start creating ts object
DynData.sel_ts<-train.dy

# prepare the whole ts
caseload.ts<-DynData.sel[,2]

################################################################################
# dynamic regression model
# one variable xreg: death
(dyn.fit <- auto.arima(DynData.sel_ts[,"caseload"], xreg=DynData.sel_ts[,"death"]))
checkresiduals(dyn.fit)

# forcast
dyn.fcast <- forecast(dyn.fit, xreg=rep(mean(DynData.sel_ts[,"death"]),6))

plot(cbind(caseload.ts, forecast(dyn.fit, xreg=rep(mean(DynData.sel_ts[,"death"]),6))$mean),      
     plot.type = "single", ylim=c(-300000,1250000),
     ylab = "differenced UC Caseload", xaxt='n', main="Forecasts from Regression with ARIMA(0,0,1) errors For the Monthly change of Household Caseload")
lines(caseload.ts)
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
(dyn.fit1 <- auto.arima(DynData.sel_ts[,"caseload"], xreg=DynData.sel_ts[,"GDP"]))
checkresiduals(dyn.fit1)

# forcast
dyn.fcast1 <- forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))

plot(cbind(caseload.ts, forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$mean), 
     plot.type = "single", ylim=c(-200000,1400000),
     ylab = "differenced UC Caseload", xaxt='n', main="Forecasts from Regression with ARIMA(1,0,0) errors For the Monthly change of Household Caseload")
lines(caseload.ts)
lines(forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$mean, type = "l", col = "blue")
lines(forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(dyn.fit1, xreg=rep(mean(DynData.sel_ts[,"GDP"]),6))$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                             "Forecasts", "95% Confidence bands"), lty = c(1,1,2,2), 
       col = c("black", "blue", "red", "red"), bty = "n")

