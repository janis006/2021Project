library(fpp2)
library(GGally)
library(forecast)
library(readxl)
library(MASS)
library(xts)
library(datetime)
library(tsoutliers)

# import data
UCData <- read_excel("ImportData/diffCase.xlsx")
UCData <- na.omit(UCData) 

# transfer to ts object at first
UCData.ts<- ts(data.frame(UCData[,-1]), frequency = 12, start = c(2018,8))
# take differncing
diffdata <- UCData.ts %>% diff()

# split into training/test dataset
train.ar<-head(diffdata, 24)
test.ar<-tail(diffdata, 6)

# start creating ts object
caseload<-train.ar

# prepare the whole ts
caseload.ts<-diffdata[,1]

#####################################################################################################

### forecast for caseload
Box.test(caseload[,1],lag=10,type="Ljung-Box")
caseload[,1] %>% tsdisplay(main="Time plot and ACF and PACF plots for the Monthly change of Household Caseload")

# just to check AICc value
fit1 <- Arima(caseload[,1], order=c(1,0,0))
summary(fit1)
fit2 <- Arima(caseload[,1], order=c(0,0,1))
summary(fit2)

fit3 <- Arima(caseload[,1], order=c(1,0,1))
summary(fit3)
fit4 <- Arima(caseload[,1], order=c(2,0,0))
summary(fit4)
fit5 <- Arima(caseload[,1], order=c(0,0,2))
summary(fit5)
fit6 <- Arima(caseload[,1], order=c(0,0,0))
summary(fit6)

fit <- Arima(caseload[,1], order=c(0,0,1))
checkresiduals(fit)

plot(cbind(caseload.ts, forecast(fit,h=6)$mean), plot.type = "single", ylim=c(-300000,1100000),
     ylab = "UC Monthly change of Household Caseload", xaxt='n', main="Forecasts from the ARIMA(0,0,1) For the Monthly change of Household Caseload")
lines(caseload.ts)
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
caseload.outliers <- tso(caseload[,1],types = c("AO","LS","TC"))
plot(caseload.outliers)

caseload.outliers$outliers

caseload_adj <- caseload.outliers$yadj

Box.test(caseload_adj,lag=10,type="Ljung-Box")



