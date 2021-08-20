library(fpp2)
library(GGally)
library(forecast)
library(readxl)
library(MASS)
library(xts)
library(datetime)

# import data
NLData <- read_excel("ImportData/Nonlinear.xlsx")
NLData <- na.omit(NLData) 

# split into training/test dataset
train.nl<-head(NLData, 24)
test.nl<-tail(NLData, 6)

# switch test dataset into ts object
test.nl.ts <- ts(data.frame(test.nl[,1],test.nl[,2],test.nl[,4],test.nl[,5]), frequency = 12, start = c(2020,9))

# start creating ts object
onflow<-ts(data.frame(train.nl[,1],train.nl[,2]), frequency = 12, start = c(2018,9))

# prepare the whole ts
onflow.ts<-ts(data.frame(NLData[,1],NLData[,2]), frequency = 12, start = c(2018,9))

############################################
# fetch time information
# predict the following 10 month data
onf.t <- time(onflow)

onf.t.break1 <- 2020+(2/12) # Mar 2020
onf.t.break2 <- 2020.250 # Apr 2020
onf.t.break3 <- 2020+(6/12) # Jul 2020

# get exact year number after turning point
onf.tb1 <- ts(pmax(0, onf.t - onf.t.break1), frequency = 12, start = c(2018,9))
onf.tb2 <- ts(pmax(0, onf.t - onf.t.break2), frequency = 12, start = c(2018,9))
onf.tb3 <- ts(pmax(0, onf.t - onf.t.break3), frequency = 12, start = c(2018,9))

onf.fit.pw <- tslm(onflow[,2] ~ onf.t + onf.tb1 + onf.tb2 + onf.tb3)
coef(onf.fit.pw)

# t.new is number of years of forecast
onf.t.new <- time(test.nl.ts)
onf.tb1.new <- onf.t.new-onf.t.break1
onf.tb2.new <- onf.t.new-onf.t.break2
onf.tb3.new <- onf.t.new-onf.t.break3

onf.newdata <- cbind(onf.t = onf.t.new, onf.tb1 = onf.tb1.new, onf.tb2 = onf.tb2.new, onf.tb3 = onf.tb3.new) %>%
  as.data.frame()
onf.fcasts.pw <- forecast(onf.fit.pw, newdata = onf.newdata)

# plot forecast
plot(cbind(onflow.ts[,2], forecast(onf.fit.pw, newdata = onf.newdata)$mean), plot.type = "single",  
     ylim=c(-1300000,1900000), ylab = "UC Onflow", xaxt='n', 
     main="Forecasts from the Non-linear Regression Model For the Onflow")
lines(onflow.ts[,2])
lines(fitted(onf.fit.pw), col = "purple")
lines(forecast(onf.fit.pw, newdata = onf.newdata)$mean, type = "l", col = "blue")
lines(forecast(onf.fit.pw, newdata = onf.newdata)$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(onf.fit.pw, newdata = onf.newdata)$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                             "Forecasts", 
                             "95% Confidence bands",
                             "Fitted line"), 
       lty = c(1,1,2,1), 
       col = c("black", "blue", "red", "purple"), bty = "n")

# get AICc value
CV(onf.fit.pw)
