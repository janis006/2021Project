library(fpp2)
library(GGally)
library(forecast)
library(readxl)
library(MASS)
library(xts)
library(datetime)
library(tsoutliers)

################################################################################
## NON-LINEAR regression model
# import data
NLData <- read_excel("ImportData/diffCase.xlsx")
NLData <- na.omit(NLData) 

# transfer to ts object at first
NLData.ts<- ts(data.frame(NLData[,-1]), frequency = 12, start = c(2018,8))
# take differncing
diffdata <- NLData.ts %>% diff()

# split into training/test dataset
train.nl<-head(diffdata, 24)
test.nl<-tail(diffdata, 6)

# start creating ts object
caseload<-train.nl

# prepare the whole ts
caseload.total<-diffdata[,1]

############################################
# fetch time information
# predict the following 10 month data
cas.t <- time(caseload)

cas.t.break1 <- 2020+(2/12) # Mar 2020
cas.t.break2 <- 2020.250 # Apr 2020
cas.t.break3 <- 2020+(5/12) # Jun 2020

# get exact year number after turning point
cas.tb1 <- ts(pmax(0, cas.t - cas.t.break1), frequency = 12, start = c(2018,9))
cas.tb2 <- ts(pmax(0, cas.t - cas.t.break2), frequency = 12, start = c(2018,9))
cas.tb3 <- ts(pmax(0, cas.t - cas.t.break3), frequency = 12, start = c(2018,9))

cas.fit.pw <- tslm(caseload[,1] ~ cas.t + cas.tb1 + cas.tb2 + cas.tb3)
coef(cas.fit.pw)

# t.new is number of years of forecast
cas.t.new <- time(test.nl)
cas.tb1.new <- cas.t.new-cas.t.break1
cas.tb2.new <- cas.t.new-cas.t.break2
cas.tb3.new <- cas.t.new-cas.t.break3

cas.newdata <- cbind(cas.t = cas.t.new, cas.tb1 = cas.tb1.new, cas.tb2 = cas.tb2.new, cas.tb3 = cas.tb3.new) %>%
  as.data.frame()
cas.fcasts.pw <- forecast(cas.fit.pw, newdata = cas.newdata)

# report the accuracy
cas.nl.test <- ts(data.frame(test.nl[,1]), frequency = 12, start = c(2020,9))
accuracy(cas.fcasts.pw$mean, cas.nl.test[,1])

################################################################################
## Dynamic regression model with 1 regressor (Death)
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

# one variable xreg: death
(cas.fit.dyn <- auto.arima(train.dy[,"caseload"], xreg=train.dy[,"death"]))
checkresiduals(cas.fit.dyn)

# forcast
cas.fcasts.dyn <- forecast(cas.fit.dyn, xreg=rep(mean(train.dy[,"death"]),6))

# report the accuracy
cas.dyn.test <- ts(data.frame(test.dy[,2]), frequency = 12, start = c(2020,9))
accuracy(cas.fcasts.dyn$mean, cas.dyn.test)

################################################################################
## Dynamic regression model with 1 regressor (GDP)
# one variable xreg: GDP
(cas.fit1.dyn <- auto.arima(train.dy[,"caseload"], xreg=train.dy[,"GDP"]))
checkresiduals(cas.fit1.dyn)

# forcast
cas.fcasts1.dyn <- forecast(cas.fit1.dyn, xreg=rep(mean(train.dy[,"GDP"]),6))

# report the accuracy
accuracy(cas.fcasts1.dyn$mean, cas.dyn.test)

################################################################################
# plot real data and predicted value together
autoplot(caseload.total) +
  autolayer(cas.fcasts.dyn, series="Dynamic model with Death regressor", PI=FALSE) +
  autolayer(cas.fcasts1.dyn, series="Dynamic model with GDP regressor", PI=FALSE) +
  autolayer(cas.fcasts.pw, series="Non-linear Regression", PI=FALSE) +
  xlab("Time") + ylab("UC differenced Caseload") +
  ggtitle("Forecasts for monthly change of Household Caseload") +
  guides(colour=guide_legend(title="Forecast"))

