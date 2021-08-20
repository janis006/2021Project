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
NLData <- read_excel("ImportData/Nonlinear.xlsx")
NLData <- na.omit(NLData) 

train.nl<-head(NLData, 24)
test.nl<-tail(NLData, 6)

# start creating ts object
onflow.nl<-ts(data.frame(train.nl[,1],train.nl[,2]), frequency = 12, start = c(2018,9))
onflow.total<-ts(data.frame(NLData[,1],NLData[,2]), frequency = 12, start = c(2018,9))

# fetch time information
# predict the following 6 month data
onf.t <- time(onflow.nl)

onf.t.break1 <- 2020+(2/12) # Mar 2020
onf.t.break2 <- 2020.250 # Apr 2020
onf.t.break3 <- 2020+(6/12) # Jul 2020

# get exact year number after turning point
onf.tb1 <- ts(pmax(0, onf.t - onf.t.break1), frequency = 12, start = c(2018,9))
onf.tb2 <- ts(pmax(0, onf.t - onf.t.break2), frequency = 12, start = c(2018,9))
onf.tb3 <- ts(pmax(0, onf.t - onf.t.break3), frequency = 12, start = c(2018,9))

onf.fit.pw <- tslm(onflow.nl[,2] ~ onf.t + onf.tb1 + onf.tb2 + onf.tb3)
CV(onf.fit.pw)

# t.new is number of years of forcast
onf.t.new <- c(2020+(8/12),2020.750,2020+(10/12),
               2020+(11/12),2021.000,2021+(1/12))
onf.tb1.new <- onf.t.new-onf.t.break1
onf.tb2.new <- onf.t.new-onf.t.break2
onf.tb3.new <- onf.t.new-onf.t.break3

onf.newdata <- cbind(onf.t = onf.t.new, onf.tb1 = onf.tb1.new, onf.tb2 = onf.tb2.new, onf.tb3 = onf.tb3.new) %>%
  as.data.frame()
onf.fcasts.pw <- forecast(onf.fit.pw, newdata = onf.newdata)

# report the accuracy
onf.nl.test <- ts(data.frame(test.nl[,1],test.nl[,2]), frequency = 12, start = c(2020,9))
accuracy(onf.fcasts.pw$mean, onf.nl.test[,2])

################################################################################
## ARIMA model with outlier adjusted
# import data
UCData <- read_excel("ImportData/UCData.xlsx")
UCData <- na.omit(UCData) 

train.adj<-head(UCData, 24)
test.adj<-tail(UCData, 6)

# start creating ts object
onflow.adj<-ts(data.frame(train.adj[,1],train.adj[,2]), frequency = 12, start = c(2018,9))

### considering outlier
onflow.outliers <- tso(onflow.adj[,2],types = c("AO","LS","TC"))
plot(onflow.outliers)

onflow.outliers$outliers

onflow_adj <- onflow.outliers$yadj

Box.test(onflow_adj,lag=10,type="Ljung-Box")
onflow_adj %>% tsdisplay(main="Time plot and ACF and PACF plots for the Adjusted Onflow Data")

onf.fit.adj <- auto.arima(onflow_adj, seasonal = FALSE,stepwise=FALSE,approximation=FALSE )
checkresiduals(onf.fit.adj)
onf.fcasts.adj <- forecast(onf.fit.adj,h=6)

# report the accuracy
onf.adj.test <- ts(data.frame(test.adj[,1],test.adj[,2]), frequency = 12, start = c(2020,9))
accuracy(onf.fcasts.adj$mean, onf.adj.test[,2])

################################################################################
## Dynamic regression model with 1 regressor (Death)
# import data
DynData <- read_excel("ImportData/Dynamic.xlsx")
DynData <- na.omit(DynData) 

DynData.sel <- DynData[c("onflow", "death", "GDP")]
train.dyn<-head(DynData.sel, 24)
test.dyn<-tail(DynData.sel, 6)

# start creating ts object
onflow.dyn<-ts(data.frame(train.dyn), frequency = 12, start = c(2018,9))

# one variable xreg: Death
(onf.fit.dyn <- auto.arima(onflow.dyn[,"onflow"], xreg=onflow.dyn[,"death"]))
checkresiduals(onf.fit.dyn)

# forcast
onf.fcasts.dyn <- forecast(onf.fit.dyn, xreg=rep(mean(onflow.dyn[,"death"]),6))

# report the accuracy
onf.dyn.test <- ts(data.frame(test.dyn[,1]), frequency = 12, start = c(2020,9))
accuracy(onf.fcasts.dyn$mean, onf.dyn.test)

################################################################################
# plot real data and predicted value together
autoplot(onflow.total[,2]) +
  autolayer(onf.fcasts.dyn, series="Dynamic model with Death regressor", PI=FALSE) +
  autolayer(onf.fcasts.adj, series="Outlier Adjusted ARIMA", PI=FALSE) +
  autolayer(onf.fcasts.pw, series="Non-linear Regression", PI=FALSE) +
  xlab("Time") + ylab("UC Onflow") +
  ggtitle("Forecasts for Onflow") +
  guides(colour=guide_legend(title="Forecast"))
