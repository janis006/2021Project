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
declaration.nl<-ts(data.frame(train.nl[,1],train.nl[,5]), frequency = 12, start = c(2018,9))
declaration.total<-ts(data.frame(NLData[,1],NLData[,5]), frequency = 12, start = c(2018,9))

# fetch time information
# predict the following 6 month data
dec.t <- time(declaration.nl)

dec.t.break1 <- 2020+(1/12) # Feb 2020
dec.t.break2 <- 2020.250 # Apr 2020
dec.t.break3 <- 2020+(5/12) # Jun 2020

# get exact year number after turning point
dec.tb1 <- ts(pmax(0, dec.t - dec.t.break1), frequency = 12, start = c(2018,9))
dec.tb2 <- ts(pmax(0, dec.t - dec.t.break2), frequency = 12, start = c(2018,9))
dec.tb3 <- ts(pmax(0, dec.t - dec.t.break3), frequency = 12, start = c(2018,9))

dec.fit.pw <- tslm(declaration.nl[,2] ~ dec.t + dec.tb1 + dec.tb2 + dec.tb3)
CV(dec.fit.pw)
# t.new is number of years of forcast
dec.t.new <- c(2020+(8/12),2020.750,2020+(10/12),
               2020+(11/12),2021.000,2021+(1/12))
dec.tb1.new <- dec.t.new-dec.t.break1
dec.tb2.new <- dec.t.new-dec.t.break2
dec.tb3.new <- dec.t.new-dec.t.break3

dec.newdata <- cbind(dec.t = dec.t.new, dec.tb1 = dec.tb1.new, dec.tb2 = dec.tb2.new, dec.tb3 = dec.tb3.new) %>%
  as.data.frame()
dec.fcasts.pw <- forecast(dec.fit.pw, newdata = dec.newdata)

# report the accuracy
dec.nl.test <- ts(data.frame(test.nl[,1],test.nl[,5]), frequency = 12, start = c(2020,9))
accuracy(dec.fcasts.pw$mean, dec.nl.test[,2])

################################################################################
## ARIMA model with outlier adjusted
# import data
UCData <- read_excel("ImportData/UCData.xlsx")
UCData <- na.omit(UCData) 

train.adj<-head(UCData, 24)
test.adj<-tail(UCData, 6)

# start creating ts object
declaration.adj<-ts(data.frame(train.adj[,1],train.adj[,5]), frequency = 12, start = c(2018,9))

### considering outlier
declaration.outliers <- tso(declaration.adj[,2],types = c("AO","LS","TC"))
plot(declaration.outliers)

declaration.outliers$outliers

declaration_adj <- declaration.outliers$yadj

Box.test(declaration_adj,lag=10,type="Ljung-Box")
declaration_adj %>% tsdisplay(main="Time plot and ACF and PACF plots for the Adjusted Declaration Data")

dec.fit.adj <- auto.arima(declaration_adj, seasonal = FALSE,stepwise=FALSE,approximation=FALSE )
checkresiduals(dec.fit.adj)

dec.fcasts.adj <- forecast(dec.fit.adj,h=6)

# report the accuracy
dec.adj.test <- ts(data.frame(test.adj[,1],test.adj[,5]), frequency = 12, start = c(2020,9))
accuracy(dec.fcasts.adj$mean, dec.adj.test[,2])

################################################################################
## Dynamic regression model with 1 regressor (Death)
# import data
DynData <- read_excel("ImportData/Dynamic.xlsx")
DynData <- na.omit(DynData) 

DynData.sel <- DynData[c("declaration", "death", "GDP")]
train.dyn<-head(DynData.sel, 24)
test.dyn<-tail(DynData.sel, 6)

# start creating ts object
declaration.dyn<-ts(data.frame(train.dyn), frequency = 12, start = c(2018,9))

(dec.fit.dyn <- auto.arima(declaration.dyn[,"declaration"], xreg=declaration.dyn[,"death"]))
checkresiduals(dec.fit.dyn)

# forcast
dec.fcasts.dyn <- forecast(dec.fit.dyn, xreg=rep(mean(declaration.dyn[,"death"]),6))

# report the accuracy
dec.dyn.test <- ts(data.frame(test.dyn[,1]), frequency = 12, start = c(2020,9))
accuracy(dec.fcasts.dyn$mean, dec.dyn.test)

################################################################################
# plot real data and predicted value together
autoplot(declaration.total[,2]) +
  autolayer(dec.fcasts.dyn, series="Dynamic model with Death regressor", PI=FALSE) +
  autolayer(dec.fcasts.adj, series="Outlier Adjusted ARIMA", PI=FALSE) +
  autolayer(dec.fcasts.pw, series="Non-linear Regression", PI=FALSE) +
  xlab("Time") + ylab("UC Declaration") +
  ggtitle("Forecasts for Declaration") +
  guides(colour=guide_legend(title="Forecast"))
