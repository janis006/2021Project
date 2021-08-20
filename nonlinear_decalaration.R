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
declaration<-ts(data.frame(train.nl[,1],train.nl[,5]), frequency = 12, start = c(2018,9))
onflow<-ts(data.frame(train.nl[,1],train.nl[,2]), frequency = 12, start = c(2018,9))

# prepare the whole ts
declaration.ts<-ts(data.frame(NLData[,1],NLData[,5]), frequency = 12, start = c(2018,9))

############################################
# fetch time information
# predict the following 10 month data
dec.t <- time(declaration)

dec.t.break1 <- 2020+(1/12) # Feb 2020 
dec.t.break2 <- 2020.250 # Apr 2020
dec.t.break3 <- 2020+(5/12) # Jun 2020

# this is introducing new variables x2,t and x3,5 in textbook, x1,t is the first fragment
# get exact year number after turning point
dec.tb1 <- ts(pmax(0, dec.t - dec.t.break1), frequency = 12, start = c(2018,9))
dec.tb2 <- ts(pmax(0, dec.t - dec.t.break2), frequency = 12, start = c(2018,9))
dec.tb3 <- ts(pmax(0, dec.t - dec.t.break3), frequency = 12, start = c(2018,9))

dec.fit.pw <- tslm(declaration[,2] ~ dec.t + dec.tb1 + dec.tb2 + dec.tb3)
coef(dec.fit.pw)

# t.new is number of years of forecast
dec.t.new <- time(test.nl.ts)
dec.tb1.new <- dec.t.new-dec.t.break1
dec.tb2.new <- dec.t.new-dec.t.break2
dec.tb3.new <- dec.t.new-dec.t.break3

dec.newdata <- cbind(dec.t = dec.t.new, dec.tb1 = dec.tb1.new, dec.tb2 = dec.tb2.new, dec.tb3 = dec.tb3.new) %>%
  as.data.frame()
dec.fcasts.pw <- forecast(dec.fit.pw, newdata = dec.newdata)

# plot forecast
plot(cbind(declaration.ts[,2], forecast(dec.fit.pw, newdata = dec.newdata)$mean), plot.type = "single",  
     ylim=c(-300000,1600000), ylab = "UC Declaration", xaxt='n', 
     main="Forecasts from the Non-linear Regression Model For the Declaration")
lines(declaration.ts[,2])
lines(fitted(dec.fit.pw), col = "purple")
lines(forecast(dec.fit.pw, newdata = dec.newdata)$mean, type = "l", col = "blue")
lines(forecast(dec.fit.pw, newdata = dec.newdata)$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(dec.fit.pw, newdata = dec.newdata)$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                              "Forecasts", 
                              "95% Confidence bands",
                              "Fitted line"), 
       lty = c(1,1,2,1), 
       col = c("black", "blue", "red", "purple"), bty = "n")

# get AICc value
CV(dec.fit.pw)





