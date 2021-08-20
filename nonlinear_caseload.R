library(fpp2)
library(GGally)
library(forecast)
library(readxl)
library(MASS)
library(xts)
library(datetime)

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
caseload.ts<-diffdata[,1]

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

# plot forecast
plot(cbind(caseload.ts, forecast(cas.fcasts.pw, newdata = cas.newdata)$mean), plot.type = "single",  
     ylim=c(-100000,1200000), ylab = "UC Monthly change of Household Caseload", xaxt='n', 
     main="Forecasts from the Non-linear Regression Model For the Monthly change of Household Caseload")
lines(caseload.ts)
lines(fitted(cas.fcasts.pw), col = "purple")
lines(forecast(cas.fcasts.pw, newdata = cas.newdata)$mean, type = "l", col = "blue")
lines(forecast(cas.fcasts.pw, newdata = cas.newdata)$lower[,2], type = "l", col = "red", lty = 2)  
lines(forecast(cas.fcasts.pw, newdata = cas.newdata)$upper[,2], type = "l", col = "red", lty = 2)
axis(side=1,at=c(2019.0,2019.5,2020.0,2020.5,2021.0), 
     labels=c("Jan2019","Jul2019","Jan2020","Jul2020","Jan2021"))
legend("topleft", legend = c("Observed data", 
                             "Forecasts", 
                             "95% Confidence bands",
                             "Fitted line"), 
       lty = c(1,1,2,1), 
       col = c("black", "blue", "red", "purple"), bty = "n")

# get AICc value
CV(cas.fit.pw)

