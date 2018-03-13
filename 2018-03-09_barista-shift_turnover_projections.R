##Barista/Shift turnover (headcount) projections by store and month
##April, May, June (FY18 Q3)
##For Brandon Hull / Sean Yagi

#load libraries
library(data.table)
library(forecast)
library(plm)
library(utils)
set.seed(98115)

#LOAD DATA HERE
#temp <- fread()

##forecasting
##DT1 - agg at topline - predict out 4 periods
##DT2 - take proportion topline terms for each store, and back out from DT1 preds

#barista
#set up data as time series
tempDT1 <- temp[(FY>=2013&FY<=2017)|(FY==2018&FP<5), list(barista.term = sum(barista.term,na.rm=T)), by=c("FP","FY")]
tempDT1ts <- ts(tempDT1, start = c(2013, 1), frequency = 12)
fit <- auto.arima(tempDT1ts[,"barista.term"])
fc <- forecast(fit, h=5)
predvalues <- fc$mean[1:5]
plot(forecast(fit,h=5),main="Forecasted Topline Barista Terms")
#expand grid and pull back
newdata <- as.data.table(expand.grid(FP = c(5:9), FY = 2018, barista.term = NA))
newdata[, barista.term := predvalues]
#rbind predictions to original agged dataset
l = list(tempDT1,newdata)
tempDT1 <- rbindlist(l,use.names=T,fill=T)
setnames(tempDT1,"barista.term","bar_term_agg")

#pull monthly agg into store-level dataset
stmonthprp <- left_join(temp,tempDT1,by=c("FP","FY"))
setDT(stmonthprp)
#only needed for last year's months of interest
stmonthprp <- stmonthprp[FY==2017&(FP>=7&FP<=9)]
stmonthprp[barista.term==0, bar_term_prp := 0]
stmonthprp[barista.term>0, bar_term_prp := barista.term/bar_term_agg]
#only keep month and proportions
stmonthprp <- stmonthprp[, .(StoreNum,FP,bar_term_prp)]
tempDT1 <- tempDT1[FY==2018&(FP>=7&FP<=9)]
stmonthpred <- left_join(stmonthprp,tempDT1,by=c("FP"))
setDT(stmonthpred)
stmonthpred[, barista.term := round(bar_term_prp*bar_term_agg,0)]

#shift
#set up data as time series
tempDT1 <- temp[(FY>=2013&FY<=2017)|(FY==2018&FP<5), list(shift.term = sum(shift.term,na.rm=T)), by=c("FP","FY")]
tempDT1ts <- ts(tempDT1, start = c(2013, 1), frequency = 12)
fit <- auto.arima(tempDT1ts[,"shift.term"])
fc <- forecast(fit, h=5)
predvalues <- fc$mean[1:5]
plot(forecast(fit,h=5),main="Forecasted Topline Shift Terms")
#expand grid and pull back
newdata <- as.data.table(expand.grid(FP = c(5:9), FY = 2018, shift.term = NA))
newdata[, shift.term := predvalues]
#rbind predictions to original agged dataset
l = list(tempDT1,newdata)
tempDT1 <- rbindlist(l,use.names=T,fill=T)
setnames(tempDT1,"shift.term","ss_term_agg")

#pull monthly agg into store-level dataset
stmonthprp <- left_join(temp,tempDT1,by=c("FP","FY"))
setDT(stmonthprp)
#only needed for last year's months of interest
stmonthprp <- stmonthprp[FY==2017&(FP>=7&FP<=9)]
stmonthprp[shift.term==0, ss_term_prp := 0]
stmonthprp[shift.term>0, ss_term_prp := shift.term/ss_term_agg]
#only keep month and proportions
stmonthprp <- stmonthprp[, .(StoreNum,FP,ss_term_prp)]
tempDT1 <- tempDT1[FY==2018&(FP>=7&FP<=9)]
stmonthpred <- left_join(stmonthprp,tempDT1,by=c("FP"))
setDT(stmonthpred)
stmonthpred[, shift.term := round(ss_term_prp*ss_term_agg,0)]
