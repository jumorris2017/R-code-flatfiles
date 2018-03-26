##Barista/Shift turnover (headcount) projections by store and month
##April, May, June (FY18 Q3)
##For Brandon Hull / Sean Yagi

#load libraries
library(data.table)
library(forecast)
library(plm)
library(utils)
set.seed(98115)

#load data (from Megan)
hc <- read.csv("O:/CoOp/CoOp194_PROReportng&OM/Megan/StoreRisk/EverythingEverJan.csv")
setDT(hc)

#reduce variables
# temp <- hc[, c("StoreNum","FP","FY",grep("barista",colnames(hc),value=T),grep("shift",colnames(hc),value=T)), with=F]
temp <- hc[, .(StoreNum,FP,FY,baristaHC,barista.term,barista.trans,shiftHC,shift.term,shift.trans)]
temp <- subset(temp, !duplicated(temp[,.(StoreNum,FP,FY)]))
temp[, fpfy := paste0(FY,".",FP)]

# #set up data as time series
# tempts <- ts(temp, start = c(2013, 1), frequency = 12)
# newdata <- as.data.table(expand.grid(FP = c(6:9), FY = 2018, barista.term = NA,
#             StoreNum = unique(temp[,StoreNum])))
# fit <- tslm(barista.term ~ FP + FY + StoreNum, data=tempts)
# fc <- forecast(fit, h=4, newdata = newdata)

#group by forecast
ptemp <- pdata.frame(temp, index = c("StoreNum", "fpfy"))
fe.full <- plm(barista.term ~ baristaHC + lag(baristaHC,1) + lag(barista.term,1) + FP + FY, data = ptemp, model= "within", effect="twoways")
#predict
fc <- predict(fe.full,h=4)

#fitted values
fitted <- as.numeric(fe.full$model[[1]] - fe.full$residuals) 

#just get last year's for each store
lyhcterm <-  hc[, .(StoreNum,FP,FY,baristaHC,barista.term,barista.trans,shiftHC,shift.term,shift.trans)]
lyhcterm <- lyhcterm[(FY==2017&FP>=6)|FY==2018]
lyhctermq3 <- lyhcterm[(FY==2017&FP>=7&FP<=9)]
lyhctermq3 <- lyhctermq3[, .(StoreNum,FP,barista.term,barista.trans,shiftHC,shift.term,shift.trans)]

#write.csv
# write.csv(lyhctermq3,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/barista-shift_terms_Q3FY18.csv")

##forecasting
##DT1 - agg at topline - predict out 4 periods
##DT2 - take proportion topline terms for each store, and back out from DT1 preds

#barista terms
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
stmonthpred_bte <- left_join(stmonthprp,tempDT1,by=c("FP"))
setDT(stmonthpred_bte)
stmonthpred_bte[, barista.term := round(bar_term_prp*bar_term_agg,0)]

#shift terms
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
stmonthpred_ste <- left_join(stmonthprp,tempDT1,by=c("FP"))
setDT(stmonthpred_ste)
stmonthpred_ste[, shift.term := round(ss_term_prp*ss_term_agg,0)]

#barista transfers
#set up data as time series
tempDT1 <- temp[(FY>=2013&FY<=2017)|(FY==2018&FP<5), list(barista.trans = sum(barista.trans,na.rm=T)), by=c("FP","FY")]
tempDT1ts <- ts(tempDT1, start = c(2013, 1), frequency = 12)
fit <- auto.arima(tempDT1ts[,"barista.trans"])
fc <- forecast(fit, h=5)
predvalues <- fc$mean[1:5]
plot(forecast(fit,h=5),main="Forecasted Topline Barista Trans")
#expand grid and pull back
newdata <- as.data.table(expand.grid(FP = c(5:9), FY = 2018, barista.trans = NA))
newdata[, barista.trans := predvalues]
#rbind predictions to original agged dataset
l = list(tempDT1,newdata)
tempDT1 <- rbindlist(l,use.names=T,fill=T)
setnames(tempDT1,"barista.trans","bar_trans_agg")
#pull monthly agg into store-level dataset
stmonthprp <- left_join(temp,tempDT1,by=c("FP","FY"))
setDT(stmonthprp)
#only needed for last year's months of interest
stmonthprp <- stmonthprp[FY==2017&(FP>=7&FP<=9)]
stmonthprp[barista.trans==0, bar_trans_prp := 0]
stmonthprp[barista.trans>0, bar_trans_prp := barista.trans/bar_trans_agg]
#only keep month and proportions
stmonthprp <- stmonthprp[, .(StoreNum,FP,bar_trans_prp)]
tempDT1 <- tempDT1[FY==2018&(FP>=7&FP<=9)]
stmonthpred_btr <- left_join(stmonthprp,tempDT1,by=c("FP"))
setDT(stmonthpred_btr)
stmonthpred_btr[, barista.trans := round(bar_trans_prp*bar_trans_agg,0)]

#shift transfers
#set up data as time series
tempDT1 <- temp[(FY>=2013&FY<=2017)|(FY==2018&FP<5), list(shift.trans = sum(shift.trans,na.rm=T)), by=c("FP","FY")]
tempDT1ts <- ts(tempDT1, start = c(2013, 1), frequency = 12)
fit <- auto.arima(tempDT1ts[,"shift.trans"])
fc <- forecast(fit, h=5)
predvalues <- fc$mean[1:5]
plot(forecast(fit,h=5),main="Forecasted Topline Shift Trans")
#expand grid and pull back
newdata <- as.data.table(expand.grid(FP = c(5:9), FY = 2018, shift.trans = NA))
newdata[, shift.trans := predvalues]
#rbind predictions to original agged dataset
l = list(tempDT1,newdata)
tempDT1 <- rbindlist(l,use.names=T,fill=T)
setnames(tempDT1,"shift.trans","ss_trans_agg")

#pull monthly agg into store-level dataset
stmonthprp <- left_join(temp,tempDT1,by=c("FP","FY"))
setDT(stmonthprp)
#only needed for last year's months of interest
stmonthprp <- stmonthprp[FY==2017&(FP>=7&FP<=9)]
stmonthprp[shift.trans==0, ss_trans_prp := 0]
stmonthprp[shift.trans>0, ss_trans_prp := shift.trans/ss_trans_agg]
#only keep month and proportions
stmonthprp <- stmonthprp[, .(StoreNum,FP,ss_trans_prp)]
tempDT1 <- tempDT1[FY==2018&(FP>=7&FP<=9)]
stmonthpred_str <- left_join(stmonthprp,tempDT1,by=c("FP"))
setDT(stmonthpred_str)
stmonthpred_str[, shift.trans := round(ss_trans_prp*ss_trans_agg,0)]

#cbind
full <- Reduce(function(x, y) {merge(x, y, by=c("StoreNum","FP","FY"), all = TRUE)}, list(stmonthpred_bte,stmonthpred_ste,stmonthpred_btr,stmonthpred_str))
full <- full[, .(StoreNum,FP,FY,barista.term,barista.trans,shift.term,shift.trans)]

#write.csv
write.csv(full,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/barista-shift_terms_trans_Q3FY18.csv")
