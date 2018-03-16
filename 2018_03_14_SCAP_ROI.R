#managing data for SCAP

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

#set folder directory
setwd("C:/Users/jumorris/Desktop/SCAP_PTF/")
file_list <- list.files("C:/Users/jumorris/Desktop/SCAP_PTF/")
file_list <- grep(".csv",file_list,value=T)

#load and merge
DT <- setNames(do.call(rbind, Map('cbind', 
                                 lapply(file_list, read.table, header = T, sep=","), V3=file_list)), 
               c("PARTNERID","SCAPSTATUS","PTFDATE"))
setDT(DT)
DT <- DT[SCAPSTATUS=="Active-Participant"|SCAPSTATUS=="ActiveParticipant"]
DT[SCAPSTATUS=="Active-Participant", SCAPSTATUS := "ActiveParticipant"]

#remove file name components
DT[, ptf := gsub("PartnerFile", "", DT[, PTFDATE])]
DT[, ptf := gsub(".csv", "", DT[, ptf])]

#reduce to only partner id and date
activedt <- DT[, .(PARTNERID, ptf)]
#order by ptf
activedt <- setorder(activedt,ptf)

#create dataset of binary indicators where partners were active participants
activedt <- activedt %>%
  mutate(yesno = 1) %>%
  distinct %>%
  spread(ptf, yesno, fill = 0)
setDT(activedt)

#grab start date
activedt[, start_date := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = colnames(activedt)[2:ncol(activedt)]]
activedt[, periodN := rowSums(.SD,na.rm=T), .SDcols=colnames(activedt)[2:74]]

#write.csv
# write.csv(activedt, file="C:/Users/jumorris/Desktop/SCAP_PTF/compiledDT/scap_participants.csv")

#read it back in!
scap <- fread("C:/Users/jumorris/Desktop/SCAP_PTF/compiledDT/scap_participants.csv")
#set names to match benefits data
setnames(scap,"PARTNERID","PRTNR_ID")
#pull year from start_date variable
scap[, start_date_scap := substr(start_date,1,4)]
#keep just partner number and scap year
scap <- scap[, .(PRTNR_ID,start_date_scap)]
#swing wide
#create dataset of binary indicators where partners were active in scap
scap <- scap %>%
  mutate(yesno = 1) %>%
  distinct %>%
  spread(start_date_scap, yesno, fill = 0)
setDT(scap)
colnames(scap)[2:ncol(scap)] <- paste0("scap",colnames(scap)[2:ncol(scap)])

#load benefits data
#only includes januaries.... 
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
ben <- fread(paste0(data_dir,"/benefits_participation.csv"))
#order by FP_End
ben <- setorder(ben,FP_End)
ben[, fpend := mdy(FP_End)]
ben[, fpyear := year(fpend)]

#just keep benefits participation variables

#create dataset of binary indicators where partners were active participants
bsipdt <- dcast.data.table(ben, PRTNR_ID ~ fpyear, value.var="SIP_Part", fun.aggregate=max)
colnames(bsipdt)[2:ncol(bsipdt)] <- paste0("sip",colnames(bsipdt)[2:ncol(bsipdt)])

bbftdt <- dcast.data.table(ben, PRTNR_ID ~ fpyear, value.var="BFT_Part", fun.aggregate=max)
colnames(bbftdt)[2:ncol(bbftdt)] <- paste0("bft",colnames(bbftdt)[2:ncol(bbftdt)])

bretdt <- dcast.data.table(ben, PRTNR_ID ~ fpyear, value.var="RET_Part", fun.aggregate=max)
colnames(bretdt)[2:ncol(bretdt)] <- paste0("ret",colnames(bretdt)[2:ncol(bretdt)])

#merge together
bendt <- Reduce(function(x, y) {merge(x, y, by=c("PRTNR_ID"), all=T)}, list(bsipdt,bbftdt,bretdt))

#grab start date & remove name character components
bendt[, start_date_sip := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = grep("sip",colnames(bendt),value=T)]
bendt[, start_date_sip := gsub("sip", "", bendt[, start_date_sip])]
bendt[sip2015==0&sip2016==0&sip2017==0&sip2018==0, start_date_sip := NA]
bendt[, start_date_bft := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = grep("bft",colnames(bendt),value=T)]
bendt[, start_date_bft := gsub("bft", "", bendt[, start_date_bft])]
bendt[bft2015==0&bft2016==0&bft2017==0&bft2018==0, start_date_bft := NA]
bendt[, start_date_ret := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = grep("ret",colnames(bendt),value=T)]
bendt[, start_date_ret := gsub("ret", "", bendt[, start_date_ret])]
bendt[ret2015==0&ret2016==0&ret2017==0&ret2018==0, start_date_ret := NA]

#load turnover data
prtnr <- fread(paste0(data_dir,"/SCAP_analysis_partner_file.csv"))
#set names to match benefits data
prtnr[, (grep("DT",colnames(prtnr),value=T)) := lapply(.SD, function(x) lubridate::ymd_hms(x)), .SDcols=grep("DT",colnames(prtnr),value=T)]
prtnr[, MOSTREC := NULL]

#join scap data
fulldt <- Reduce(function(x,y){merge(x,y,by="PRTNR_ID",all=T)}, list(bendt,scap,prtnr))

#make separation date NA if still active (happens for multiple-hires)
fulldt[EMP_STAT_CD=="Active", SEPARATION_DT := NA]

#create indicators for whether still an active partner at end of year
#2015
fulldt[MOST_RECENT_HIRE_DT<='2015-12-31', active2015end := 0] #put 0's for relevant partners
fulldt[MOST_RECENT_HIRE_DT<='2015-12-31'&EMP_STAT_CD=="Active", active2015end := 1]
fulldt[MOST_RECENT_HIRE_DT<='2015-12-31'&EMP_STAT_CD=="Separated"&
         SEPARATION_DT>='2016-01-01', active2015end := 1]
fulldt[MOST_RECENT_HIRE_DT<='2015-12-31', sepduring2015 := 0] #put 0's for relevant partners
fulldt[MOST_RECENT_HIRE_DT<='2015-12-31'&EMP_STAT_CD=="Separated"&
         (SEPARATION_DT>='2015-01-01'&SEPARATION_DT<='2015-12-31'), sepduring2015 := 1]
#2016
fulldt[MOST_RECENT_HIRE_DT<='2016-12-31', active2016end := 0] #put 0's for relevant partners
fulldt[MOST_RECENT_HIRE_DT<='2016-12-31'&EMP_STAT_CD=="Active", active2016end := 1]
fulldt[MOST_RECENT_HIRE_DT<='2016-12-31'&EMP_STAT_CD=="Separated"&
         SEPARATION_DT>='2016-01-01', active2016end := 1]
fulldt[MOST_RECENT_HIRE_DT<='2016-12-31', sepduring2016 := 0] #put 0's for relevant partners
fulldt[MOST_RECENT_HIRE_DT<='2016-12-31'&EMP_STAT_CD=="Separated"&
         (SEPARATION_DT>='2016-01-01'&SEPARATION_DT<='2016-12-31'), sepduring2016 := 1]
#2017
fulldt[MOST_RECENT_HIRE_DT<='2017-12-31', active2017end := 0] #put 0's for relevant partners
fulldt[MOST_RECENT_HIRE_DT<='2017-12-31'&EMP_STAT_CD=="Active", active2017end := 1]
fulldt[MOST_RECENT_HIRE_DT<='2017-12-31'&EMP_STAT_CD=="Separated"&
         SEPARATION_DT>='2016-01-01', active2017end := 1]
fulldt[MOST_RECENT_HIRE_DT<='2017-12-31', sepduring2017 := 0] #put 0's for relevant partners
fulldt[MOST_RECENT_HIRE_DT<='2017-12-31'&EMP_STAT_CD=="Separated"&
         (SEPARATION_DT>='2017-01-01'&SEPARATION_DT<='2017-12-31'), sepduring2017 := 1]

#keep a subset
dt <- fulldt[, .(PRTNR_ID, EMP_STAT_CD, MOST_RECENT_HIRE_DT, SEPARATION_DT, 
                 sip2015, sip2016, sip2017, bft2015, bft2016, bft2017, ret2015,
                 ret2016, ret2017, scap2015, scap2016, scap2017, 
                 active2015end, sepduring2015, active2016end, sepduring2016,
                 active2017end, sepduring2017)]
dt[active2015end==1|sepduring2015==1, wasapartnerin2015 := 1];dt[active2015end==0&sepduring2015==0, wasapartnerin2015 := 0]
dt[active2016end==1|sepduring2016==1, wasapartnerin2016 := 1];dt[active2016end==0&sepduring2016==0, wasapartnerin2016 := 0]
dt[active2017end==1|sepduring2017==1, wasapartnerin2017 := 1];dt[active2017end==0&sepduring2017==0, wasapartnerin2017 := 0]

#keep only rows where partners were active in years of analysis (2015, 2016, 2017)
dt <- dt[wasapartnerin2015==1|wasapartnerin2016==1|wasapartnerin2017==1]
#drop separation date since it's causing issues...
dt[, SEPARATION_DT := NULL]
#turn NA's to 0's
dt[is.na(dt)] <- 0

#tenure on dec 31, 2015
#2015
dt[, hiredate := as.Date(MOST_RECENT_HIRE_DT)]
dt[wasapartnerin2015==1, tempdt := as.Date(ymd('2015-12-25'))]
dt[wasapartnerin2015==1, tenure15yrs := as.numeric(difftime(tempdt, hiredate), units = c("days"))]
dt[wasapartnerin2015==1, tenure15yrs := round(tenure15yrs/365,1)]
#2016
dt[wasapartnerin2016==1, tempdt := as.Date(ymd('2016-12-25'))]
dt[wasapartnerin2016==1, tenure16yrs := as.numeric(difftime(tempdt, hiredate), units = c("days"))]
dt[wasapartnerin2016==1, tenure16yrs := round(tenure16yrs/365,1)]
#2017
dt[wasapartnerin2017==1, tempdt := as.Date(ymd('2017-12-25'))]
dt[wasapartnerin2017==1, tenure17yrs := as.numeric(difftime(tempdt, hiredate), units = c("days"))]
dt[wasapartnerin2017==1, tenure17yrs := round(tenure17yrs/365,1)]
#remove fake var
dt[, tempdt := NULL]

#make tenure buckets
#2015
dt[tenure15yrs<0.5, tenure15bin := 1] #<6months
dt[tenure15yrs>=0.5&tenure15yrs<1, tenure15bin := 2] #6months-1year
dt[tenure15yrs>=1&tenure15yrs<2, tenure15bin := 3] #1year-2years
dt[tenure15yrs>=2&tenure15yrs<3, tenure15bin := 4] #2year-3years
dt[tenure15yrs>=3&tenure15yrs<5, tenure15bin := 5] #3year-5years
dt[tenure15yrs>=5&tenure15yrs<7, tenure15bin := 6] #5year-7years
dt[tenure15yrs>=7&tenure15yrs<10, tenure15bin := 7] #7year-10years
dt[tenure15yrs>=10, tenure15bin := 8] #10years+
#2016
dt[tenure16yrs<0.5, tenure16bin := 1] #<6months
dt[tenure16yrs>=0.5&tenure16yrs<1, tenure16bin := 2] #6months-1year
dt[tenure16yrs>=1&tenure16yrs<2, tenure16bin := 3] #1year-2years
dt[tenure16yrs>=2&tenure16yrs<3, tenure16bin := 4] #2year-3years
dt[tenure16yrs>=3&tenure16yrs<5, tenure16bin := 5] #3year-5years
dt[tenure16yrs>=5&tenure16yrs<7, tenure16bin := 6] #5year-7years
dt[tenure16yrs>=7&tenure16yrs<10, tenure16bin := 7] #7year-10years
dt[tenure16yrs>=10, tenure16bin := 8] #10years+
#2017
dt[tenure17yrs<0.5, tenure17bin := 1] #<6months
dt[tenure17yrs>=0.5&tenure17yrs<1, tenure17bin := 2] #6months-1year
dt[tenure17yrs>=1&tenure17yrs<2, tenure17bin := 3] #1year-2years
dt[tenure17yrs>=2&tenure17yrs<3, tenure17bin := 4] #2year-3years
dt[tenure17yrs>=3&tenure17yrs<5, tenure17bin := 5] #3year-5years
dt[tenure17yrs>=5&tenure17yrs<7, tenure17bin := 6] #5year-7years
dt[tenure17yrs>=7&tenure17yrs<10, tenure17bin := 7] #7year-10years
dt[tenure17yrs>=10, tenure17bin := 8] #10years+

#create weights
dtsip15wts <- dt %>%
  filter(wasapartnerin2015==1) %>%
  group_by(sip2015, tenure15bin) %>%
  summarise (n = n()) %>%
  mutate(sipwt15 = n / sum(n))
setDT(dtsip15wts)

dtbft15wts <- dt %>%
  filter(wasapartnerin2015==1) %>%
  group_by(bft2015, tenure15bin) %>%
  summarise (n = n()) %>%
  mutate(bftwt15 = n / sum(n))
setDT(dtbft15wts)

dtret15wts <- dt %>%
  filter(wasapartnerin2015==1) %>%
  group_by(ret2015, tenure15bin) %>%
  summarise (n = n()) %>%
  mutate(retwt15 = n / sum(n))
setDT(dtret15wts)

dtscap15wts <- dt %>%
  filter(wasapartnerin2015==1) %>%
  group_by(scap2015, tenure15bin) %>%
  summarise (n = n()) %>%
  mutate(scapwt15 = n / sum(n))
setDT(dtscap15wts)

#2016
dtsip16wts <- dt %>%
  filter(wasapartnerin2016==1) %>%
  group_by(sip2016, tenure16bin) %>%
  summarise (n = n()) %>%
  mutate(sipwt16 = n / sum(n))
setDT(dtsip16wts)

dtbft16wts <- dt %>%
  filter(wasapartnerin2016==1) %>%
  group_by(bft2016, tenure16bin) %>%
  summarise (n = n()) %>%
  mutate(bftwt16 = n / sum(n))
setDT(dtbft16wts)

dtret16wts <- dt %>%
  filter(wasapartnerin2016==1) %>%
  group_by(ret2016, tenure16bin) %>%
  summarise (n = n()) %>%
  mutate(retwt16 = n / sum(n))
setDT(dtret16wts)

dtscap16wts <- dt %>%
  filter(wasapartnerin2016==1) %>%
  group_by(scap2016, tenure16bin) %>%
  summarise (n = n()) %>%
  mutate(scapwt16 = n / sum(n))
setDT(dtscap16wts)

#2017
dtsip17wts <- dt %>%
  filter(wasapartnerin2017==1) %>%
  group_by(sip2017, tenure17bin) %>%
  summarise (n = n()) %>%
  mutate(sipwt17 = n / sum(n))
setDT(dtsip17wts)

dtbft17wts <- dt %>%
  filter(wasapartnerin2017==1) %>%
  group_by(bft2017, tenure17bin) %>%
  summarise (n = n()) %>%
  mutate(bftwt17 = n / sum(n))
setDT(dtbft17wts)

dtret17wts <- dt %>%
  filter(wasapartnerin2017==1) %>%
  group_by(ret2017, tenure17bin) %>%
  summarise (n = n()) %>%
  mutate(retwt17 = n / sum(n))
setDT(dtret17wts)

dtscap17wts <- dt %>%
  filter(wasapartnerin2017==1) %>%
  group_by(scap2017, tenure17bin) %>%
  summarise (n = n()) %>%
  mutate(scapwt17 = n / sum(n))
setDT(dtscap17wts)

#prep for merging
dtsip15wts <- dtsip15wts[,.(sip2015,tenure15bin,sipwt15)]
dtsip15wts[, sip2015 := abs(sip2015-1)]
dtsip15wts[sip2015==1, sipwt15 := 1]

dtbft15wts <- dtbft15wts[,.(bft2015,tenure15bin,bftwt15)]
dtbft15wts[, bft2015 := abs(bft2015-1)]
dtbft15wts[bft2015==1, bftwt15 := 1]

dtret15wts <- dtret15wts[,.(ret2015,tenure15bin,retwt15)]
dtret15wts[, ret2015 := abs(ret2015-1)]
dtret15wts[ret2015==1, retwt15 := 1]

dtscap15wts <- dtscap15wts[,.(scap2015,tenure15bin,scapwt15)]
dtscap15wts[, scap2015 := abs(scap2015-1)]
dtscap15wts[scap2015==1, scapwt15 := 1]

#2016
dtsip16wts <- dtsip16wts[,.(sip2016,tenure16bin,sipwt16)]
dtsip16wts[, sip2016 := abs(sip2016-1)]
dtsip16wts[sip2016==1, sipwt16 := 1]

dtbft16wts <- dtbft16wts[,.(bft2016,tenure16bin,bftwt16)]
dtbft16wts[, bft2016 := abs(bft2016-1)]
dtbft16wts[bft2016==1, bftwt16 := 1]

dtret16wts <- dtret16wts[,.(ret2016,tenure16bin,retwt16)]
dtret16wts[, ret2016 := abs(ret2016-1)]
dtret16wts[ret2016==1, retwt16 := 1]

dtscap16wts <- dtscap16wts[,.(scap2016,tenure16bin,scapwt16)]
dtscap16wts[, scap2016 := abs(scap2016-1)]
dtscap16wts[scap2016==1, scapwt16 := 1]

#2017
dtsip17wts <- dtsip17wts[,.(sip2017,tenure17bin,sipwt17)]
dtsip17wts[, sip2017 := abs(sip2017-1)]
dtsip17wts[sip2017==1, sipwt17 := 1]

dtbft17wts <- dtbft17wts[,.(bft2017,tenure17bin,bftwt17)]
dtbft17wts[, bft2017 := abs(bft2017-1)]
dtbft17wts[bft2017==1, bftwt17 := 1]

dtret17wts <- dtret17wts[,.(ret2017,tenure17bin,retwt17)]
dtret17wts[, ret2017 := abs(ret2017-1)]
dtret17wts[ret2017==1, retwt17 := 1]

dtscap17wts <- dtscap17wts[,.(scap2017,tenure17bin,scapwt17)]
dtscap17wts[, scap2017 := abs(scap2017-1)]
dtscap17wts[scap2017==1, scapwt17 := 1]

#merge
dt <- Reduce(function(x, y) {merge(x, y, by=c("sip2015","tenure15bin"), all=T)}, list(dt,dtsip15wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("bft2015","tenure15bin"), all=T)}, list(dt,dtbft15wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("ret2015","tenure15bin"), all=T)}, list(dt,dtret15wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("scap2015","tenure15bin"), all=T)}, list(dt,dtscap15wts))

dt <- Reduce(function(x, y) {merge(x, y, by=c("sip2016","tenure16bin"), all=T)}, list(dt,dtsip16wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("bft2016","tenure16bin"), all=T)}, list(dt,dtbft16wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("ret2016","tenure16bin"), all=T)}, list(dt,dtret16wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("scap2016","tenure16bin"), all=T)}, list(dt,dtscap16wts))

dt <- Reduce(function(x, y) {merge(x, y, by=c("sip2017","tenure17bin"), all=T)}, list(dt,dtsip17wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("bft2017","tenure17bin"), all=T)}, list(dt,dtbft17wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("ret2017","tenure17bin"), all=T)}, list(dt,dtret17wts))
dt <- Reduce(function(x, y) {merge(x, y, by=c("scap2017","tenure17bin"), all=T)}, list(dt,dtscap17wts))

#ok - benefits time!
#logic: for active 2015 partners who used XYZ benefit, what is the separation by end of 2015 rate
#2015
dt15sip <- dt[wasapartnerin2015==1, list(benefit = "sip", partnerN = .N,
                                         weight = mean(sipwt15,na.rm=T),
                                         avgtenure = mean(tenure15yrs,na.rm=T),
                                         activeyearend = sum(active2015end,na.rm=T),
                                         sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by=c("sip2015","tenure15bin")]
setnames(dt15sip,"sip2015","utilization")
dt15bft <- dt[wasapartnerin2015==1, list(benefit = "bft", partnerN = .N,
                                         weight = mean(bftwt15,na.rm=T),
                                         avgtenure = mean(tenure15yrs,na.rm=T),
                                         activeyearend = sum(active2015end,na.rm=T),
                                         sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by=c("bft2015","tenure15bin")]
setnames(dt15bft,"bft2015","utilization")
dt15ret <- dt[wasapartnerin2015==1, list(benefit = "ret", partnerN = .N,
                                         weight = mean(retwt15,na.rm=T),
                                         avgtenure = mean(tenure15yrs,na.rm=T),
                                         activeyearend = sum(active2015end,na.rm=T),
                                         sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by=c("ret2015","tenure15bin")]
setnames(dt15ret,"ret2015","utilization")
dt15scap <- dt[wasapartnerin2015==1, list(benefit = "scap", partnerN = .N,
                                          weight = mean(scapwt15,na.rm=T),
                                          avgtenure = mean(tenure15yrs,na.rm=T),
                                          activeyearend = sum(active2015end,na.rm=T),
                                          sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by=c("scap2015","tenure15bin")]
setnames(dt15scap,"scap2015","utilization")

#2016
dt16sip <- dt[wasapartnerin2016==1, list(benefit = "sip", partnerN = .N,
                                         weight = mean(sipwt16,na.rm=T),
                                         avgtenure = mean(tenure16yrs,na.rm=T),
                                         activeyearend = sum(active2016end,na.rm=T),
                                         sepduringyear = sum(sepduring2016,na.rm=T),
                                         sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
              by=c("sip2016","tenure16bin")]
setnames(dt16sip,"sip2016","utilization")
dt16bft <- dt[wasapartnerin2016==1, list(benefit = "bft", partnerN = .N,
                                         weight = mean(bftwt16,na.rm=T),
                                         avgtenure = mean(tenure16yrs,na.rm=T),
                                         activeyearend = sum(active2016end,na.rm=T),
                                         sepduringyear = sum(sepduring2016,na.rm=T),
                                         sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
              by=c("bft2016","tenure16bin")]
setnames(dt16bft,"bft2016","utilization")
dt16ret <- dt[wasapartnerin2016==1, list(benefit = "ret", partnerN = .N,
                                         weight = mean(retwt16,na.rm=T),
                                         avgtenure = mean(tenure16yrs,na.rm=T),
                                         activeyearend = sum(active2016end,na.rm=T),
                                         sepduringyear = sum(sepduring2016,na.rm=T),
                                         sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
              by=c("ret2016","tenure16bin")]
setnames(dt16ret,"ret2016","utilization")
dt16scap <- dt[wasapartnerin2016==1, list(benefit = "scap", partnerN = .N,
                                          weight = mean(scapwt16,na.rm=T),
                                          avgtenure = mean(tenure16yrs,na.rm=T),
                                          activeyearend = sum(active2016end,na.rm=T),
                                          sepduringyear = sum(sepduring2016,na.rm=T),
                                          sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
               by=c("scap2016","tenure16bin")]
setnames(dt16scap,"scap2016","utilization")

#2017
dt17sip <- dt[wasapartnerin2017==1, list(benefit = "sip", partnerN = .N,
                                         weight = mean(sipwt17,na.rm=T),
                                         avgtenure = mean(tenure17yrs,na.rm=T),
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by=c("sip2017","tenure17bin")]
setnames(dt17sip,"sip2017","utilization")
dt17bft <- dt[wasapartnerin2017==1, list(benefit = "bft", partnerN = .N,
                                         weight = mean(bftwt17,na.rm=T),
                                         avgtenure = mean(tenure17yrs,na.rm=T),
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by=c("bft2017","tenure17bin")]
setnames(dt17bft,"bft2017","utilization")
dt17ret <- dt[wasapartnerin2017==1, list(benefit = "ret", partnerN = .N,
                                         weight = mean(retwt17,na.rm=T),
                                         avgtenure = mean(tenure17yrs,na.rm=T),
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by=c("ret2017","tenure17bin")]
setnames(dt17ret,"ret2017","utilization")
dt17scap <- dt[wasapartnerin2017==1, list(benefit = "scap", partnerN = .N,
                                          weight = mean(scapwt17,na.rm=T),
                                          avgtenure = mean(tenure17yrs,na.rm=T),
                                          activeyearend = sum(active2017end,na.rm=T),
                                          sepduringyear = sum(sepduring2017,na.rm=T),
                                          sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
               by=c("scap2017","tenure17bin")]
setnames(dt17scap,"scap2017","utilization")

dt15sip[, tenure15bin := NULL];dt15bft[, tenure15bin := NULL];dt15ret[, tenure15bin := NULL];dt15scap[, tenure15bin := NULL]
dt16sip[, tenure16bin := NULL];dt16bft[, tenure16bin := NULL];dt16ret[, tenure16bin := NULL];dt16scap[, tenure16bin := NULL]
dt17sip[, tenure17bin := NULL];dt17bft[, tenure17bin := NULL];dt17ret[, tenure17bin := NULL];dt17scap[, tenure17bin := NULL]

#rbindlist
l = list(dt15sip,dt15bft,dt15ret,dt15scap,
         dt16sip,dt16bft,dt16ret,dt16scap,
         dt17sip,dt17bft,dt17ret,dt17scap)
dtresults <- rbindlist(l, use.names=T, fill = T)

#weight the data
dtresults[, activeyearend := activeyearend*weight]
dtresults[, sepduringyear := sepduringyear*weight]

#aggregate and recalculate sep_over_hec
dtresults <- dtresults[, list(partnerN = (sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T)),
                 activeyearend = sum(activeyearend,na.rm=T),
                 sepduringyear = sum(sepduringyear,na.rm=T),
                 sep_over_hc = sum(sepduringyear,na.rm=T)/(sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T)),
                 eoy_retention = sum(activeyearend,na.rm=T)/(sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T))),
          by=c("utilization","benefit")]
dtresults[, sep_over_hc := round(sep_over_hc,3)*100]
dtresults[, eoy_retention := round(eoy_retention,3)*100]
#setorder
dtresults <- setorder(dtresults,benefit,utilization)

#write.csv
# write.csv(dtresults,file=paste0(data_dir,"/benefits_terms_results.csv"))


#set labels
xlabels <- c("Healthcare\n(16.1% Participation)","Retirement\n(15.3% Participation)","SCAP\n(1.2% Participation)","Stock Investment Plan\n(4.7% Participation)")
ylabel <- "Annual Retention"
tlabel <- "Annual Retention by Benefit Utilization"
sublabel <- "Annualized Results for 2015, 2016, and 2017, Weighted by Partner Tenure"
caption <- "3-year partner N: 990,401\nData weighted by tenure: weights assigned by benefit- and year-specific participant tenure\nAverage tenure among participants, in years: 5.9 (BFT), 5.8 (RET), 2.9 (SCAP), 7.3 (SIP)\nConstruction of measures:\nBFT, RET, and SIP: enrolled by end of January at start of year\nSCAP: active participant anytime during year\nActive partner: end of year"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Non-Participant", "Participant")
#values
pdata1a <- dtresults
px1a <- dtresults[,benefit]
py1a <- dtresults[,eoy_retention]
groupvar1a <- dtresults[,utilization]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -0.5, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)



#set labels
xlabels <- c("Healthcare\n(16.1% Participation)","Retirement\n(15.3% Participation)","SCAP\n(1.2% Participation)","Stock Investment Plan\n(4.7% Participation)")
ylabel <- "Annual Separation Rate"
tlabel <- "Annual Separation Rate by Benefit Utilization"
sublabel <- "Annualized Results for 2015, 2016, and 2017, Weighted by Partner Tenure"
caption <- "3-year partner N: 990,401\nData weighted by tenure: weights assigned by benefit- and year-specific participant tenure\nAverage tenure among participants, in years: 5.9 (BFT), 5.8 (RET), 2.9 (SCAP), 7.3 (SIP)\nConstruction of measures:\nBFT, RET, and SIP: enrolled by end of January at start of year\nSCAP: active participant anytime during year\nActive partner: end of year"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Non-Participant", "Participant")
#values
pdata1a <- dtresults
px1a <- dtresults[,benefit]
py1a <- dtresults[,sep_over_hc]
groupvar1a <- dtresults[,utilization]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -0.5, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


#total results

#aggregate and recalculate sep_over_hec
dtresults <- dtresults[, list(partnerN = (sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T)),
                              activeyearend = sum(activeyearend,na.rm=T),
                              sepduringyear = sum(sepduringyear,na.rm=T),
                              sep_over_hc = sum(sepduringyear,na.rm=T)/(sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T)),
                              eoy_retention = sum(activeyearend,na.rm=T)/(sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T))),
                       by=c("utilization","benefit")]
dtresults[, sep_over_hc := round(sep_over_hc,3)*100]
dtresults[, eoy_retention := round(eoy_retention,3)*100]
