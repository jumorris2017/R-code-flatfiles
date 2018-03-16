#managing data for SCAP

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)

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

#ok - benefits time!
#logic: for active 2015 partners who used XYZ benefit, what is the separation by end of 2015 rate
#2015
dt15sip <- dt[wasapartnerin2015==1, list(benefit = "sip", partnerN = .N,
                                         activeyearend = sum(active2015end,na.rm=T),
                                         sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by="sip2015"]
setnames(dt15sip,"sip2015","utilization")
dt15bft <- dt[wasapartnerin2015==1, list(benefit = "bft", partnerN = .N,
                                         activeyearend = sum(active2015end,na.rm=T),
                                         sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by="bft2015"]
setnames(dt15bft,"bft2015","utilization")
dt15ret <- dt[wasapartnerin2015==1, list(benefit = "ret", partnerN = .N,
                                         activeyearend = sum(active2015end,na.rm=T),
                                         sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by="ret2015"]
setnames(dt15ret,"ret2015","utilization")
dt15scap <- dt[wasapartnerin2015==1, list(benefit = "scap", partnerN = .N,
                                          activeyearend = sum(active2015end,na.rm=T),
                                          sepduringyear = sum(sepduring2015,na.rm=T),
                                         sep_over_hec = sum(sepduring2015,na.rm=T)/(sum(sepduring2015,na.rm=T)+sum(active2015end,na.rm=T))),
              by="scap2015"]
setnames(dt15scap,"scap2015","utilization")

#2016
dt16sip <- dt[wasapartnerin2016==1, list(benefit = "sip", partnerN = .N,
                                         activeyearend = sum(active2016end,na.rm=T),
                                         sepduringyear = sum(sepduring2016,na.rm=T),
                                         sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
              by="sip2016"]
setnames(dt16sip,"sip2016","utilization")
dt16bft <- dt[wasapartnerin2016==1, list(benefit = "bft", partnerN = .N,
                                         activeyearend = sum(active2016end,na.rm=T),
                                         sepduringyear = sum(sepduring2016,na.rm=T),
                                         sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
              by="bft2016"]
setnames(dt16bft,"bft2016","utilization")
dt16ret <- dt[wasapartnerin2016==1, list(benefit = "ret", partnerN = .N,
                                         activeyearend = sum(active2016end,na.rm=T),
                                         sepduringyear = sum(sepduring2016,na.rm=T),
                                         sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
              by="ret2016"]
setnames(dt16ret,"ret2016","utilization")
dt16scap <- dt[wasapartnerin2016==1, list(benefit = "scap", partnerN = .N,
                                          activeyearend = sum(active2016end,na.rm=T),
                                          sepduringyear = sum(sepduring2016,na.rm=T),
                                          sep_over_hec = sum(sepduring2016,na.rm=T)/(sum(sepduring2016,na.rm=T)+sum(active2016end,na.rm=T))),
               by="scap2016"]
setnames(dt16scap,"scap2016","utilization")

#2017
dt17sip <- dt[wasapartnerin2017==1, list(benefit = "sip", partnerN = .N,
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by="sip2017"]
setnames(dt17sip,"sip2017","utilization")
dt17bft <- dt[wasapartnerin2017==1, list(benefit = "bft", partnerN = .N,
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by="bft2017"]
setnames(dt17bft,"bft2017","utilization")
dt17ret <- dt[wasapartnerin2017==1, list(benefit = "ret", partnerN = .N,
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by="ret2017"]
setnames(dt17ret,"ret2017","utilization")
dt17scap <- dt[wasapartnerin2017==1, list(benefit = "scap", partnerN = .N,
                                          activeyearend = sum(active2017end,na.rm=T),
                                          sepduringyear = sum(sepduring2017,na.rm=T),
                                          sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
               by="scap2017"]
setnames(dt17scap,"scap2017","utilization")

#rbindlist
l = list(dt15sip,dt15bft,dt15ret,dt15scap,
         dt16sip,dt16bft,dt16ret,dt16scap,
         dt17sip,dt17bft,dt17ret,dt17scap)
dtresults <- rbindlist(l, use.names=T, fill = T)

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
sublabel <- "Annualized Results for 2015, 2016, and 2017"
caption <- "3-year partner N: 990,401\nAverage tenure among participants, in years: 5.9 (BFT), 5.8 (RET), 2.9 (SCAP), 7.3 (SIP)\nConstruction of measures:\nBFT, RET, and SIP: enrolled by end of January at start of year\nSCAP: active participant anytime during year\nActive partner: end of year"
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
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)



#set labels
xlabels <- c("Health","Retirement","SCAP","Stocks & Investments")
ylabel <- "Annual Separation Rate"
tlabel <- "Annual Separation Rate by Benefit Utilization"
sublabel <- "Annualized Results for 2015, 2016, and 2017"
caption <- "BFT, RET, and SIP: enrolled by end of January at start of year\nSCAP: active participant anytime during year\nSeparations: anytime during year"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Did not participate", "Participated")
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
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


#total results
dtagg <- dtresults[, list(partnerN = (sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T)),
                          activeyearend = sum(activeyearend,na.rm=T),
                          sepduringyear = sum(sepduringyear,na.rm=T),
                          sep_over_hc = sum(sepduringyear,na.rm=T)/(sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T)),
                          eoy_retention = sum(activeyearend,na.rm=T)/(sum(sepduringyear,na.rm=T)+sum(activeyearend,na.rm=T)))]
dtagg[, sep_over_hc := round(sep_over_hc,3)*100]
dtagg[, eoy_retention := round(eoy_retention,3)*100]