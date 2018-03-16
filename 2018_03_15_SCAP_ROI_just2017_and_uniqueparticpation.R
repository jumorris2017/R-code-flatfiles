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
activedt[, period17N := rowSums(.SD,na.rm=T), .SDcols=grep("2017",colnames(activedt),value=T)]
activedt[period17N>=1, scap2017 := 1]
activedt[period17N==0, scap2017 := 0]

#subset vars
scap <- activedt[, .(PARTNERID,scap2017)]

#write.csv
# write.csv(activedt, file="C:/Users/jumorris/Desktop/SCAP_PTF/compiledDT/scap_participants.csv")

#read it back in!
# scap <- fread("C:/Users/jumorris/Desktop/SCAP_PTF/compiledDT/scap_participants.csv")
#set names to match benefits data
setnames(scap,"PARTNERID","PRTNR_ID")
# #pull year from start_date variable
# scap[, start_date_scap := substr(start_date,1,4)]
# #keep just partner number and scap year
# scap <- scap[, .(PRTNR_ID,start_date_scap)]
#swing wide
#create dataset of binary indicators where partners were active in scap
# scap <- scap %>%
#   mutate(yesno = 1) %>%
#   distinct %>%
#   spread(start_date_scap, yesno, fill = 0)
# setDT(scap)
# colnames(scap)[2:ncol(scap)] <- paste0("scap",colnames(scap)[2:ncol(scap)])

#load benefits data
#only includes januaries.... 
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
ben <- fread(paste0(data_dir,"/scap_pnumber_2017start.csv"))
setnames(ben,"Personnel Number","PRTNR_ID")
ben2 <- fread(paste0(data_dir,"/scap_pnumber_2017end.csv"))
ben2[, activeforsure := 1]
setnames(ben2,"Personnel Number","PRTNR_ID")
ben2 <- ben2[, .(PRTNR_ID,activeforsure)]
ben <- left_join(ben,ben2,by="PRTNR_ID")
#order by FP_End
#order by FP_End
# ben <- setorder(ben,FP_End)
# ben[, fpend := mdy(FP_End)]
# ben[, fpyear := year(fpend)]
# 
# #restrict to only BFT eligible partners
# # ben <- ben[BFT_Elig==1]
# 
# #keep only 2017 data
# ben <- ben[fpyear==2017]

# #just keep benefits participation variables
# 
# #create dataset of binary indicators where partners were active participants
# bsipdt <- dcast.data.table(ben, PRTNR_ID ~ fpyear, value.var="SIP_Part", fun.aggregate=max)
# colnames(bsipdt)[2:ncol(bsipdt)] <- paste0("sip",colnames(bsipdt)[2:ncol(bsipdt)])
# 
# bbftdt <- dcast.data.table(ben, PRTNR_ID ~ fpyear, value.var="BFT_Part", fun.aggregate=max)
# colnames(bbftdt)[2:ncol(bbftdt)] <- paste0("bft",colnames(bbftdt)[2:ncol(bbftdt)])
# 
# bretdt <- dcast.data.table(ben, PRTNR_ID ~ fpyear, value.var="RET_Part", fun.aggregate=max)
# colnames(bretdt)[2:ncol(bretdt)] <- paste0("ret",colnames(bretdt)[2:ncol(bretdt)])
# 
# #merge together
# bendt <- Reduce(function(x, y) {merge(x, y, by=c("PRTNR_ID"), all=T)}, list(bsipdt,bbftdt,bretdt))
# 
# #grab start date & remove name character components
# bendt[, start_date_sip := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = grep("sip",colnames(bendt),value=T)]
# bendt[, start_date_sip := gsub("sip", "", bendt[, start_date_sip])]
# bendt[sip2015==0&sip2016==0&sip2017==0&sip2018==0, start_date_sip := NA]
# bendt[, start_date_bft := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = grep("bft",colnames(bendt),value=T)]
# bendt[, start_date_bft := gsub("bft", "", bendt[, start_date_bft])]
# bendt[bft2015==0&bft2016==0&bft2017==0&bft2018==0, start_date_bft := NA]
# bendt[, start_date_ret := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = grep("ret",colnames(bendt),value=T)]
# bendt[, start_date_ret := gsub("ret", "", bendt[, start_date_ret])]
# bendt[ret2015==0&ret2016==0&ret2017==0&ret2018==0, start_date_ret := NA]

#load turnover data
prtnr <- fread(paste0(data_dir,"/SCAP_analysis_partner_file.csv"))
#set names to match benefits data
prtnr[, (grep("DT",colnames(prtnr),value=T)) := lapply(.SD, function(x) lubridate::mdy_hm(x)), .SDcols=grep("DT",colnames(prtnr),value=T)]

#RESTRICT BY JOB ROLE
# prtnr <- prtnr[JOB_ID==50000362] #BARISTA
# prtnr <- prtnr[JOB_ID==50000358] #SHIFT
# prtnr <- prtnr[JOB_ID==50000117] #SM

#join scap data
fulldt <- Reduce(function(x,y){merge(x,y,by="PRTNR_ID",all=T)}, list(prtnr,scap))
fulldt <- left_join(ben,fulldt,by="PRTNR_ID")
setDT(fulldt)

# #make separation date NA if still active (happens for multiple-hires)
# fulldt[EMP_STAT_CD=="Active", SEPARATION_DT := NA]
# 
# #create indicators for whether still an active partner at end of year
# #2017
# fulldt[MOST_RECENT_HIRE_DT<='2017-12-31', active2017end := 0] #put 0's for relevant partners
# fulldt[MOST_RECENT_HIRE_DT<='2017-12-31'&EMP_STAT_CD=="Active", active2017end := 1]
# fulldt[MOST_RECENT_HIRE_DT<='2017-12-31'&EMP_STAT_CD=="Separated"&
#          SEPARATION_DT>='2018-01-01', active2017end := 1]
# fulldt[MOST_RECENT_HIRE_DT<='2017-12-31', sepduring2017 := 0] #put 0's for relevant partners
# fulldt[MOST_RECENT_HIRE_DT<='2017-12-31'&EMP_STAT_CD=="Separated"&
#          (SEPARATION_DT>='2017-01-01'&SEPARATION_DT<='2017-12-31'), sepduring2017 := 1]

#keep a subset
dt <- fulldt[, .(PRTNR_ID, activeforsure, 
                 SIP_Part, BFT_Part, RET_Part, scap2017, JOB_ID)]
dt[, active2017end := 0]
dt[activeforsure==1, active2017end := 1]
dt[active2017end==0, sepduring2017 := 0]
dt[active2017end==1, sepduring2017 := 1]

# dt[active2017end==1|sepduring2017==1, wasapartnerin2017 := 1];dt[active2017end==0&sepduring2017==0, wasapartnerin2017 := 0]
setnames(dt, c("SIP_Part", "BFT_Part", "RET_Part"), c("sip2017","bft2017","ret2017"))
#keep only rows where partners were active in years of analysis (2017)
# dt <- dt[wasapartnerin2017==1]
# #drop separation date since it's causing issues...
# dt[, SEPARATION_DT := NULL]
#turn NA's to 0's
dt[is.na(dt)] <- 0

#get rid of 0's
dt <- dt[JOB_ID>1]

# #tenure on dec 31, 2017
# dt[, hiredate := as.Date(MOST_RECENT_HIRE_DT)]
# dt[wasapartnerin2017==1, tempdt := as.Date(ymd('2017-12-25'))]
# dt[wasapartnerin2017==1, tenure17yrs := as.numeric(difftime(tempdt, hiredate), units = c("days"))]
# dt[wasapartnerin2017==1, tenure17yrs := round(tenure17yrs/365,1)]
# #remove fake var
# # dt[, tempdt := NULL]
# # # 
# #pull in pulse data
pulse <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/SCAP_pulse_cal2017.csv")
setnames(pulse,"SAP_PRTNR_ID","PRTNR_ID")
#keep only partners in dt data
pulse <- pulse[PRTNR_ID %in% dt[,PRTNR_ID]]
pulse <- Reduce(function(x,y){merge(x,y,by="PRTNR_ID",all=T)}, list(pulse,fulldt))
setDT(pulse)
pulse <- na.omit(pulse,cols=c("scap2017","JOB_ID"))
pulsedt <- pulse[, list(partnerN = .N, RespPP = round(sum(Q1_RESP,na.rm=T)/.N,1),
                        Q1_RESP = sum(Q1_RESP,na.rm=T),
                   Q1_7 = sum(Q1_7,na.rm=T),
                   Q1_6 = sum(Q1_6,na.rm=T),
                   Q1_5 = sum(Q1_5,na.rm=T),
                   Q1_4 = sum(Q1_4,na.rm=T),
                   Q1_3 = sum(Q1_3,na.rm=T),
                   Q1_2 = sum(Q1_2,na.rm=T),
                   Q1_1 = sum(Q1_1,na.rm=T)), by=c("scap2017","JOB_ID")]
pulsedt[, q1tb := round(Q1_7/Q1_RESP,3)*100]
pulsedt[, q1tb2 := round((Q1_7+Q1_6)/Q1_RESP,3)*100]
pulsedt[, q1tb3 := round((Q1_7+Q1_6+Q1_5)/Q1_RESP,3)*100]
pulsedt[, q1avg := round((Q1_7*7+Q1_6*6+Q1_5*5+Q1_4*4+Q1_3*3+Q1_2*2+Q1_1*1)/Q1_RESP,2)]
pulsedt <- setorder(pulsedt,JOB_ID,scap2017)

# #agg scores by SCAP and non-SCAP
# dtscaptemp <- dt[, .(PRTNR_ID,scap2017)]
# pulse <- 
# #ok - benefits time!
#2017
dt17sip <- dt[, list(benefit = "sip", partnerN = .N,
                                         #avgtenure = mean(tenure17yrs,na.rm=T),
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by=c("sip2017","JOB_ID")]
setnames(dt17sip,"sip2017","utilization")
dt17bft <- dt[, list(benefit = "bft", partnerN = .N,
                                         #avgtenure = mean(tenure17yrs,na.rm=T),
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by=c("bft2017","JOB_ID")]
setnames(dt17bft,"bft2017","utilization")
dt17ret <- dt[, list(benefit = "ret", partnerN = .N,
                                         #avgtenure = mean(tenure17yrs,na.rm=T),
                                         activeyearend = sum(active2017end,na.rm=T),
                                         sepduringyear = sum(sepduring2017,na.rm=T),
                                         sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by=c("ret2017","JOB_ID")]
setnames(dt17ret,"ret2017","utilization")
dt17scap <- dt[, list(benefit = "scap", partnerN = .N,
                                          #avgtenure = mean(tenure17yrs,na.rm=T),
                                          activeyearend = sum(active2017end,na.rm=T),
                                          sepduringyear = sum(sepduring2017,na.rm=T),
                                          sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
               by=c("scap2017","JOB_ID")]
setnames(dt17scap,"scap2017","utilization")

#rbindlist
l = list(dt17sip,dt17bft,dt17ret,dt17scap)
dtresults <- rbindlist(l, use.names=T, fill = T)

# #pull in headcount
# dtresults[JOB_ID==50000362, HC := 118273]
# dtresults[JOB_ID==50000358, HC := 35541]
# dtresults[JOB_ID==50000117, HC := 118273]

#aggregate and recalculate sep_over_hec
dtresults <- dtresults[, list(partnerN = sum(partnerN),
                 activeyearend = sum(activeyearend,na.rm=T),
                 sepduringyear = sum(sepduringyear,na.rm=T),
                 sep_over_hc = sum(sepduringyear,na.rm=T)/sum(partnerN),
                 eoy_retention = sum(activeyearend,na.rm=T)/sum(partnerN)),
          by=c("utilization","benefit","JOB_ID")]
dtresults[, sep_over_hc := round(sep_over_hc,3)*100]
dtresults[, eoy_retention := round(eoy_retention,3)*100]
#setorder
dtresults <- setorder(dtresults,benefit,utilization)

#total results
dtagg <- dt[, list(partnerN = (sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T)),
                   active2017end = sum(active2017end,na.rm=T),
                   sepduring2017 = sum(sepduring2017,na.rm=T),
                   sep_over_hc = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T)),
                   eoy_retention = sum(active2017end,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
            by=c("JOB_ID")]
dtagg[, sep_over_hc := round(sep_over_hc,3)*100]
dtagg[, eoy_retention := round(eoy_retention,3)*100]

#agg and swing to get participation rates
dtp <- dcast.data.table(dtresults, benefit + activeyearend + JOB_ID ~ utilization, value.var="partnerN")
setnames(dtp,c("0","1"),c("nonPar","Par"))
dtp <- dtp[, list(activeyearend = sum(activeyearend,na.rm=T),
                  nonPar = sum(nonPar,na.rm=T),
                  Par = sum(Par,na.rm=T)),by=c("benefit","JOB_ID")]
dtp[, parpct := Par / activeyearend]

#total N and retention rate
dtagg

#participation rates
dtp

#avg tenure
dt17bft
dt17ret
dt17scap
dt17sip

####SMs
#set labels
xlabels <- c("Healthcare","Retirement","SCAP","Stock Investment Plan")
ylabel <- "2017 Retention"
tlabel <- "Store Managers"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Non-Participant", "Participant")
#values
pdata1a <- dtresults[JOB_ID==50000117]
px1a <- dtresults[JOB_ID==50000117,benefit]
py1a <- dtresults[JOB_ID==50000117,eoy_retention]
groupvar1a <- dtresults[JOB_ID==50000117,utilization]
nvar <- dtresults[JOB_ID==50000117,partnerN]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) +
  geom_text(size = 3, aes(label=paste0("n=",nvar),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) +
    geom_text(size = 3.5, aes(label=paste0(py1a,"%")), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)

####SHIFTS
#set labels
xlabels <- c("Healthcare","Retirement","SCAP","Stock Investment Plan")
ylabel <- "2017 Retention"
tlabel <- "Shift Supervisors"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Non-Participant", "Participant")
#values
pdata1a <- dtresults[JOB_ID==50000358]
px1a <- dtresults[JOB_ID==50000358,benefit]
py1a <- dtresults[JOB_ID==50000358,eoy_retention]
groupvar1a <- dtresults[JOB_ID==50000358,utilization]
nvar <- dtresults[JOB_ID==50000358,partnerN]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) +
  geom_text(size = 3, aes(label=paste0("n=",nvar),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) +
  geom_text(size = 3.5, aes(label=paste0(py1a,"%")), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


####BARISTA
#set labels
xlabels <- c("Healthcare","Retirement","SCAP","Stock Investment Plan")
ylabel <- "2017 Retention"
tlabel <- "Baristas"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Non-Participant", "Participant")
#values
pdata1a <- dtresults[JOB_ID==50000362]
px1a <- dtresults[JOB_ID==50000362,benefit]
py1a <- dtresults[JOB_ID==50000362,eoy_retention]
groupvar1a <- dtresults[JOB_ID==50000362,utilization]
nvar <- dtresults[JOB_ID==50000362,partnerN]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) +
  geom_text(size = 3, aes(label=paste0("n=",nvar),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) +
  geom_text(size = 3.5, aes(label=paste0(py1a,"%")), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


# ####BARISTA
# #set labels
# xlabels <- c("Healthcare\n(19.2% Participation)","Retirement\n(17.6% Participation)","SCAP\n(4.8% Participation)","Stock Investment Plan\n(4.1% Participation)")
# ylabel <- "2017 Retention"
# tlabel <- "Annual Retention by Benefit Utilization"
# sublabel <- "U.S. Company-Operated Store Partners"
# #manual legend labels
# lname <- "Benefit Utilization"
# llabels <- c("Non-Participant", "Participant")
# #values
# pdata1a <- dtresults[JOB_ID==50000362]
# px1a <- dtresults[JOB_ID==50000362,benefit]
# py1a <- dtresults[JOB_ID==50000362,eoy_retention]
# groupvar1a <- dtresults[JOB_ID==50000362,utilization]
# #plot itself
# plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
#   geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
#   scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
#   scale_x_discrete(name="",labels=xlabels) +
#   scale_y_continuous(limits=c(0,100)) +
#   xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel) +
#   geom_text(size = 3.5, aes(label=paste0(py1a,"%"),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
# print(plot1a)
# 
# 

# #sm
# dtp_sm <- dtp
# dtp_sm <- dtp_sm[, .(benefit,parpct)]
# dtp_sm[, role := "sm"]
# #shift
# dtp_ss <- dtp
# dtp_ss <- dtp_ss[, .(benefit,parpct)]
# dtp_ss[, role := "sh"]
# #barista
# dtp_bar <- dtp
# dtp_bar <- dtp_bar[, .(benefit,parpct)]
# dtp_bar[, role := "bar"]

# #rbind
# l = list(dtp_sm,dtp_ss,dtp_bar)
# dtpfull <- rbindlist(l,use.names=T,fill=T)
# dtpfull[, parpct := round(parpct,3)*100]
# dtpfull <- setorder(dtpfull,benefit,role)

#hard code from query data
dtp[benefit=="bft"&JOB_ID==50000362, parpct := round(22756/118273,3)*100]
dtp[benefit=="bft"&JOB_ID==50000358, parpct := round(19648/35540,3)*100]
dtp[benefit=="bft"&JOB_ID==50000117, parpct := round(7138/8301,3)*100]

dtp[benefit=="ret"&JOB_ID==50000362, parpct := round(27561/118273,3)*100]
dtp[benefit=="ret"&JOB_ID==50000358, parpct := round(17573/35540,3)*100]
dtp[benefit=="ret"&JOB_ID==50000117, parpct := round(6417/8301,3)*100]

dtp[benefit=="sip"&JOB_ID==50000362, parpct := round(5785/118273,3)*100]
dtp[benefit=="sip"&JOB_ID==50000358, parpct := round(4806/35540,3)*100]
dtp[benefit=="sip"&JOB_ID==50000117, parpct := round(2400/8301,3)*100]

#eoy SCAP over beggining of year BFT eligible
dtp[benefit=="scap"&JOB_ID==50000362, parpct := round(4744/118273,3)*100]
dtp[benefit=="scap"&JOB_ID==50000358, parpct := round(4688/35540,3)*100]
dtp[benefit=="scap"&JOB_ID==50000117, parpct := round(818/8301,3)*100]

#create new var for plotting
dtp[JOB_ID==50000362, jobvar := 1]
dtp[JOB_ID==50000358, jobvar := 2]
dtp[JOB_ID==50000117, jobvar := 3]


xlabels <- c("Healthcare","Retirement","SCAP","Stock Investment Plan")
ylabel <- "Participation"
tlabel <- "Benefit Utilization, 2017, U.S. CO Store Partners"
caption <- "Partner N: 162,114"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Barista", "Shift Supervisors", "Store Managers")
#values
pdata1a <- dtp
px1a <- dtp[,benefit]
py1a <- dtp[,parpct]
groupvar1a <- dtp[,jobvar]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(caption=caption) +
  geom_text(size = 3.5, aes(label=paste0(py1a,"%")), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)












#hard code from query data
dtp[benefit=="bft"&JOB_ID==50000362, parpct := 33.4]
dtp[benefit=="bft"&JOB_ID==50000358, parpct := 58.1]
dtp[benefit=="bft"&JOB_ID==50000117, parpct := 86.4]

dtp[benefit=="ret"&JOB_ID==50000362, parpct := 31.6]
dtp[benefit=="ret"&JOB_ID==50000358, parpct := 50.4]
dtp[benefit=="ret"&JOB_ID==50000117, parpct := 77.7]

dtp[benefit=="sip"&JOB_ID==50000362, parpct := ]6.7
dtp[benefit=="sip"&JOB_ID==50000358, parpct := 13.5]
dtp[benefit=="sip"&JOB_ID==50000117, parpct := 29.1]

#eoy SCAP over beggining of year BFT eligible
dtp[benefit=="scap"&JOB_ID==50000362, parpct := round(4744/64830,3)*100]
dtp[benefit=="scap"&JOB_ID==50000358, parpct := round(4688/33574,3)*100]
dtp[benefit=="scap"&JOB_ID==50000117, parpct := round(818/8182,3)*100]

#create new var for plotting
dtp[JOB_ID==50000362, jobvar := 1]
dtp[JOB_ID==50000358, jobvar := 2]
dtp[JOB_ID==50000117, jobvar := 3]


xlabels <- c("Healthcare","Retirement","SCAP","Stock Investment Plan")
ylabel <- "Participation"
tlabel <- "Benefit Utilization, 2017, Benefits-Eligible U.S. CO Store Partners"
caption <- "Partner N: 106,640"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Barista", "Shift Supervisors", "Store Managers")
#values
pdata1a <- dtp
px1a <- dtp[,benefit]
py1a <- dtp[,parpct]
groupvar1a <- dtp[,jobvar]
nvar <- dtp[,Par]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(caption=caption) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar),y=0), stat="identity", vjust = -0.5, position = position_dodge(0.95)) +
  geom_text(size = 3.5, aes(label=paste0(py1a,"%")), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)






#create new only/both variables
dt[bft2017==1&scap2017==1, bothbftscap := 1]
dt[bft2017==0|scap2017==0, bothbftscap := 0]

dt[bft2017==0&scap2017==1, onlyscap := 1]
dt[scap2017==0|(scap2017==1&bft2017==1), onlyscap := 0]

dt[scap2017==0&bft2017==1, onlybft := 1]
dt[bft2017==0|(bft2017==1&scap2017==1), onlybft := 0]

dt17both <- dt[, list(benefit = "both", partnerN = .N,
                     #avgtenure = mean(tenure17yrs,na.rm=T),
                     activeyearend = sum(active2017end,na.rm=T),
                     sepduringyear = sum(sepduring2017,na.rm=T),
                     sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
              by=c("bothbftscap","JOB_ID")]
setnames(dt17both,"bothbftscap","utilization")

dt17onlyscap <- dt[, list(benefit = "onlyscap", partnerN = .N,
                      #avgtenure = mean(tenure17yrs,na.rm=T),
                      activeyearend = sum(active2017end,na.rm=T),
                      sepduringyear = sum(sepduring2017,na.rm=T),
                      sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
               by=c("onlyscap","JOB_ID")]
setnames(dt17onlyscap,"onlyscap","utilization")

dt17onlybft <- dt[, list(benefit = "onlybft", partnerN = .N,
                      #avgtenure = mean(tenure17yrs,na.rm=T),
                      activeyearend = sum(active2017end,na.rm=T),
                      sepduringyear = sum(sepduring2017,na.rm=T),
                      sep_over_hec = sum(sepduring2017,na.rm=T)/(sum(sepduring2017,na.rm=T)+sum(active2017end,na.rm=T))),
               by=c("onlybft","JOB_ID")]
setnames(dt17onlybft,"onlybft","utilization")



#rbindlist
l = list(dt17both,dt17onlyscap,dt17onlybft)
dtresults <- rbindlist(l, use.names=T, fill = T)
dtresults <- na.omit(dtresults,cols="utilization")

#aggregate and recalculate sep_over_hec
dtresults[, eoy_retention := activeyearend/partnerN]
dtresults[, eoy_retention := round(eoy_retention,3)*100]
#setorder
dtresults <- setorder(dtresults,JOB_ID,benefit,utilization)

#agg and swing to get participation rates
dtp <- dcast.data.table(dtresults, benefit + JOB_ID + activeyearend ~ utilization, value.var="partnerN")
setnames(dtp,c("0","1"),c("nonPar","Par"))
dtp <- dtp[, list(activeyearend = sum(activeyearend,na.rm=T),
                  nonPar = sum(nonPar,na.rm=T),
                  Par = sum(Par,na.rm=T)),by="benefit"]
dtp[, parpct := Par / activeyearend]

#participation rates
dtp

#avg tenure
dt17both
dt17onlyscap
dt17onlybft


#create new var for plotting
dtresults[JOB_ID==50000362, jobvar := 1]
dtresults[JOB_ID==50000358, jobvar := 2]
dtresults[JOB_ID==50000117, jobvar := 3]

dtresults[benefit=="both", benvar := 1]
dtresults[benefit=="onlyscap", benvar := 2]
dtresults[benefit=="onlybft", benvar := 3]

xlabels <- c("Barista", "Shift Supervisors", "Store Managers")
ylabel <- "2017 Retention"
tlabel <- "Annual Retention by Benefit Utilization Pairings"
subtitle <- "U.S. Company-Operated Store Partners"
#manual legend labels
lname <- "Benefit Utilization"
llabels <- c("Both SCAP/Healthcare","SCAP Only","Healthcare Only")
#values
pdata1a <- dtresults[utilization==1]
px1a <- dtresults[utilization==1,jobvar]
py1a <- dtresults[utilization==1,eoy_retention]
groupvar1a <- dtresults[utilization==1,benvar]
nvar <- dtresults[utilization==1,partnerN]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,105)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + 
  geom_text(size = 3, aes(label=paste0("n=",nvar),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) +
  geom_text(size = 3.5, aes(label=paste0(py1a,"%")), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)



