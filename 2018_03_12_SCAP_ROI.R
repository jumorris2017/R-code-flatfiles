#managing data for SCAP

#load libraries
library(data.table)
library(tidyr)
library(dplyr)

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

#join scap data
fulldt <- Reduce(function(x,y){merge(x,y,by="PRTNR_ID",all=T)}, list(bendt,scap))

