## Training Hours Overspend Analysis -- Pulse & CC ##
## Request from Lisa 11/28/17 ##

#load libraries
library(data.table)
library(plyr)

#load data
#training hours
th <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/q1laborreport.csv")
#setnames
setnames(th,c("STORE_NUM","FISCAL_WEEK_NUMBER","QTD_ACTUAL_TRAINING"),
         c("store_num","fiscalweek","thours"))
#CC data
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_by_store_q1laborreport.csv")
setnames(cc,c("STORE_NUM","FSCL_WK_IN_YR_NUM","CAL_WK_IN_YR_NUM","CCTOTALRESP","CCTBCOUNT"),
         c("store_num","fiscalweek","calweek","totalresp_cc","tbcount_cc"))
cc[, c("store_num") := lapply(.SD, as.numeric), .SDcols=c("store_num")]

#pulse data
p <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pulse_by_store_q1laborreport.csv")
setnames(p,c("PersonnelNumber","survWeek","STORE_NUM_ASSIGNED"),c("pn","calweek","store_num"))
p <- na.omit(p, cols="store_num")

#organize pulse data
temp1 <- p %>% 
  group_by(calweek, Question_ID, store_num) %>%
  summarise(totalresp = n())
setDT(temp1)
temp2 <- p %>% 
  filter(RespID==7) %>%
  group_by(calweek, Question_ID, store_num) %>%
  summarise(tbcount = n())
setDT(temp2)
pf <- merge(temp1,temp2,by=c("calweek","Question_ID","store_num"))
pf <- dcast.data.table(pf, calweek + store_num ~ Question_ID, value.var=c("tbcount","totalresp"))

#merge
full <- merge(th, cc, by=c("store_num","fiscalweek"))
full <- merge(full, pf, by=c("store_num","calweek"))
#get rid of calweek
full[, calweek := NULL]
full <- full[, lapply(.SD,sum,na.rm=T), by=c("store_num","fiscalweek")]

# #create delta variable
# full <- setorder(full,store_num,fiscalweek)
# #this basic shift takes previous store value - *not* necessarily the previous week
# full[, thourslag := shift(thours, 1L, fill=NA, type="lag"), by="store_num"]

#aggregate up to get topline
fullwk <- full[, store_num := NULL]
fullwk <- fullwk[, lapply(.SD,sum,na.rm=T), by=c("fiscalweek")]
fullwk <- setorder(fullwk,fiscalweek)
fullwk[, thourslag := shift(thours, 1L, fill=NA, type="lag")]
fullwk[fiscalweek>1, tdelta := thours - thourslag]

#cc top box
fullwk[, ccscore := tbcount_cc / totalresp_cc]
fullwk[, Q1score := tbcount_Q1 / totalresp_Q1]
fullwk[, Q2_Ascore := tbcount_Q2_A / totalresp_Q2_A]
fullwk[, Q2_Bscore := tbcount_Q2_B / totalresp_Q2_B]
fullwk[, Q2_Cscore := tbcount_Q2_C / totalresp_Q2_C]
fullwk[, Q2_Dscore := tbcount_Q2_D / totalresp_Q2_D]
fullwk[, Q2_Escore := tbcount_Q2_E / totalresp_Q2_E]









