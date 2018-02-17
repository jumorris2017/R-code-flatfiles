##Generating necessary sample to get a noticeable shift in speed perceptions

#pull speed perceptions by store, by week, for multiple weeks

#look at distribution of scores to assess level of detectable shift

#assess the size necessary to detect that shift

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)

#load data
sppa <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/power_analysis_speed_mop.csv") #speed scores by store
setnames(sppa, c("TB_COUNT","RSPNS_COUNT"),c("Q2_1_TB_CNT","Q2_1_RESPONSE_TOTAL"))
sppa[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]

#one week
#summarise mean and standard deviation
sppa1 <- sppa[FSCL_WK_IN_YR_NUM==17]
wk1 <- sppa1 %>% 
  group_by(FSCL_YR_NUM) %>% 
  summarise(Nstores = length(unique(STORE_NUM)),
            total_resp = sum(Q2_1_RESPONSE_TOTAL,na.rm=T),
            avg_store_resp = round(mean(Q2_1_RESPONSE_TOTAL,na.rm=T),1),
            sp_mean = round(mean(Q2_1_TB_SCORE,na.rm=T),4)*100,
            sp_sd = round(sd(Q2_1_TB_SCORE,na.rm=T),4)*100)
setDT(wk1)
wk1[, Npcalc_1pt := ceiling(((1.96^2)*(sp_sd^2))/(1^2))]
wk1[, Npcalc_2pts := ceiling(((1.96^2)*(sp_sd^2))/(2^2))]
wk1[, Npcalc_3pts := ceiling(((1.96^2)*(sp_sd^2))/(3^2))]
#group two weeks
sppa2 <- sppa[FSCL_WK_IN_YR_NUM==16|FSCL_WK_IN_YR_NUM==17, 
              list(Q2_1_TB_CNT = sum(Q2_1_TB_CNT,na.rm=T),
                   Q2_1_RESPONSE_TOTAL = sum(Q2_1_RESPONSE_TOTAL,na.rm=T),
                   Q2_1_TB_SCORE = sum(Q2_1_TB_CNT,na.rm=T)/sum(Q2_1_RESPONSE_TOTAL,na.rm=T)),
              by=c("STORE_NUM","FSCL_YR_NUM")]
#summarise mean and standard deviation
wk2 <- sppa2 %>% 
  group_by(FSCL_YR_NUM) %>% 
  summarise(Nstores = length(unique(STORE_NUM)),
            total_resp = sum(Q2_1_RESPONSE_TOTAL,na.rm=T),
            avg_store_resp = round(mean(Q2_1_RESPONSE_TOTAL,na.rm=T),1),
            sp_mean = round(mean(Q2_1_TB_SCORE,na.rm=T),4)*100,
            sp_sd = round(sd(Q2_1_TB_SCORE,na.rm=T),4)*100)
setDT(wk2)
wk2[, Npcalc_1pt := ceiling(((1.96^2)*(sp_sd^2))/(1^2))]
wk2[, Npcalc_2pts := ceiling(((1.96^2)*(sp_sd^2))/(2^2))]
wk2[, Npcalc_3pts := ceiling(((1.96^2)*(sp_sd^2))/(3^2))]
#group three weeks
sppa3 <- sppa[FSCL_WK_IN_YR_NUM>=15&FSCL_WK_IN_YR_NUM<=17, 
              list(Q2_1_TB_CNT = sum(Q2_1_TB_CNT,na.rm=T),
                   Q2_1_RESPONSE_TOTAL = sum(Q2_1_RESPONSE_TOTAL,na.rm=T),
                   Q2_1_TB_SCORE = sum(Q2_1_TB_CNT,na.rm=T)/sum(Q2_1_RESPONSE_TOTAL,na.rm=T)),
              by=c("STORE_NUM","FSCL_YR_NUM")]
#summarise mean and standard deviation
wk3 <- sppa3 %>% 
  group_by(FSCL_YR_NUM) %>% 
  summarise(Nstores = length(unique(STORE_NUM)),
            total_resp = sum(Q2_1_RESPONSE_TOTAL,na.rm=T),
            avg_store_resp = round(mean(Q2_1_RESPONSE_TOTAL,na.rm=T),1),
            sp_mean = round(mean(Q2_1_TB_SCORE,na.rm=T),4)*100,
            sp_sd = round(sd(Q2_1_TB_SCORE,na.rm=T),4)*100)
setDT(wk3)
wk3[, Npcalc_1pt := ceiling(((1.96^2)*(sp_sd^2))/(1^2))]
wk3[, Npcalc_2pts := ceiling(((1.96^2)*(sp_sd^2))/(2^2))]
wk3[, Npcalc_3pts := ceiling(((1.96^2)*(sp_sd^2))/(3^2))]
#group four weeks
sppa4 <- sppa[FSCL_WK_IN_YR_NUM>=14&FSCL_WK_IN_YR_NUM<=17, 
              list(Q2_1_TB_CNT = sum(Q2_1_TB_CNT,na.rm=T),
                   Q2_1_RESPONSE_TOTAL = sum(Q2_1_RESPONSE_TOTAL,na.rm=T),
                   Q2_1_TB_SCORE = sum(Q2_1_TB_CNT,na.rm=T)/sum(Q2_1_RESPONSE_TOTAL,na.rm=T)),
              by=c("STORE_NUM","FSCL_YR_NUM")]
#summarise mean and standard deviation
wk4 <- sppa4 %>% 
  group_by(FSCL_YR_NUM) %>% 
  summarise(Nstores = length(unique(STORE_NUM)),
            total_resp = sum(Q2_1_RESPONSE_TOTAL,na.rm=T),
            avg_store_resp = round(mean(Q2_1_RESPONSE_TOTAL,na.rm=T),1),
            sp_mean = round(mean(Q2_1_TB_SCORE,na.rm=T),4)*100,
            sp_sd = round(sd(Q2_1_TB_SCORE,na.rm=T),4)*100)
setDT(wk4)
wk4[, Npcalc_1pt := ceiling(((1.96^2)*(sp_sd^2))/(1^2))]
wk4[, Npcalc_2pts := ceiling(((1.96^2)*(sp_sd^2))/(2^2))]
wk4[, Npcalc_3pts := ceiling(((1.96^2)*(sp_sd^2))/(3^2))]


