##request for Brandon and Neha

#load libraries
library(data.table)
library(tidyverse)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
ce <- fread(paste0(data_dir,"/FY18Q2-3_forBrandon_CC-SO_allinds_daypart.csv"))

#create so flag
ce[QSTN_ID!='Q2_2', so_flag := 1]; ce[QSTN_ID=='Q2_2', so_flag := 0]
so <- ce[so_flag==1]
so <- so[, list(TB_SCORE = round(mean(TB_SCORE),3)),
         by=c("DAY_PART","FSCL_YR_NUM","STORE_NUM")]
so[, QSTN_ID := "St_Ops"]
#rbind
l = list(ce, so)
ce <- rbindlist(l, use.names=TRUE, fill=TRUE) 
ce[, so_flag := NULL]

#ntile
decs <- ce %>% group_by(QSTN_ID, DAY_PART) %>% mutate(Decile = ntile(TB_SCORE, 10))
setDT(decs)

#rename question variable
decs[QSTN_ID=="Q2_1", QSTN_NM := "Speed"]
decs[QSTN_ID=="Q2_2", QSTN_NM := "CustomerConnection"]
decs[QSTN_ID=="Q2_3", QSTN_NM := "AboveAndBeyond"]
decs[QSTN_ID=="Q2_4", QSTN_NM := "Accuracy"]
decs[QSTN_ID=="Q2_5", QSTN_NM := "BevTaste"]
decs[QSTN_ID=="Q2_6", QSTN_NM := "FoodTaste"]
decs[QSTN_ID=="Q2_7", QSTN_NM := "Cleanliness"]
decs[QSTN_ID=="St_Ops", QSTN_NM := "StoreOperations"]

#reduce
decs <- decs[, .(STORE_NUM,DAY_PART,QSTN_ID,QSTN_NM,Decile)]

#setorder
setorder(decs,STORE_NUM,DAY_PART,QSTN_ID)

#write.csv
write.csv(decs,paste0(data_dir,"/FY18Q2-3_CE_daypart_deciles.csv"),row.names=F)