#calculate rolling 3 month CC values from period values
library(data.table)

#set path
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir,"/DC_CC_daypartcheck_st101.csv"))
ce <- setorder(ce,FSCL_YR_NUM,FSCL_PER_IN_YR_NUM,DAY_PART)
ce <- ce[FSCL_YR_NUM==2018|FSCL_YR_NUM==2017|(FSCL_YR_NUM==2016&FSCL_PER_IN_YR_NUM>=9)]

#lag twice to get rolling 3
ce[, lag_TB :=lapply(.SD, function(x) c(NA, x[-.N])), by="DAY_PART", .SDcols="TOTAL_TB"]
ce[, lag_RSPNS :=lapply(.SD, function(x) c(NA, x[-.N])), by="DAY_PART", .SDcols="TOTAL_RSPNS"]
ce[, lag2_TB :=lapply(.SD, function(x) c(NA, x[-.N])), by="DAY_PART", .SDcols="lag_TB"]
ce[, lag2_RSPNS :=lapply(.SD, function(x) c(NA, x[-.N])), by="DAY_PART", .SDcols="lag_RSPNS"]
#sum together
ce[, R3MTB := rowSums(.SD, na.rm = TRUE), .SDcols=c("TOTAL_TB","lag_TB","lag2_TB")]
ce[, R3MRSPNS := rowSums(.SD, na.rm = TRUE), .SDcols=c("TOTAL_RSPNS","lag_RSPNS","lag2_RSPNS")]

#calculate TB
ce[, R3M_CC := round(R3MTB/R3MRSPNS,3)]
ce2 <- ce[, .(DAY_PART,FSCL_YR_NUM,FSCL_PER_IN_YR_NUM,R3MTB,R3MRSPNS,R3M_CC)]
write.csv(ce2,paste0(data_dir,"/DC_CC_daypartcheck_st101_results.csv"))