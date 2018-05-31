#store-level indicators R/Tableau for Andrew

#load libaries
library(data.table)

#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ccso <- fread(paste0(data_dir,"/rtableau_ccso.csv"))
uplh <- fread(paste0(data_dir,"/rtableau_uplh.csv"))
cosd <- fread(paste0(data_dir,"/rtableau_cosd.csv"))
mop <- fread(paste0(data_dir,"/rtableau_mop.csv"))
sr <- fread(paste0(data_dir,"/rtableau_sr.csv"))

#cc/so - swing wide
ccso <- dcast.data.table(ccso, STORE_NUM ~ FSCL_YR_NUM, value.var=c("CC_R3M","SO_R3M"))
#yoy change
ccso[, CC_R3M_YOY := CC_R3M_2018-CC_R3M_2017]
ccso[, SO_R3M_YOY := SO_R3M_2018-SO_R3M_2017]
ccso <- ccso[, .(STORE_NUM,CC_R3M_2018,CC_R3M_YOY,SO_R3M_2018,SO_R3M_YOY)]
ccso <- na.omit(ccso)

#uplh
uplh <- uplh[, .(STORE_NUM,UPLH)]

#cosd
cosd <- dcast.data.table(cosd, FSCL_PER_IN_YR_NUM + STORE_NUMBER ~ FSCL_YR_NUM, value.var=c("Sales","COSD"))
cosd[, NET_SALES_PCT_YOY := round(Sales_2018/Sales_2017,2)]
cosd[, COSD_YOY := COSD_2018-COSD_2017]
cosd <- cosd[, .(STORE_NUMBER,Sales_2018,NET_SALES_PCT_YOY,COSD_2018,COSD_YOY)]
setnames(cosd,c("STORE_NUMBER","Sales_2018"),c("STORE_NUM","NET_SALES_2018"))
cosd <- na.omit(cosd)

#mop
mop <- dcast.data.table(mop, STORE_NUM ~ FSCL_YR_NUM, value.var=c("MOP_PCT"))
setnames(mop,c("2017","2018"),c("PCT_MOP_2017","PCT_MOP_2018"))
mop[, PCT_MOP_YOY := PCT_MOP_2018-PCT_MOP_2017]
mop <- mop[, .(STORE_NUM,PCT_MOP_2018,PCT_MOP_YOY)]
mop <- na.omit(mop)

#sr
setnames(sr,c("SR_PCT","SR_PCT_YOY_DELTA"),c("PCT_SR","PCT_SR_YOY_DELTA"))
sr[, PCT_SR := PCT_SR*100]
sr[, PCT_SR_YOY_DELTA := PCT_SR_YOY_DELTA*100]

#merge
full <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), all = TRUE)}, list(ccso,uplh,cosd,mop,sr))

#write.csv
write.csv(full,file=paste0(data_dir,"/CE_FUN_STORE_DATA.csv"))