##analyze CE scores by mobile screens
##see 12/15/17 emails from Lisa, Cambridge, and John Smith (Subject: Mobile Drop Downs)

#load libraries
library(data.table)
library(bit64)


##to get AVERAGE SCORES
#load data 
mtest <- fread("C:/Users/jumorris/ce101_12_18_17/ce101_12_18_17.csv")

#Recode MOBVERSION so blanks are 1's
mtest[, mob := ifelse(is.na(mobversion),1,mobversion)]

#create new version that splits movile from desktop/tablet
mtest[, mob3cat := ifelse(is.na(mobversion),3,mobversion)]

#convert 9's to NA
listofvars <- colnames(mtest)[c(28:36)]
mtest[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=listofvars]
mtest[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
mtest[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]


#tests
t.test(mtest[mob==1,q1],mtest[mob==2,q1])
t.test(mtest[mob==1,q2_1],mtest[mob==2,q2_1])
t.test(mtest[mob==1,q2_2],mtest[mob==2,q2_2])
t.test(mtest[mob==1,q2_3],mtest[mob==2,q2_3])
t.test(mtest[mob==1,q2_4],mtest[mob==2,q2_4])
t.test(mtest[mob==1,q2_5],mtest[mob==2,q2_5])
t.test(mtest[mob==1,q2_6],mtest[mob==2,q2_6])
t.test(mtest[mob==1,q2_7],mtest[mob==2,q2_7])
t.test(mtest[mob==1,q2_8],mtest[mob==2,q2_8])

#aggreate
mtemp <- mtest[, lapply(.SD, function(x) round(mean(x,na.rm=T),2)), by="mob", .SDcols=listofvars]
#melt and case
mtemp <- melt(mtemp,id="mob")
mtemp <- dcast.data.table(mtemp, variable ~ mob, value.var="value")
setnames(mtemp,c("1","2"),c("mob_old","mob_new"))
mtemp[, delta := mob_new-mob_old]




##to get RESPONSE RATE
#load data 
mtest2 <- fread("C:/Users/jumorris/ce101_12_18_17/ce101_12_18_17.csv")

#Recode MOBVERSION so blanks are 1's
mtest2[, mob := ifelse(is.na(mobversion),1,mobversion)]

#create new version that splits movile from desktop/tablet
mtest2[, mob3cat := ifelse(is.na(mobversion),3,mobversion)]

#convert 9's to NA
listofvars <- colnames(mtest2)[c(28:36)]
mtest2[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=listofvars]
mtest2[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
mtest2[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]


#tests
t.test(mtest2[mob==1,q1],mtest2[mob==2,q1])
t.test(mtest2[mob==1,q2_1],mtest2[mob==2,q2_1])
t.test(mtest2[mob==1,q2_2],mtest2[mob==2,q2_2])
t.test(mtest2[mob==1,q2_3],mtest2[mob==2,q2_3])
t.test(mtest2[mob==1,q2_4],mtest2[mob==2,q2_4])
t.test(mtest2[mob==1,q2_5],mtest2[mob==2,q2_5])
t.test(mtest2[mob==1,q2_6],mtest2[mob==2,q2_6])
t.test(mtest2[mob==1,q2_7],mtest2[mob==2,q2_7])
t.test(mtest2[mob==1,q2_8],mtest2[mob==2,q2_8])

#aggreate
mtemp2 <- mtest2[, lapply(.SD, function(x) round(mean(x,na.rm=T),2)), by="mob3cat", .SDcols=listofvars]
#melt and case
mtemp2 <- melt(mtemp2,id="mob3cat")
mtemp2 <- dcast.data.table(mtemp2, variable ~ mob3cat, value.var="value")
setnames(mtemp2,c("1","2","3"),c("mob_old","mob_new","desk_old"))
# setnames(mtemp2,c("1","2"),c("mob_old","mob_new"))
# mtemp2[, delta := mob_new-mob_old]


#aggreate TO GET SUMS
mtemp3 <- mtest2[, lapply(.SD, function(x) sum(x,na.rm=T)), by="mob3cat", .SDcols=listofvars]
#melt and case
mtemp3 <- melt(mtemp3,id="mob3cat")
mtemp3 <- dcast.data.table(mtemp3, variable ~ mob3cat, value.var="value")
setnames(mtemp3,c("1","2","3"),c("mob_old","mob_new","desk_old"))



##to get TOP BOX
#load data 
mtest4 <- fread("C:/Users/jumorris/ce101_12_15_17/ce101_12_15_17.csv")

#Recode MOBVERSION so blanks are 1's
mtest4[, mob := ifelse(is.na(mobversion),1,mobversion)]

#create new version that splits movile from desktop/tablet
mtest4[, mob3cat := ifelse(is.na(mobversion),3,mobversion)]

#convert 9's to NA
listofvars <- colnames(mtest4)[c(28:36)]
mtest4[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=listofvars]
mtest4[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
mtest4[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]

#tests
# t.test(mtest4[mob==1,q1],mtest4[mob==2,q1])
# t.test(mtest4[mob==1,q2_1],mtest4[mob==2,q2_1])
# t.test(mtest4[mob==1,q2_2],mtest4[mob==2,q2_2])
# t.test(mtest4[mob==1,q2_3],mtest4[mob==2,q2_3])
# t.test(mtest4[mob==1,q2_4],mtest4[mob==2,q2_4])
# t.test(mtest4[mob==1,q2_5],mtest4[mob==2,q2_5])
# t.test(mtest4[mob==1,q2_6],mtest4[mob==2,q2_6])
# t.test(mtest4[mob==1,q2_7],mtest4[mob==2,q2_7])
# t.test(mtest4[mob==1,q2_8],mtest4[mob==2,q2_8])

t.test(mtest4[mob==1,q1],mtest4[mob==2,q1])
t.test(mtest4[mob==1,q2_1],mtest4[mob==2,q2_1])
t.test(mtest4[mob==1,q2_2],mtest4[mob==2,q2_2])
t.test(mtest4[mob==1,q2_3],mtest4[mob==2,q2_3])
t.test(mtest4[mob==1,q2_4],mtest4[mob==2,q2_4])
t.test(mtest4[mob==1,q2_5],mtest4[mob==2,q2_5])
t.test(mtest4[mob==1,q2_6],mtest4[mob==2,q2_6])
t.test(mtest4[mob==1,q2_7],mtest4[mob==2,q2_7])
t.test(mtest4[mob==1,q2_8],mtest4[mob==2,q2_8])

#aggreate
mtemp4 <- mtest4[, lapply(.SD, function(x) round(mean(x,na.rm=T),2)), by="mob3cat", .SDcols=listofvars]
#melt and case
mtemp4 <- melt(mtemp4,id="mob3cat")
mtemp4 <- dcast.data.table(mtemp4, variable ~ mob3cat, value.var="value")
setnames(mtemp4,c("1","2","3"),c("mob_old","mob_new","desk_old"))
# setnames(mtemp4,c("1","2"),c("mob_old","mob_new"))
mtemp4[, del_mob_newtoold := mob_new - mob_old]
