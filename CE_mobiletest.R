##analyze CE scores by mobile screens
##see 12/15/17 emails from Lisa, Cambridge, and John Smith (Subject: Mobile Drop Downs)

#load libraries
library(data.table)
library(bit64)
set.seed(98115)


##to get AVERAGE SCORES
#load data 
mtest <- fread("C:/Users/jumorris/ce101_01_02_18/ce101_01_02_18.csv")

#pull in transaction count data from November 2017
mtrans <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE-mobiletest_transcount_byguid.csv")



#merge transaction data by guid
setnames(mtrans,"GUID_ID","guid")
mtest <- left_join(mtest,mtrans,by="guid")
setDT(mtest)

#Recode MOBVERSION so blanks are 1's
mtest[, mob := ifelse(is.na(mobversion),1,mobversion)]

#create new version that splits movile from desktop/tablet
#c("1","2","3"),c("mob_old","mob_new","desk_old")
mtest[, mob3cat := ifelse(is.na(mobversion),3,mobversion)]

#convert 9's to NA
listofvars <- colnames(mtest)[c(28:36)]
mtest[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=listofvars]
# mtest[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
# mtest[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]

# #tests
# t.test(mtest[mob==1,q1],mtest[mob==2,q1])
# t.test(mtest[mob==1,q2_1],mtest[mob==2,q2_1])
# t.test(mtest[mob==1,q2_2],mtest[mob==2,q2_2])
# t.test(mtest[mob==1,q2_3],mtest[mob==2,q2_3])
# t.test(mtest[mob==1,q2_4],mtest[mob==2,q2_4])
# t.test(mtest[mob==1,q2_5],mtest[mob==2,q2_5])
# t.test(mtest[mob==1,q2_6],mtest[mob==2,q2_6])
# t.test(mtest[mob==1,q2_7],mtest[mob==2,q2_7])
# t.test(mtest[mob==1,q2_8],mtest[mob==2,q2_8])

#aggregate for average scores
mtemp <- mtest[, lapply(.SD, function(x) round(mean(x,na.rm=T),2)), by="mob", .SDcols=listofvars]
#melt and case
mtemp <- melt(mtemp,id="mob")
mtemp <- dcast.data.table(mtemp, variable ~ mob, value.var="value")
setnames(mtemp,c("1","2"),c("mob_old","mob_new"))
mtemp[, delta := mob_new-mob_old]

#get score frequency
q1dt <- mtest %>% group_by(mob3cat) %>%
  count(q1) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q1dt)
setnames(q1dt,c("mob3cat","value","q1","q1pct"))
q21dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_1) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q21dt)
setnames(q21dt,c("mob3cat","value","q21","q21pct"))
q22dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_2) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q22dt)
setnames(q22dt,c("mob3cat","value","q22","q22pct"))
q23dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_3) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q23dt)
setnames(q23dt,c("mob3cat","value","q23","q23pct"))
q24dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_4) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q24dt)
setnames(q24dt,c("mob3cat","value","q24","q24pct"))
q25dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_5) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q25dt)
setnames(q25dt,c("mob3cat","value","q25","q25pct"))
q26dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_6) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q26dt)
setnames(q26dt,c("mob3cat","value","q26","q26pct"))
q27dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_7) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q27dt)
setnames(q27dt,c("mob3cat","value","q27","q27pct"))
q28dt <- mtest %>% group_by(mob3cat) %>%
  count(q2_8) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(q28dt)
setnames(q28dt,c("mob3cat","value","q28","q28pct"))

#get transaction frequency
#recode 15+ as 15
mtest[NOV17_TRAN>=15, NOV17_TRAN := 15]
#get frequncy
transdt <- mtest %>% group_by(mob3cat) %>%
  count(NOV17_TRAN) %>% mutate(pct = round(n/sum(n),3)*100)
setDT(transdt)

#break mob_new into 10 samples
randum <- runif(nrow(mtest),0,1)
mtest <- cbind(mtest,randum)
mtest <- mtest %>% mutate(sample = ntile(randum, 10))
setDT(mtest)

#aggregate for average scores by sample
rtemp <- mtest
rtemp[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
rtemp[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]
rtemp <- rtemp[, lapply(.SD, function(x) round(mean(x,na.rm=T),4)*100), by="sample", .SDcols=listofvars]
hist(rtemp[,q2_8])


#merge
newDT <- Reduce(function(x, y) {merge(x, y, by=c("mob3cat", "value"), all = TRUE)}, 
                list(q1dt,q21dt,q22dt,q23dt,q24dt,q25dt,q26dt,q27dt,q28dt))
write.csv(newDT,file="C:/Users/jumorris/mobile_test_scoredist.csv")

##to get RESPONSE RATE
#load data 
mtest2 <- fread("C:/Users/jumorris/ce101_01_02_18/ce101_01_02_18.csv")

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
#aggregate to get so
sotemp <- mtemp2[variable=="q2_1"|variable=="q2_3"|variable=="q2_4"|
                   variable=="q2_5"|variable=="q2_6"|variable=="q2_7", 
                 lapply(.SD,mean), .SDcols=colnames(mtemp2)[c(2:4)]]



#aggreate TO GET SUMS
mtemp3 <- mtest2[, lapply(.SD, function(x) sum(x,na.rm=T)), by="mob3cat", .SDcols=listofvars]
#melt and case
mtemp3 <- melt(mtemp3,id="mob3cat")
mtemp3 <- dcast.data.table(mtemp3, variable ~ mob3cat, value.var="value")
setnames(mtemp3,c("1","2","3"),c("mob_old","mob_new","desk_old"))
mtemp3[, total_old := mob_old + desk_old]
setcolorder(mtemp3,c("variable","total_old","mob_old","mob_new","desk_old"))
#aggregate to get so
sotemp <- mtemp3[variable=="q2_1"|variable=="q2_3"|variable=="q2_4"|
                   variable=="q2_5"|variable=="q2_6"|variable=="q2_7", 
                 lapply(.SD,sum), .SDcols=colnames(mtemp3)[c(2:5)]]






##to get TOP BOX
#load data 
mtest4 <- fread("C:/Users/jumorris/ce101_01_02_18/ce101_01_02_18.csv")

#Recode MOBVERSION so blanks are 1's
mtest4[, mob := ifelse(is.na(mobversion),1,mobversion)]

#create new version that splits movile from desktop/tablet
mtest4[, mob3cat := ifelse(is.na(mobversion),3,mobversion)]

#convert 9's to NA
listofvars <- colnames(mtest4)[c(28:36)]
mtest4[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=listofvars]
mtest4[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
mtest4[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]

#create so aggregate
mtest4[, q_so := (q2_1+q2_3+q2_4+q2_5+q2_6+q2_7)/6]

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

t.test(mtest4[mob3cat==1,q1],mtest4[mob3cat==2,q1])
t.test(mtest4[mob3cat==1,q2_1],mtest4[mob3cat==2,q2_1])
t.test(mtest4[mob3cat==1,q2_2],mtest4[mob3cat==2,q2_2])
t.test(mtest4[mob3cat==1,q2_3],mtest4[mob3cat==2,q2_3])
t.test(mtest4[mob3cat==1,q2_4],mtest4[mob3cat==2,q2_4])
t.test(mtest4[mob3cat==1,q2_5],mtest4[mob3cat==2,q2_5])
t.test(mtest4[mob3cat==1,q2_6],mtest4[mob3cat==2,q2_6])
t.test(mtest4[mob3cat==1,q2_7],mtest4[mob3cat==2,q2_7])
t.test(mtest4[mob3cat==1,q2_8],mtest4[mob3cat==2,q2_8])
t.test(mtest4[mob3cat==1,q_so],mtest4[mob3cat==2,q_so])

#aggreate for TOP BOX - old v new
mtemp4 <- mtest4[, lapply(.SD, function(x) round(mean(x,na.rm=T),5)*100), by="mob", .SDcols=listofvars]
#melt and case
mtemp4 <- melt(mtemp4,id="mob")
mtemp4 <- dcast.data.table(mtemp4, variable ~ mob, value.var="value")
setnames(mtemp4,c("1","2"),c("mob_old","mob_new"))

#aggregate to get so
sotemp <- mtemp4[variable=="q2_1"|variable=="q2_3"|variable=="q2_4"|
                   variable=="q2_5"|variable=="q2_6"|variable=="q2_7", 
                 lapply(.SD,mean), .SDcols=colnames(mtemp4)[c(2:3)]]
sotemp[, del_mob_newtoold := mob_new - mob_old]

#aggreate for TOP BOX - old mobile, new mobile, old desktop
mtemp4 <- mtest4[, lapply(.SD, function(x) round(mean(x,na.rm=T),5)*100), by="mob3cat", .SDcols=listofvars]
#melt and case
mtemp4 <- melt(mtemp4,id="mob3cat")
mtemp4 <- dcast.data.table(mtemp4, variable ~ mob3cat, value.var="value")
setnames(mtemp4,c("1","2","3"),c("mob_old","mob_new","desk_old"))
# setnames(mtemp4,c("1","2"),c("mob_old","mob_new"))
mtemp4[, del_mob_newtoold := mob_new - mob_old]

#aggregate to get so
sotemp <- mtemp4[variable=="q2_1"|variable=="q2_3"|variable=="q2_4"|
                   variable=="q2_5"|variable=="q2_6"|variable=="q2_7", 
                 lapply(.SD,mean), .SDcols=colnames(mtemp4)[c(2:5)]]
sotemp[, del_mob_newtoold := mob_new - mob_old]