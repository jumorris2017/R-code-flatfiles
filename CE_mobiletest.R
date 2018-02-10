##analyze CE scores by mobile screens
##see 12/15/17 emails from Lisa, Cambridge, and John Smith (Subject: Mobile Drop Downs)

#load libraries
library(data.table)
library(bit64)
set.seed(98115)


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
setnames(mtemp4,c("1","2"),c("total_old","mob_new"))

#aggregate to get so
sotemp4 <- mtemp4[variable=="q2_1"|variable=="q2_3"|variable=="q2_4"|
                    variable=="q2_5"|variable=="q2_6"|variable=="q2_7", 
                  lapply(.SD,mean), .SDcols=colnames(mtemp4)[c(2:3)]]
#sotemp4[, del_mob_newtoold := mob_new - total_old]
sotemp4[, variable := 'so']
#for r/c-binding, drop mob_new
mtemp4[, mob_new := NULL]
sotemp4[, mob_new := NULL]

#aggreate for TOP BOX - old mobile, new mobile, old desktop
mtemp5 <- mtest4[, lapply(.SD, function(x) round(mean(x,na.rm=T),5)*100), by="mob3cat", .SDcols=listofvars]
#melt and case
mtemp5 <- melt(mtemp5,id="mob3cat")
mtemp5 <- dcast.data.table(mtemp5, variable ~ mob3cat, value.var="value")
setnames(mtemp5,c("1","2","3"),c("mob_old","mob_new","desk_old"))
# setnames(mtemp5,c("1","2"),c("mob_old","mob_new"))
mtemp5[, del_mob_newtoold := mob_old - mob_new]

#aggregate to get so
sotemp5 <- mtemp5[variable=="q2_1"|variable=="q2_3"|variable=="q2_4"|
                    variable=="q2_5"|variable=="q2_6"|variable=="q2_7", 
                  lapply(.SD,mean), .SDcols=colnames(mtemp5)[c(2:5)]]
sotemp5[, del_mob_newtoold := mob_old - mob_new]
sotemp5[, variable := 'so']

#cbind
mtemp6 <- merge(mtemp4,mtemp5,by="variable")
sotemp6 <- merge(sotemp4,sotemp5,by="variable")
#bind together
l = list(mtemp6,sotemp6)
fulltemp <- rbindlist(l, use.names=T,fill=T)
#round
fulltemp[, (colnames(fulltemp)[2:ncol(fulltemp)]) := lapply(.SD, function(x) round(x,2)), .SDcols=colnames(fulltemp)[2:ncol(fulltemp)]]

#remove delta for plotting
ftemp <- copy(fulltemp)
ftemp[, del_mob_newtoold := NULL]
#melt
ftemp <- melt(ftemp,id.var="variable",
              variable.name = "version",
              value.name = "tbscore")
#relabel values so they can be ordered in plot
ftemp[variable=="q1", var2 := "01-q1"]
ftemp[variable=="q2_2", var2 := "02-q2_2"]
ftemp[variable=="so", var2 := "03-so"]
ftemp[variable=="q2_1", var2 := "04-q2_1"]
ftemp[variable=="q2_3", var2 := "05-q2_3"]
ftemp[variable=="q2_4", var2 := "06-q2_4"]
ftemp[variable=="q2_5", var2 := "07-q2_5"]
ftemp[variable=="q2_6", var2 := "08-q2_6"]
ftemp[variable=="q2_7", var2 := "09-q2_7"]
ftemp[variable=="q2_8", var2 := "10-q2_8"]

#setorder
setorder(ftemp,var2)

#remove delta for plotting
delta <- fulltemp[, .(variable,del_mob_newtoold)]
delta[, del_mob_newtoold := round(del_mob_newtoold,1)]
delta[, version := "mob_new"]
#merge
ftemp <- merge(ftemp,delta,all.x=T,by=c("variable","version"))
# 
# #set labels
# xlabel <- "CE Survey Question"
# xlabels <- c("Likelihood to Return","Customer Connection", "Store Operations", "Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness","Worth")
# ylabel <- "Top Box Score"
# tlabel <- "Customer Experience Survey"
# tlabel <- "Mobile UI Test"
# caption <- "Fielded December FY18"
# #manual legend labels
# lname <- "Survey Version"
# llabels <- c("Old Mobile & Desktop/Tablet", "Old (Mobile)", "New (Mobile)", "Desktop/Tablet")
# #values
# pdata4 <- ftemp
# px4 <- ftemp[,var2]
# py4 <- ftemp[,tbscore]
# groupvar4 <- ftemp[,version]
# #plot itself
# plot4 <- ggplot(data=pdata4,aes(y=py4,x=as.factor(px4),fill=as.factor(groupvar4))) +
#   geom_bar(stat="identity", width = 0.7, position=position_dodge(), colour="black") +
#   scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
#   scale_x_discrete(name="",labels=xlabels) +
#   ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
#   geom_text(size = 2.5, aes(label=py4,y=0), angle=90, hjust=-0.25, stat="identity", position = position_dodge(0.7))
# print(plot4)

#manually add significance 
ftemp[variable=="q2_2"&version=="mob_new", delsig := "***"]
ftemp[variable=="q2_3"&version=="mob_new", delsig := "***"]
ftemp[variable=="q2_4"&version=="mob_new", delsig := "***"]
ftemp[variable=="q2_5"&version=="mob_new", delsig := "***"]
ftemp[variable=="q2_6"&version=="mob_new", delsig := "*"]
ftemp[variable=="q2_8"&version=="mob_new", delsig := "***"]
ftemp[variable=="so"&version=="mob_new", delsig := "**"]
#paste together
ftemp[!is.na(delsig), delwithsig := paste0(del_mob_newtoold,delsig)]
ftemp[is.na(delsig), delwithsig := as.character(del_mob_newtoold)]

#set labels
xlabel <- "CE Survey Question"
xlabels <- c("Likelihood to Return","Customer Connection", "Store Ops", "Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness","Worth")
ylabel <- "Top Box Score"
sublabel <- "Customer Experience Survey"
tlabel <- "Mobile Screen Test"
caption <- "p<.05*,  p<.01**,  p<.001***\nOld (N) = 358,401 (90%),  New (N) = 39,782 (10%)\nFielded December 14 - January 1 FY18"
#manual legend labels
lname <- "Mobile Version"
llabels <- c("Old", "New")
#values
pdata4 <- ftemp[version=="mob_old"|version=="mob_new"]
px4 <- ftemp[version=="mob_old"|version=="mob_new",var2]
py4 <- ftemp[version=="mob_old"|version=="mob_new",tbscore]
groupvar4 <- ftemp[version=="mob_old"|version=="mob_new",version]
deltavar <- ftemp[version=="mob_old"|version=="mob_new",delwithsig]
#plot itself
plot4 <- ggplot(data=pdata4,aes(y=py4,x=as.factor(px4),fill=as.factor(groupvar4))) +
  geom_bar(stat="identity", width = 0.9, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,pdata4[,max(py4)]*1.2)) +
  theme(axis.text=element_text(size=8)) +
  ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 4.5, aes(label=deltavar, vjust=-1.25)) +
  geom_text(size = 4, aes(label=round(py4,1),y=0), vjust=-0.5, stat="identity", position = position_dodge(0.8))
print(plot4)



##read in comparative data
mcomp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_mobiletest_nov30-dec11.csv")
#add fake row for calculating total response counts
mcomp[, TOTAL_RESP := 1]

#break GUIDs into 10 samples
randum <- runif(nrow(mcomp),0,1)
mcomp <- cbind(mcomp,randum)
mcomp <- mcomp %>% mutate(sample = ntile(randum, 10))
setDT(mcomp)
# 
# #calculate tb scores for each of the 10 samples
# mcomp <- mcomp[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
#                       TOTAL_RESP = sum(TOTAL_RESP,na.rm=T),
#                       tbscore = ((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RESP,na.rm=T))*100)),
#                by=c("QSTN_ID","sample")]
# mcomp[, tbscore := round(tbscore,1)]
# #make histograms of each question (including SO)

# #for sample sizes
# mcompsamp[QSTN_ID=="Q1",sum(TOTAL_RESP)]
# mcompsamp[QSTN_ID=="Q1",mean(TOTAL_RESP)]
# 
# #set labels
# xlabel <- "Top Box Score"
# ylabel <- "Number of Stores"
# tlabel <- "Likelihood to Return"
# sublabel <- "Top Box Scores across Samples"
# caption <- "Total (N) = 293,068,  Each sample (N) = 29,307 (10%)\nNov 30 - Dec 11 FY18"
# #data
# pdata <- mcompsamp[QSTN_ID=="Q1"]
# pval <- mcompsamp[QSTN_ID=="Q1",tbscore]
# #plot itself
# plot2 <- ggplot(pdata,aes(pval)) + 
#   geom_histogram(binwidth=0.2,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
#   theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
#   scale_x_continuous(limits=c(42.2,44.2), breaks = scales::pretty_breaks(10)) +
#   xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
# print(plot2)
# 
# 
# #for sample sizes
# mcompsamp[QSTN_ID=="Q2_2",sum(TOTAL_RESP)]
# mcompsamp[QSTN_ID=="Q2_2",mean(TOTAL_RESP)]
# 
# #set labels
# xlabel <- "Top Box Score"
# ylabel <- "Number of Stores"
# tlabel <- "Customer Connection"
# sublabel <- "Top Box Scores across Samples"
# caption <- "Total (N) = 253,289,  Each sample (N) = 25,329 (10%)\nNov 30 - Dec 11 FY18"
# #data
# pdata <- mcompsamp[QSTN_ID=="Q2_2"]
# pval <- mcompsamp[QSTN_ID=="Q2_2",tbscore]
# #plot itself
# plot2 <- ggplot(pdata,aes(pval)) + 
#   geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
#   theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
#   scale_x_continuous(limits=c(30.7,31.9), breaks = scales::pretty_breaks(11)) +
#   xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
# print(plot2)



#calculate tb scores for each of the 10 samples
mcompfull <- mcomp[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                          TOTAL_RESP = sum(TOTAL_RESP,na.rm=T),
                          tbscorefullsamp = ((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RESP,na.rm=T))*100)),
                   by=c("QSTN_ID")]
mcompfull[, tbscorefullsamp := round(tbscorefullsamp,1)]

##calculate delta from total mean
#calculate tb scores for each of the 10 samples
mcompsamp <- mcomp[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                      TOTAL_RESP = sum(TOTAL_RESP,na.rm=T),
                      tbscore = ((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RESP,na.rm=T))*100)),
               by=c("QSTN_ID","sample")]
mcompsamp[, tbscore := round(tbscore,1)]
#merge in full sample tb scores
mcompsamp <- merge(mcompsamp,mcompfull[,.(QSTN_ID,tbscorefullsamp)],by=c("QSTN_ID"))
#calculate delta
mcompsamp[, tbdelta := tbscore-tbscorefullsamp]


#for sample sizes
mcompsamp[QSTN_ID=="Q1",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q1",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q1",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Likelihood to Return"
sublabel <- "Deltas in Sample Top Box Scores from Full Sample"
caption <- "Total (N) = 293,068,  Each sample (N) = 29,307 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q1"]
pval <- mcompsamp[QSTN_ID=="Q1",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.6,0.4), breaks = scales::pretty_breaks(10)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)
 

#for sample sizes
mcompsamp[QSTN_ID=="Q2_2",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_2",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_2",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Customer Connection"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 293,068,  Each sample (N) = 29,307 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_2"]
pval <- mcompsamp[QSTN_ID=="Q2_2",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.6,0.6), breaks = scales::pretty_breaks(12)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)


#for sample sizes
mcompsamp[QSTN_ID=="Q2_1",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_1",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_1",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Speed"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 286,734,  Each sample (N) = 28,673 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_1"]
pval <- mcompsamp[QSTN_ID=="Q2_1",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.5,0.5), breaks = scales::pretty_breaks(11)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)


#for sample sizes
mcompsamp[QSTN_ID=="Q2_3",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_3",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_3",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Above & Beyond"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 274,269,  Each sample (N) = 27,427 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_3"]
pval <- mcompsamp[QSTN_ID=="Q2_3",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.6,0.7), breaks = scales::pretty_breaks(13)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)


#for sample sizes
mcompsamp[QSTN_ID=="Q2_4",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_4",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_4",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Order Accuracy"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 291,110,  Each sample (N) = 29,111 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_4"]
pval <- mcompsamp[QSTN_ID=="Q2_4",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.3,0.5), breaks = scales::pretty_breaks(9)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)


#for sample sizes
mcompsamp[QSTN_ID=="Q2_5",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_5",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_5",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Beverage Taste"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 284,828,  Each sample (N) = 28,483 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_5"]
pval <- mcompsamp[QSTN_ID=="Q2_5",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.5,0.5), breaks = scales::pretty_breaks(11)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#for sample sizes
mcompsamp[QSTN_ID=="Q2_6",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_6",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_6",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Food Taste"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 116,808,  Each sample (N) = 11,681 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_6"]
pval <- mcompsamp[QSTN_ID=="Q2_6",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.9,0.9), breaks = scales::pretty_breaks(19)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#for sample sizes
mcompsamp[QSTN_ID=="Q2_7",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_7",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_7",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Cleanliness"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 219,314,  Each sample (N) = 21,931 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_7"]
pval <- mcompsamp[QSTN_ID=="Q2_7",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.5,0.8), breaks = scales::pretty_breaks(14)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#for sample sizes
mcompsamp[QSTN_ID=="Q2_8",sum(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_8",mean(TOTAL_RESP)]
mcompsamp[QSTN_ID=="Q2_8",mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Worth Perceptions"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total (N) = 291,787,  Each sample (N) = 29,179 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- mcompsamp[QSTN_ID=="Q2_8"]
pval <- mcompsamp[QSTN_ID=="Q2_8",tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.6,0.4), breaks = scales::pretty_breaks(11)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)


#calculate SO
#make so indicator
mcompsamp[QSTN_ID=="Q2_1"|QSTN_ID=="Q2_3"|QSTN_ID=="Q2_4"|QSTN_ID=="Q2_5"|QSTN_ID=="Q2_6"|QSTN_ID=="Q2_7", soflag := 1]

#calculate tb scores for each of the 10 samples
socompfull <- mcompsamp[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                          TOTAL_RESP = sum(TOTAL_RESP,na.rm=T),
                          tbscorefullsamp = ((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RESP,na.rm=T))*100)),
                   by=c("soflag")]
socompfull[, tbscorefullsamp := round(tbscorefullsamp,1)]
socompfull <- na.omit(socompfull,cols="soflag")
##calculate delta from total mean
#calculate tb scores for each of the 10 samples
socompsamp <- mcompsamp[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                          TOTAL_RESP = sum(TOTAL_RESP,na.rm=T),
                          tbscore = ((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RESP,na.rm=T))*100)),
                   by=c("soflag","sample")]
socompsamp[, tbscore := round(tbscore,1)]
socompsamp <- na.omit(socompsamp,cols="soflag")
#merge in full sample tb scores
socompsamp <- merge(socompsamp,socompfull[,.(soflag,tbscorefullsamp)],by=c("soflag"))
#calculate delta
socompsamp[, tbdelta := tbscore-tbscorefullsamp]

#for sample sizes
socompsamp[,sum(TOTAL_RESP)]
socompsamp[,mean(TOTAL_RESP)]
socompsamp[,mean(abs(tbdelta))]
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Store Operations"
sublabel <- "Deltas for Sample Top Box Scores from Topline"
caption <- "Total Resp (N) = 1,473,063,  Each sample (N) = 147,306 (10%)\nNov 30 - Dec 11 FY18"
#data
pdata <- socompsamp
pval <- socompsamp[,tbdelta]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-0.3,0.4), breaks = scales::pretty_breaks(9)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)














##to get AVERAGE SCORES
#load data 
mtest <- fread("C:/Users/jumorris/ce101_01_02_18/ce101_01_02_18.csv")

# #pull in transaction count data from November 2017
# mtrans <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE-mobiletest_transcount_byguid.csv")
# 
# #merge transaction data by guid
# setnames(mtrans,"GUID_ID","guid")
# mtest <- left_join(mtest,mtrans,by="guid")
# setDT(mtest)

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
hist(rtemp[,q2_2])
#set labels
xlabel <- "Top Box Score"
ylabel <- "Number of Samples"
tlabel <- "Customer Connection"
sublabel <- "Sample of Top Box Scores from Old Mobile Screen Test"
caption <- "Total (N) = 311,218,  Each sample (N) = 31,122 (10%)\nDec 14 - Jan 1 FY18"
#data
pdata <- rtemp
pval <- rtemp[,q2_2]
#plot itself
plot2 <- ggplot(pdata,aes(pval)) +
  geom_histogram(binwidth=0.1,show.legend=FALSE,fill="lightgrey",col=I("black")) +
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(30.2,31.3), breaks = scales::pretty_breaks(12)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)









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
write.csv(mtemp2,file="C:/Users/jumorris/mobile_test_TBscores.csv")


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
write.csv(mtemp3,file="C:/Users/jumorris/mobile_test_responseNs.csv")










