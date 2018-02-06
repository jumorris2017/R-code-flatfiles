##CC by daypart and store
##identifying minimum timeframe
##ideal cell minimum = 70 CE responses

#load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(xlsx)

#load data
cs0q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2months.csv") #2months
cs1q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_1quarter.csv") #1 quarter
cs2q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters.csv") #2 quarters
cs3q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_3quarters.csv") #3 quarters
cs4q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_4quarters.csv") #4 quarters
cs8q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_8quarters.csv") #8 quarters
#agg at the store level
cs0q <- cs0q[STORE_NUM %in% unique(cs0q[,STORE_NUM])&STORE_NUM %in% unique(cs8q[,STORE_NUM]), list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","DAY_PART")]
cs1q <- cs1q[STORE_NUM %in% unique(cs0q[,STORE_NUM])&STORE_NUM %in% unique(cs8q[,STORE_NUM]), list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","DAY_PART")]
cs2q <- cs2q[STORE_NUM %in% unique(cs0q[,STORE_NUM])&STORE_NUM %in% unique(cs8q[,STORE_NUM]), list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
            TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
            CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
     by=c("STORE_NUM","QSTN_ID","DAY_PART")]
cs3q <- cs3q[STORE_NUM %in% unique(cs0q[,STORE_NUM])&STORE_NUM %in% unique(cs8q[,STORE_NUM]), list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","DAY_PART")]
cs4q <- cs4q[STORE_NUM %in% unique(cs0q[,STORE_NUM])&STORE_NUM %in% unique(cs8q[,STORE_NUM]), list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","DAY_PART")]
cs8q <- cs8q[STORE_NUM %in% unique(cs0q[,STORE_NUM])&STORE_NUM %in% unique(cs8q[,STORE_NUM]), list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","DAY_PART")]

# #calculate percent of stores with survey counts over 70, by daypart
# cs2q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs2q[TOTAL_RSPNS<70, resp_over70 := 0]
# cs3q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs3q[TOTAL_RSPNS<70, resp_over70 := 0]
# cs4q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs4q[TOTAL_RSPNS<70, resp_over70 := 0]
# cs8q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs8q[TOTAL_RSPNS<70, resp_over70 := 0]
# 
# #calculate percent of stores with survey counts over 70, by daypart
# cs2q[TOTAL_RSPNS>=60, resp_over60 := 1]; cs2q[TOTAL_RSPNS<60, resp_over60 := 0]
# cs3q[TOTAL_RSPNS>=60, resp_over60 := 1]; cs3q[TOTAL_RSPNS<60, resp_over60 := 0]
# cs4q[TOTAL_RSPNS>=60, resp_over60 := 1]; cs4q[TOTAL_RSPNS<60, resp_over60 := 0]
# cs8q[TOTAL_RSPNS>=60, resp_over60 := 1]; cs8q[TOTAL_RSPNS<60, resp_over60 := 0]

#calculate percent of stores with survey counts over 50, by daypart
cs0q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs0q[TOTAL_RSPNS<50, resp_over50 := 0]
cs1q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs1q[TOTAL_RSPNS<50, resp_over50 := 0]
cs2q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs2q[TOTAL_RSPNS<50, resp_over50 := 0]
cs3q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs3q[TOTAL_RSPNS<50, resp_over50 := 0]
cs4q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs4q[TOTAL_RSPNS<50, resp_over50 := 0]
cs8q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs8q[TOTAL_RSPNS<50, resp_over50 := 0]
# 
# #percent of stores
# #two quarters
# temp2 <- cs2q %>%
#   group_by(DAY_PART,resp_over70) %>%
#   summarise (storeN70 = n()) %>%
#   mutate(pct70 = round(storeN70 / sum(storeN70),3)*100)
# setDT(temp2)
# setorder(temp2,resp_over70,DAY_PART)
# temp2[, quarters := 2]
# #three quarters
# temp3 <- cs3q %>%
#   group_by(DAY_PART,resp_over70) %>%
#   summarise (storeN70 = n()) %>%
#   mutate(pct70 = round(storeN70 / sum(storeN70),3)*100)
# setDT(temp3)
# setorder(temp3,resp_over70,DAY_PART)
# temp3[, quarters := 3]
# #4 quarters
# temp4 <- cs4q %>%
#   group_by(DAY_PART,resp_over70) %>%
#   summarise (storeN70 = n()) %>%
#   mutate(pct70 = round(storeN70 / sum(storeN70),3)*100)
# setDT(temp4)
# setorder(temp4,resp_over70,DAY_PART)
# temp4[, quarters := 4]
# #8 quarters
# temp8 <- cs8q %>%
#   group_by(DAY_PART,resp_over70) %>%
#   summarise (storeN70 = n()) %>%
#   mutate(pct70 = round(storeN70 / sum(storeN70),3)*100)
# setDT(temp8)
# setorder(temp8,resp_over70,DAY_PART)
# temp8[, quarters := 8]
# #descriptives
# t2 <- temp2[resp_over70==1]
# t3 <- temp3[resp_over70==1]
# t4 <- temp4[resp_over70==1]
# t8 <- temp8[resp_over70==1]
# t70 <- rbind(t2,t3,t4,t8)
# setDT(t70)
# 
# #percent of stores
# #two quarters
# temp2 <- cs2q %>%
#   group_by(DAY_PART,resp_over60) %>%
#   summarise (storeN60 = n()) %>%
#   mutate(pct60 = round(storeN60 / sum(storeN60),3)*100)
# setDT(temp2)
# setorder(temp2,resp_over60,DAY_PART)
# temp2[, quarters := 2]
# #three quarters
# temp3 <- cs3q %>%
#   group_by(DAY_PART,resp_over60) %>%
#   summarise (storeN60 = n()) %>%
#   mutate(pct60 = round(storeN60 / sum(storeN60),3)*100)
# setDT(temp3)
# setorder(temp3,resp_over60,DAY_PART)
# temp3[, quarters := 3]
# #4 quarters
# temp4 <- cs4q %>%
#   group_by(DAY_PART,resp_over60) %>%
#   summarise (storeN60 = n()) %>%
#   mutate(pct60 = round(storeN60 / sum(storeN60),3)*100)
# setDT(temp4)
# setorder(temp4,resp_over60,DAY_PART)
# temp4[, quarters := 4]
# #8 quarters
# temp8 <- cs8q %>%
#   group_by(DAY_PART,resp_over60) %>%
#   summarise (storeN60 = n()) %>%
#   mutate(pct60 = round(storeN60 / sum(storeN60),3)*100)
# setDT(temp8)
# setorder(temp8,resp_over60,DAY_PART)
# temp8[, quarters := 8]
# #descriptives
# t2 <- temp2[resp_over60==1]
# t3 <- temp3[resp_over60==1]
# t4 <- temp4[resp_over60==1]
# t8 <- temp8[resp_over60==1]
# t60 <- rbind(t2,t3,t4,t8)
# setDT(t60)

#percent of stores
#two months
temp0 <- cs0q %>%
  group_by(DAY_PART,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp0)
setorder(temp0,resp_over50,DAY_PART)
temp0[, quarters := 0]
#one quarter
temp1 <- cs1q %>%
  group_by(DAY_PART,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp1)
setorder(temp1,resp_over50,DAY_PART)
temp1[, quarters := 1]
#two quarters
temp2 <- cs2q %>%
  group_by(DAY_PART,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp2)
setorder(temp2,resp_over50,DAY_PART)
temp2[, quarters := 2]
#three quarters
temp3 <- cs3q %>%
  group_by(DAY_PART,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp3)
setorder(temp3,resp_over50,DAY_PART)
temp3[, quarters := 3]
#4 quarters
temp4 <- cs4q %>%
  group_by(DAY_PART,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp4)
setorder(temp4,resp_over50,DAY_PART)
temp4[, quarters := 4]
#8 quarters
temp8 <- cs8q %>%
  group_by(DAY_PART,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp8)
setorder(temp8,resp_over50,DAY_PART)
temp8[, quarters := 8]
#descriptives
t0 <- temp0[resp_over50==1]
t1 <- temp1[resp_over50==1]
t2 <- temp2[resp_over50==1]
t3 <- temp3[resp_over50==1]
t4 <- temp4[resp_over50==1]
t8 <- temp8[resp_over50==1]
t50 <- rbind(t0,t1,t2,t3,t4)
setDT(t50)
# 
# #merge
# tmat <- merge(t70,t60,by=c("DAY_PART","quarters"))
# tmat <- merge(tmat,t50,by=c("DAY_PART","quarters"))
# # #write to .xls
# # write.xlsx(tmat,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_storecounts.xlsx")
# 
# #melt for plotting
# tmelt <- tmat[, .(DAY_PART,quarters,storeN70,pct70)]
# tmelt <- melt(tmelt, id=c("DAY_PART","quarters"))
# 
# #set labels
# # xlabel <- "Proven SR"
# xlabels <- c("Early AM","AM","Midday","PM","Late PM")
# ylabel <- "CC Score"
# sublabel <- "Percent of Stores meeting Mimum Threshold at Varying Intervals"
# tlabel <- "CE Survey Responses by Day Part"
# caption <- "Miminum Threshold = 70 Surveys"
# #manual legend labels
# lname <- "Months"
# llabels <- c("2","3","6","9","12","24")
# #values
# pdata1a <- tmat
# px1a <- tmat[,DAY_PART]
# py1a <- tmat[,pct70]
# groupvar1a <- tmat[,quarters]
# nvar1a <- tmat[,storeN70]
# #plot itself
# plot2 <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
#   geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
#   scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
#   scale_x_discrete(name="",labels=xlabels) +
#   xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
#   geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
#   geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
# print(plot2)
# 
# #melt for plotting
# tmelt <- tmat[, .(DAY_PART,quarters,storeN60,pct60)]
# tmelt <- melt(tmelt, id=c("DAY_PART","quarters"))
# 
# #set labels
# # xlabel <- "Proven SR"
# xlabels <- c("Early AM","AM","Midday","PM","Late PM")
# ylabel <- "CC Score"
# sublabel <- "Percent of Stores meeting Mimum Threshold at Varying Intervals"
# tlabel <- "CE Survey Responses by Day Part"
# caption <- "Miminum Threshold = 60 Surveys"
# #manual legend labels
# lname <- "Months"
# llabels <- c("6","9","12","24")
# #values
# pdata1a <- tmat
# px1a <- tmat[,DAY_PART]
# py1a <- tmat[,pct60]
# groupvar1a <- tmat[,quarters]
# nvar1a <- tmat[,storeN60]
# #plot itself
# plot2 <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
#   geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
#   scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
#   scale_x_discrete(name="",labels=xlabels) +
#   xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
#   geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
#   geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
# print(plot2)

#melt for plotting
# tmelt <- tmat[, .(DAY_PART,quarters,storeN50,pct50)]
# tmelt <- melt(tmelt, id=c("DAY_PART","quarters"))

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "CC Score"
sublabel <- "Percent of Stores meeting Mimum Threshold at Varying Intervals"
tlabel <- "CE Survey Responses by Day Part"
caption <- "Miminum Threshold = 50 Surveys"
#manual legend labels
lname <- "Months"
llabels <- c("2","3","6","9","12")
#values
pdata1a <- t50
px1a <- t50[,DAY_PART]
py1a <- t50[,pct50]
groupvar1a <- t50[,quarters]
nvar1a <- t50[,storeN50]
#plot itself
plot2 <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot2)


##SELECT A "TYPICAL" STORE
#load data
st10404 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_store10404.csv") #2 quarters
#prep data
st10404[, CC_SCORE := round(CC_SCORE*100,1)]

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "CC Score"
sublabel <- "Store 10404"
tlabel <- "Customer Connection by Day Part"
caption <- "CE Survey Responses from Q4FY17 and Q1FY18 for Store 10404"
#manual legend labels
lname <- "Quarter"
llabels <- c("Q4FY17","Q1FY18")
#values
pdata1a <- st10404
px1a <- st10404[,DAY_PART]
py1a <- st10404[,CC_SCORE]
groupvar1a <- st10404[,FSCL_YR_NUM]
nvar1a <- st10404[,TOTAL_RSPNS ]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)



##SELECT A "TYPICAL" STORE
#load data
st10404 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_store10404.csv") #2 quarters
#create a binary day part indicator for pre-11am and post-11am
st10404[DAY_PART<=3, post2pm := 0]; st10404[DAY_PART>=4, post2pm := 1]
st10404 <- st10404[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
         by=c("STORE_NUM","FSCL_YR_NUM","post2pm")]

#prep data
st10404[, CC_SCORE := round(CC_SCORE*100,1)]

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM - Midday","PM - Late PM")
ylabel <- "CC Score"
sublabel <- "Store 10404"
tlabel <- "Customer Connection by Day Part"
caption <- "CE Survey Responses from Q4FY17 and Q1FY18 for Store 10404"
#manual legend labels
lname <- "Quarter"
llabels <- c("Q4FY17","Q1FY18")
#values
pdata1a <- st10404
px1a <- st10404[,post2pm]
py1a <- st10404[,CC_SCORE]
groupvar1a <- st10404[,FSCL_YR_NUM]
nvar1a <- st10404[,TOTAL_RSPNS ]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)



##SELECT A "TYPICAL" STORE
#load data
st10038 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_store10038.csv") #2 quarters
#prep data
st10038[, CC_SCORE := round(CC_SCORE*100,1)]

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "CC Score"
sublabel <- "Store 10038"
tlabel <- "Customer Connection by Day Part"
caption <- "CE Survey Responses from Q4FY17 and Q1FY18 for Store 10038"
#manual legend labels
lname <- "Quarter"
llabels <- c("Q4FY17","Q1FY18")
#values
pdata1a <- st10038
px1a <- st10038[,DAY_PART]
py1a <- st10038[,CC_SCORE]
groupvar1a <- st10038[,FSCL_YR_NUM]
nvar1a <- st10038[,TOTAL_RSPNS ]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)




#load data
st10063 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_store10063.csv") #2 quarters
#prep data
st10063[, CC_SCORE := round(CC_SCORE*100,1)]

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "CC Score"
sublabel <- "Store 10063"
tlabel <- "Customer Connection by Day Part"
caption <- "CE Survey Responses from Q4FY17 and Q1FY18 for Store 10063"
#manual legend labels
lname <- "Quarter"
llabels <- c("Q4FY17","Q1FY18")
#values
pdata1a <- st10063
px1a <- st10063[,DAY_PART]
py1a <- st10063[,CC_SCORE]
groupvar1a <- st10063[,FSCL_YR_NUM]
nvar1a <- st10063[,TOTAL_RSPNS ]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)




#ROLLING 3, 6, AND 12 FOR ONE STORE
st349_3 <- st349[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1]
st349_3 <- st349_3[, .(STORE_NUM,QSTN_ID,DAY_PART,TOTAL_TB,TOTAL_RSPNS,CC_SCORE)]
st349_3[, months := 3]
st349_6 <- st349[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                          TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                          CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
                   by=c("STORE_NUM","QSTN_ID","DAY_PART")]
st349_6 <- st349_6[, .(STORE_NUM,QSTN_ID,DAY_PART,TOTAL_TB,TOTAL_RSPNS,CC_SCORE)]
st349_6[, CC_SCORE := round(CC_SCORE*100,1)]
st349_6[, months := 6]
st349_12 <- cs4q[STORE_NUM==349]
st349_12 <- st349_12[, .(STORE_NUM,QSTN_ID,DAY_PART,TOTAL_TB,TOTAL_RSPNS,CC_SCORE)]
st349_12[, CC_SCORE := round(CC_SCORE*100,1)]
st349_12[, months := 12]
st349roll <- rbind(st349_3,st349_6,st349_12)

#set labels
xlabel <- "Day Part"
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "CC Score"
sublabel <- "Store 349"
tlabel <- "Customer Connection by Day Part"
caption <- "CC scores from 3, 6, and 12 month rolling periods"
#manual legend labels
lname <- "Rolling Months"
llabels <- c("3", "6", "12") 
#set data and variables
pdata <- st349roll
px <- st349roll[, DAY_PART]
py <- st349roll[, CC_SCORE]
groupvar <- st349roll[, months]
#line chart, factored by one variable
plot3 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  scale_colour_discrete(name=lname, labels=llabels) + 
  theme_economist() + 
  scale_x_discrete(name="",labels=xlabels) +
  ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot3)




##SELECT A "TYPICAL" STORE
#load data
st10051 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_store10051.csv") #2 quarters
#prep data
st10051[, CC_SCORE := round(CC_SCORE*100,1)]

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "CC Score"
sublabel <- "Store 10051"
tlabel <- "Customer Connection by Day Part"
caption <- "CE Survey Responses from Q4FY17 and Q1FY18 for Store 10051"
#manual legend labels
lname <- "Quarter"
llabels <- c("Q4FY17","Q1FY18")
#values
pdata1a <- st10051
px1a <- st10051[,DAY_PART]
py1a <- st10051[,CC_SCORE]
groupvar1a <- st10051[,FSCL_YR_NUM]
nvar1a <- st10051[,TOTAL_RSPNS ]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


#aggreate pre-11am and post-11am

#load data
cs0q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2months.csv") #2months
cs1q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_1quarter.csv") #1 quarter
cs2q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters.csv") #2 quarters
cs3q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_3quarters.csv") #3 quarters
cs4q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_4quarters.csv") #4 quarters
cs8q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_8quarters.csv") #8 quarters

#create a binary day part indicator for pre-11am and post-11am
cs0q[DAY_PART<=2, post11am := 0]; cs0q[DAY_PART>=3, post11am := 1]
cs1q[DAY_PART<=2, post11am := 0]; cs1q[DAY_PART>=3, post11am := 1]
cs2q[DAY_PART<=2, post11am := 0]; cs2q[DAY_PART>=3, post11am := 1]
cs3q[DAY_PART<=2, post11am := 0]; cs3q[DAY_PART>=3, post11am := 1]
cs4q[DAY_PART<=2, post11am := 0]; cs4q[DAY_PART>=3, post11am := 1]
cs8q[DAY_PART<=2, post11am := 0]; cs8q[DAY_PART>=3, post11am := 1]

#agg at the store level
cs0q <- cs0q[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","post11am")]
cs1q <- cs1q[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","post11am")]
cs2q <- cs2q[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","post11am")]
cs3q <- cs3q[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","post11am")]
cs4q <- cs4q[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","post11am")]
cs8q <- cs8q[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("STORE_NUM","QSTN_ID","post11am")]

#calculate percent of stores with survey counts over 50, by daypart
cs0q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs0q[TOTAL_RSPNS<50, resp_over50 := 0]
cs1q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs1q[TOTAL_RSPNS<50, resp_over50 := 0]
cs2q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs2q[TOTAL_RSPNS<50, resp_over50 := 0]
cs3q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs3q[TOTAL_RSPNS<50, resp_over50 := 0]
cs4q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs4q[TOTAL_RSPNS<50, resp_over50 := 0]
cs8q[TOTAL_RSPNS>=50, resp_over50 := 1]; cs8q[TOTAL_RSPNS<50, resp_over50 := 0]

#percent of stores
#two months
temp0 <- cs0q %>%
  group_by(post11am,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp0)
setorder(temp0,resp_over50,post11am)
temp0[, quarters := 0]
#one quarter
temp1 <- cs1q %>%
  group_by(post11am,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp1)
setorder(temp1,resp_over50,post11am)
temp1[, quarters := 1]
#two quarters
temp2 <- cs2q %>%
  group_by(post11am,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp2)
setorder(temp2,resp_over50,post11am)
temp2[, quarters := 2]
#three quarters
temp3 <- cs3q %>%
  group_by(post11am,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp3)
setorder(temp3,resp_over50,post11am)
temp3[, quarters := 3]
#4 quarters
temp4 <- cs4q %>%
  group_by(post11am,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp4)
setorder(temp4,resp_over50,post11am)
temp4[, quarters := 4]
#8 quarters
temp8 <- cs8q %>%
  group_by(post11am,resp_over50) %>%
  summarise (storeN50 = n()) %>%
  mutate(pct50 = round(storeN50 / sum(storeN50),3)*100)
setDT(temp8)
setorder(temp8,resp_over50,post11am)
temp8[, quarters := 8]
#descriptives
t0 <- temp0[resp_over50==1]
t1 <- temp1[resp_over50==1]
t2 <- temp2[resp_over50==1]
t3 <- temp3[resp_over50==1]
t4 <- temp4[resp_over50==1]
t8 <- temp8[resp_over50==1]
t50 <- rbind(t0,t1,t2,t3,t4)
setDT(t50)

#melt for plotting
tmelt <- t50[, resp_over50 := NULL]

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM - AM","Midday - Late PM")
ylabel <- "CC Score"
sublabel <- "Percent of Stores meeting Mimum Threshold at Varying Intervals"
tlabel <- "CE Survey Responses by Day Part"
caption <- "Miminum Threshold = 50 Surveys"
#manual legend labels
lname <- "Months"
llabels <- c("2","3","6","9","12")
#values
pdata1a <- tmelt
px1a <- tmelt[,post11am]
py1a <- tmelt[,pct50]
groupvar1a <- tmelt[,quarters]
nvar1a <- tmelt[,storeN50]
#plot itself
plot2 <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot2)


#specific movement
cs <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters_fordeltas.csv") #2months
csd <- cs[, .(STORE_NUM,DAY_PART,CC_SCORE,FSCL_YR_NUM)]
#swing wide
csd <- dcast.data.table(csd, STORE_NUM + DAY_PART ~ FSCL_YR_NUM, value.var="CC_SCORE")
setnames(csd, c("2017","2018"), c("cc17","cc18"))
#calculate delta
csd[, ccdel := (cc18-cc17)*100]

#indicator for greater than 10% change
csdpm <- csd[DAY_PART==4]
csdpm[ccdel<10&ccdel>(-10), ccdel10 := 0]; csdpm[ccdel>=10|ccdel<=(-10), ccdel10 := 1]
csdpm <- na.omit(csdpm,cols="ccdel10")
csdpm[,.N/nrow(csdpm),by="ccdel10"]
csdpm[ccdel<5&ccdel>(-5), ccdel5 := 0]; csdpm[ccdel>=5|ccdel<=(-5), ccdel5 := 1]
csdpm <- na.omit(csdpm,cols="ccdel5")
csdpm[,.N/nrow(csdpm),by="ccdel5"]

#set labels
xlabel <- "CC Score Delta"
ylabel <- "Number of Stores"
sublabel <- "3-Month Periods: Q4FY17 and Q1FY18, PM-Only"
tlabel <- "Delta in CC Score across Two 3-Month Periods"
caption <- "All Stores"
#plot itself
plot2 <- ggplot(csdpm,aes(ccdel)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-50,50), breaks = scales::pretty_breaks(20)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)



#specific movement
cs <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters_fordeltas.csv") #2months
csd <- cs[TOTAL_RSPNS>50, .(STORE_NUM,DAY_PART,CC_SCORE,FSCL_YR_NUM)]
#swing wide
csd <- dcast.data.table(csd, STORE_NUM + DAY_PART ~ FSCL_YR_NUM, value.var="CC_SCORE")
setnames(csd, c("2017","2018"), c("cc17","cc18"))
#calculate delta
csd[, ccdel := (cc18-cc17)*100]

#indicator for greater than 10% change
csdpm <- csd[DAY_PART==4]
csdpm[ccdel<10&ccdel>(-10), ccdel10 := 0]; csdpm[ccdel>=10|ccdel<=(-10), ccdel10 := 1]
csdpm <- na.omit(csdpm,cols="ccdel10")
csdpm[,.N/nrow(csdpm),by="ccdel10"]
csdpm[ccdel<5&ccdel>(-5), ccdel5 := 0]; csdpm[ccdel>=5|ccdel<=(-5), ccdel5 := 1]
csdpm <- na.omit(csdpm,cols="ccdel5")
csdpm[,.N/nrow(csdpm),by="ccdel5"]

#stores with 50+
#set labels
xlabel <- "CC Score Delta"
ylabel <- "Number of Stores"
sublabel <- "3-Month Periods: Q4FY17 and Q1FY18, PM-Only"
tlabel <- "Delta in CC Score across Two 3-Month Periods"
caption <- "Stores with 50+ Surveys"
#plot itself
plot2 <- ggplot(csdpm,aes(ccdel)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-25,25), breaks = scales::pretty_breaks(10)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#specific movement
cs <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters_fordeltas.csv") #2months
csd <- cs[TOTAL_RSPNS>40, .(STORE_NUM,DAY_PART,CC_SCORE,FSCL_YR_NUM)]
#swing wide
csd <- dcast.data.table(csd, STORE_NUM + DAY_PART ~ FSCL_YR_NUM, value.var="CC_SCORE")
setnames(csd, c("2017","2018"), c("cc17","cc18"))
#calculate delta
csd[, ccdel := (cc18-cc17)*100]

#indicator for greater than 10% change
csdpm <- csd[DAY_PART==4]
csdpm[ccdel<10&ccdel>(-10), ccdel10 := 0]; csdpm[ccdel>=10|ccdel<=(-10), ccdel10 := 1]
csdpm <- na.omit(csdpm,cols="ccdel10")
csdpm[,.N/nrow(csdpm),by="ccdel10"]
csdpm[ccdel<5&ccdel>(-5), ccdel5 := 0]; csdpm[ccdel>=5|ccdel<=(-5), ccdel5 := 1]
csdpm <- na.omit(csdpm,cols="ccdel5")
csdpm[,.N/nrow(csdpm),by="ccdel5"]

#stores with 50+
#set labels
xlabel <- "CC Score Delta"
ylabel <- "Number of Stores"
sublabel <- "3-Month Periods: Q4FY17 and Q1FY18, PM-Only"
tlabel <- "Delta in CC Score across Two 3-Month Periods"
caption <- "Stores with 40+ Surveys"
#plot itself
plot2 <- ggplot(csdpm,aes(ccdel)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-40,40), breaks = scales::pretty_breaks(20)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)


#mean and sd
sd(csdpm[,ccdel],na.rm=T)
mean(csdpm[,ccdel],na.rm=T)




#mean survey count by day part

#load data
cs1q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_1quarter.csv") #1 quarter
cs2q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters.csv") #2 quarters
#agg at the store level
cs1q <- cs1q[, list(avg_rspns = round(mean(TOTAL_RSPNS,na.rm=T),0)), by="DAY_PART"]
cs1q[, quarters := 1]
cs2q <- cs2q[, list(avg_rspns = round(mean(TOTAL_RSPNS,na.rm=T),0)), by="DAY_PART"]
cs2q[, quarters := 2]
csplot <- rbind(cs1q,cs2q)

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "Survey Count"
tlabel <- "Average CE Survey Response Count by Day Part"
caption <- "CE Survey Responses from Q4FY17 and Q1FY18"
#manual legend labels
lname <- "Months"
llabels <- c("3","6")
#values
pdata1a <- csplot
px1a <- csplot[,DAY_PART]
py1a <- csplot[,avg_rspns]
groupvar1a <- csplot[,quarters]
nvar1a <- csplot[,avg_rspns]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)




#load data
#create binary indicator
csplot[DAY_PART<=2, post11am := 0]; csplot[DAY_PART>=3, post11am := 1]
#agg at the store level
csplot <- csplot[, list(avg_rspns = sum(avg_rspns,na.rm=T)), by=c("post11am","quarters")]

#set labels
# xlabel <- "Proven SR"
xlabels <- c("Early AM - AM","Midday - Late PM")
ylabel <- "Survey Count"
tlabel <- "Average CE Survey Response Count by Day Part"
caption <- "CE Survey Responses from Q4FY17 and Q1FY18"
#manual legend labels
lname <- "Months"
llabels <- c("3","6")
#values
pdata1a <- csplot
px1a <- csplot[,post11am]
py1a <- csplot[,avg_rspns]
groupvar1a <- csplot[,quarters]
nvar1a <- csplot[,avg_rspns]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)





#specific movement
cs <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters_fordeltas.csv") #2months
#create a binary day part indicator for pre-11am and post-11am
cs[DAY_PART<=2, post11am := 0]; cs[DAY_PART>=3, post11am := 1]
cs <- cs[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                        TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                        CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
                 by=c("STORE_NUM","FSCL_YR_NUM","post11am")]
#swing wide
csd <- cs[, .(STORE_NUM,post11am,CC_SCORE,FSCL_YR_NUM)]
csd <- dcast.data.table(csd, STORE_NUM + post11am ~ FSCL_YR_NUM, value.var="CC_SCORE")
setnames(csd, c("2017","2018"), c("cc17","cc18"))
#calculate delta
csd[, ccdel := (cc18-cc17)*100]

#indicator for greater than 10% change
csdpm <- csd[post11am==1]
csdpm[ccdel<10&ccdel>(-10), ccdel10 := 0]; csdpm[ccdel>=10|ccdel<=(-10), ccdel10 := 1]
csdpm <- na.omit(csdpm,cols="ccdel10")
csdpm[,.N/nrow(csdpm),by="ccdel10"]
csdpm[ccdel<5&ccdel>(-5), ccdel5 := 0]; csdpm[ccdel>=5|ccdel<=(-5), ccdel5 := 1]
csdpm <- na.omit(csdpm,cols="ccdel5")
csdpm[,.N/nrow(csdpm),by="ccdel5"]

#set labels
xlabel <- "CC Score Delta"
ylabel <- "Number of Stores"
#sublabel <- "3-Month Periods: Q4FY17 and Q1FY18, PM-Only"
sublabel <- "3-Month Periods: Q4FY17 and Q1FY18, Midday-Late PM"
tlabel <- "Delta in CC Score across Two 3-Month Periods"
caption <- "All Stores"
#plot itself
plot2 <- ggplot(csdpm,aes(ccdel)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-40,40), breaks = scales::pretty_breaks(15)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)






#specific movement
cs <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters_fordeltas.csv") #2months
#create a binary day part indicator for pre-11am and post-11am
cs[DAY_PART<=3, post2pm := 0]; cs[DAY_PART>=4, post2pm := 1]
cs <- cs[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                CC_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
         by=c("STORE_NUM","FSCL_YR_NUM","post2pm")]
#swing wide
csd <- cs[, .(STORE_NUM,post2pm,CC_SCORE,FSCL_YR_NUM)]
csd <- dcast.data.table(csd, STORE_NUM + post2pm ~ FSCL_YR_NUM, value.var="CC_SCORE")
setnames(csd, c("2017","2018"), c("cc17","cc18"))
#calculate delta
csd[, ccdel := (cc18-cc17)*100]

#indicator for greater than 10% change
csdpm <- csd[post2pm==1]
csdpm[ccdel<10&ccdel>(-10), ccdel10 := 0]; csdpm[ccdel>=10|ccdel<=(-10), ccdel10 := 1]
csdpm <- na.omit(csdpm,cols="ccdel10")
csdpm[,.N/nrow(csdpm),by="ccdel10"]
csdpm[ccdel<5&ccdel>(-5), ccdel5 := 0]; csdpm[ccdel>=5|ccdel<=(-5), ccdel5 := 1]
csdpm <- na.omit(csdpm,cols="ccdel5")
csdpm[,.N/nrow(csdpm),by="ccdel5"]

#set labels
xlabel <- "CC Score Delta"
ylabel <- "Number of Stores"
#sublabel <- "3-Month Periods: Q4FY17 and Q1FY18, PM-Only"
sublabel <- "3-Month Periods: Q4FY17 and Q1FY18, PM-Late PM"
tlabel <- "Delta in CC Score across Two 3-Month Periods"
caption <- "All Stores"
#plot itself
plot2 <- ggplot(csdpm,aes(ccdel)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-40,40), breaks = scales::pretty_breaks(15)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)
