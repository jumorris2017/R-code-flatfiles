##SR/Non-SR Analysis using Brand Equity Study##
##December FY 18 data##

#load libraries
library(data.table)
library(foreign)
library(tidyverse)
library(ggthemes)
library(patchwork)

#load data
# be <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Foundational/Brand Equity Monitor/$ Brand Equity 2.0 US Retail/SPSS/DEC FY18 Final.sav", use.value.labels = FALSE, to.data.frame=TRUE)
be <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Foundational/Brand Equity Monitor/$ Brand Equity 2.0 US Retail/SPSS/APR 2018 Final.sav", use.value.labels = FALSE, to.data.frame=TRUE)

#convert to data.table
setDT(be)

#keep only variables we need
be <- be[, .(ProvenSR,Q5Starbucks_TotalVisits,QSB3_GotMyOrderRight_slice,QSB3_TheStoreWasClean_slice,
             QSB3_MadeAnEffortToGetToKnowMe_slice,QSB3_MyBeverageTastedGreat_slice,
             QSB3_WentAboveAndBeyondMyExpectations_slice,
             QSB3_WasAbleToPlacAndPickUpMyOrderInAReasonableAmounOfTime_slice,
             QSB3_MyFoodTastedGreat_slice,QSB3_PurchaseWasWorthWhatIPaid_slice)]

#drop NA's for core variables
be <- na.omit(be, cols=c("ProvenSR","Q5Starbucks_TotalVisits","QSB3_MadeAnEffortToGetToKnowMe_slice"))

#recode ProvenSR to binary
be[ProvenSR==2, ProvenSR := 0]

#rename long variables
setnames(be,c("Q5Starbucks_TotalVisits","QSB3_GotMyOrderRight_slice","QSB3_TheStoreWasClean_slice",
              "QSB3_MadeAnEffortToGetToKnowMe_slice","QSB3_MyBeverageTastedGreat_slice",
              "QSB3_WentAboveAndBeyondMyExpectations_slice",
              "QSB3_WasAbleToPlacAndPickUpMyOrderInAReasonableAmounOfTime_slice",
              "QSB3_MyFoodTastedGreat_slice","QSB3_PurchaseWasWorthWhatIPaid_slice"),
         c("visits","Q2_4","Q2_7","Q2_2","Q2_5","Q2_3","Q2_1","Q2_6","Q2_8"))

#create TB variables
be[Q2_4<7, Q2_4_score := 0]; be[Q2_4==7, Q2_4_score := 1]
be[Q2_7<7, Q2_7_score := 0]; be[Q2_7==7, Q2_7_score := 1]
be[Q2_2<7, Q2_2_score := 0]; be[Q2_2==7, Q2_2_score := 1]
be[Q2_5<7, Q2_5_score := 0]; be[Q2_5==7, Q2_5_score := 1]
be[Q2_3<7, Q2_3_score := 0]; be[Q2_3==7, Q2_3_score := 1]
be[Q2_1<7, Q2_1_score := 0]; be[Q2_1==7, Q2_1_score := 1]
be[Q2_6<7, Q2_6_score := 0]; be[Q2_6==7, Q2_6_score := 1]
be[Q2_8<7, Q2_8_score := 0]; be[Q2_8==7, Q2_8_score := 1]

#create frequency bins (1: 1-5, 2: 6-10, 3: 11-15, 4: 16+)
be[visits<=5, vis_bin := 1]
be[visits>=6&visits<=10, vis_bin := 2]
be[visits>=11&visits<=15, vis_bin := 3]
be[visits>=16, vis_bin := 4]

#calculate proportions in each group
tempbe <- be %>%
  group_by(vis_bin,ProvenSR) %>%
  summarize(Q2_4_score = round(mean(Q2_4_score,na.rm=T)*100,1), #accuracy
            Q2_7_score = round(mean(Q2_7_score,na.rm=T)*100,1), #clean
            Q2_2_score = round(mean(Q2_2_score,na.rm=T)*100,1), #cc
            Q2_5_score = round(mean(Q2_5_score,na.rm=T)*100,1), #bev taste
            Q2_3_score = round(mean(Q2_3_score,na.rm=T)*100,1), #above & beyond
            Q2_1_score = round(mean(Q2_1_score,na.rm=T)*100,1), #speed
            Q2_6_score = round(mean(Q2_6_score,na.rm=T)*100,1), #food taste
            Q2_8_score = round(mean(Q2_8_score,na.rm=T)*100,1), #worth
            n = n())
setDT(tempbe)
tempbe <- tempbe[!is.na(ProvenSR)&!is.na(vis_bin)]

#bring in our data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_customer_frequency_v2.csv")

#keep only december fy18
ce <- ce[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==3]

#create frequency bins (1: 1-5, 2: 6-10, 3: 11-15, 4: 16+)
ce[TRANS<=5, vis_bin := 1]
ce[TRANS>=6&TRANS<=10, vis_bin := 2]
ce[TRANS>=11&TRANS<=15, vis_bin := 3]
ce[TRANS>=16, vis_bin := 4]

#aggregate by vis_bin and question
ce <- ce[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
                TB_COUNT = sum(TB_COUNT,na.rm=T),
                RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T)),
         by=c("vis_bin")]
#make fake SR variable for plotting
ce[, ProvenSR := 1]

#freq table
ce %>% transmute(vis_bin, percent = USER_COUNT/sum(USER_COUNT))

#calculate TB score
ce[, tb_score := round((TB_COUNT / RSPSN_COUNT)*100,1)]

#plot 1: customer connection from brand equity
#set labels
# xlabel <- "Proven SR"
xlabels <- c("Non-SR","SR")
ylabel <- "CC Score"
tlabel <- "Brand Equity Study"
sublabel <- "Customer Connection"
caption <- "Brand Equity Study, April FY18"
#manual legend labels
lname <- "30-Day Visitation"
# llabels <- c("1-5", "6-10", "11-15", "16-20", "21-25", "26+")
llabels <- c("1-5", "6-10", "11-15", "16+")
#values
pdata1a <- tempbe
px1a <- tempbe[,ProvenSR]
py1a <- tempbe[,Q2_2_score]
groupvar1a <- tempbe[,vis_bin]
nvar1a <- tempbe[,n]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,50)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)

#plot 1b: customer connection from ce survey
#make fake grouping variable
#set labels
# xlabel <- "Proven SR"
xlabels <- c("SR")
ylabel <- "CC Score"
tlabel <- "Customer Experience Survey"
sublabel <- "Customer Connection"
caption <- "Customer Experience Survey, December FY18"
#manual legend labels
lname <- "30-Day Visitation"
llabels <- c("1-5", "6-10", "11-15", "16+")
#values
pdata1b <- ce
px1b <- ce[,ProvenSR]
py1b <- ce[,tb_score]
groupvar1b <- ce[,vis_bin]
nvar1b <- ce[,USER_COUNT]
#plot itself
plot1b <- ggplot(data=pdata1b,aes(y=py1b,x=as.factor(px1b),fill=as.factor(groupvar1b))) + 
  geom_bar(stat="identity", width = .7, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1b,y=0), stat="identity", vjust = -2, position = position_dodge(0.7)) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar1b),y=0), stat= "identity", vjust = -1, position = position_dodge(0.7)) 
print(plot1b)

#put together
plot1 <- plot1a + plot1b
print(plot1)

#store experience: all variables
#melt to do side-by-side barcharts
meltbe <- tempbe[, n := NULL]
meltbe <- melt(meltbe,id=c("vis_bin","ProvenSR"))
meltbe[, variable := ordered(variable, levels = c("Q2_1_score", "Q2_2_score", "Q2_3_score",
                                   "Q2_4_score", "Q2_5_score", "Q2_6_score",
                                   "Q2_7_score", "Q2_8_score"))]

#plot 2: ce from brand equity - non-sr
#set labels
xlabel <- "Non-SR Customers"
xlabels <- c("Speed","CC","Above & Beyond","Order Accuracy","Bev Taste","Food Taste","Cleanliness","Worth")
ylabel <- "Top Box Score"
tlabel <- "Brand Equity Study"
sublabel <- "Customer Experience"
#manual legend labels
lname <- "30-Day Visitation"
llabels <- c("1-5", "6-10", "11-15", "16+")
#values
pdata2 <- meltbe[ProvenSR==0]
px2 <- meltbe[ProvenSR==0,variable]
py2 <- meltbe[ProvenSR==0,value]
groupvar2 <- meltbe[ProvenSR==0,vis_bin]
#plot itself
plot2 <- ggplot(data=pdata2,aes(y=py2,x=as.factor(px2),fill=as.factor(groupvar2))) + 
  geom_bar(stat="identity", width = 0.7, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel) +
  geom_text(size = 2.5, aes(label=py2,y=0), angle=90, hjust=-0.25, stat="identity", position = position_dodge(0.7))

#plot 3: ce from brand equity - sr
#set labels
xlabel <- "SR Customers"
caption <- "Brand Equity Study, December FY18"
#values
pdata3 <- meltbe[ProvenSR==1]
px3 <- meltbe[ProvenSR==1,variable]
py3 <- meltbe[ProvenSR==1,value]
groupvar3 <- meltbe[ProvenSR==1,vis_bin]
#plot itself
plot3 <- ggplot(data=pdata3,aes(y=py3,x=as.factor(px3),fill=as.factor(groupvar3))) + 
  geom_bar(stat="identity", width = 0.7, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, guide=FALSE) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  xlab(xlabel) + ylab(ylabel) + labs(caption=caption) +
  geom_text(size = 2.5, aes(label=py3,y=0), angle=90, hjust=-0.25, stat="identity", position = position_dodge(0.7))

# #set labels
# xlabel <- "SR Customers"
# xlabels <- c("Speed","CC","Above & Beyond","Order Accuracy","Bev Taste","Food Taste","Cleanliness","Worth")
# ylabel <- "Top Box Score"
# tlabel <- "Customer Experience Survey"
# sublabel <- "Customer Experience"
# caption <- "Customer Experience Survey, December FY18"
# #manual legend labels
# lname <- "30-Day Visitation"
# llabels <- c("1-5", "6-10", "11-15", "16+")
# #values
# pdata4 <- ce
# px4 <- ce[,QSTN_ID]
# py4 <- ce[,tb_score]
# groupvar4 <- ce[,vis_bin]
# #plot itself
# plot4 <- ggplot(data=pdata4,aes(y=py4,x=as.factor(px4),fill=as.factor(groupvar4))) + 
#   geom_bar(stat="identity", width = 0.7, position=position_dodge(), colour="black") +
#   scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
#   scale_x_discrete(name=xlabel,labels=xlabels) +
#   ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
#   geom_text(size = 2.5, aes(label=py4,y=0), angle=90, hjust=-0.25, stat="identity", position = position_dodge(0.7))
# #combine into one plot
# plot5 <- plot2 / plot3 / plot4
# print(plot5)

# 
# ##chart of High freq (6+) SR, non-SR from brand equity
# hfnames <- c("2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")
# hfsr <- c(0.316734235763449,0.400199086,0.354659141,0.338949766,0.347741418,0.386038913,0.403829059,0.357128164,0.349871869,0.375000117,0.349929275,0.364159199)
# hfnsr <- c(0.263260941976257,0.33346994,0.244993283,0.304362228,0.301443643,0.258287159,0.306139404,0.262061505,0.247021047,0.272584936,0.287737543,0.32874728)
# 
# 
# #bring in our data
# ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_customer_frequency_v2.csv")
# 
# #keep only question 2
# #ce <- ce[QSTN_ID=="Q2_2"]
# 
# #create frequency bins (1: 1-5, 2: 6+)
# ce[TRANS<=5, hf6plus := 0]
# ce[TRANS>=6, hf6plus := 1]
# #keep only high freq
# ce <- ce[hf6plus==1]
# 
# #aggregate by vis_bin and question
# ce <- ce[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
#                 TB_COUNT = sum(TB_COUNT,na.rm=T),
#                 RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T)),
#          by=c("hf6plus","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM")]
# 
# #calculate TB score
# ce[, tb_score := round((TB_COUNT / RSPSN_COUNT),3)]
# 
# #sort by year and month
# setorder(ce,FSCL_YR_NUM,FSCL_PER_IN_YR_NUM)
# 
# #attach columns from brand equity
# cebe <- cbind(ce,hfnames,hfsr,hfnsr)
# setDT(cebe)
# cebe <- cebe[,c("hfnames","tb_score","hfsr","hfnsr"),with=FALSE]
# 
# #melt by pop
# cebe <- melt(cebe,id.var="hfnames")
# 
# #set labels
# xlabel <- "Time"
# ylabel <- "CC Score"
# sublabel <- "Customer Connection Trend"
# tlabel <- "Higher Frequency Customers (6+ visits/month)"
# caption <- "Customer Experience Survey and Brand Equity Surveys, Jan-Dec 2017"
# #manual legend labels
# lname <- "Survey Group"
# llabels <- c("Customer Experience Survey","Brand Equity: SR","Brand Equity: Non-SR")
# #values
# pdata <- cebe
# px <- cebe[,hfnames]
# py <- cebe[,value]
# groupvar <- cebe[,variable]
# #plot itself
# plot2 <- ggplot() +
#   geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
#   xlab("") + ylab(ylabel) + 
#   scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
#   guides(colour = guide_legend(override.aes = list(size = 7))) + 
#   #scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks), labels = waiver()) +
#   scale_y_continuous(limits=c(0,pdata[,max(py)])) + theme_economist_white(gray_bg = FALSE) +
#   ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
# print(plot2)
# 






# ##UPDATE TO INCLUDE TRENDED CC BY FREQUENCY BINS & ROLLING 2-MONTHS

#load data
be <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Foundational/Brand Equity Monitor/$ Brand Equity 2.0 US Retail/SPSS/DEC FY18 Final.sav", use.value.labels = FALSE, to.data.frame=TRUE)
#convert to data.table
setDT(be)

#keep only variables we need
be <- be[, .(ProvenSR,Month,Q5Starbucks_TotalVisits,QSB3_MadeAnEffortToGetToKnowMe_slice)]

#drop NA's for core variables
be <- na.omit(be, cols=c("ProvenSR","Month","Q5Starbucks_TotalVisits","QSB3_MadeAnEffortToGetToKnowMe_slice"))

#recode ProvenSR to binary
be[ProvenSR==2, ProvenSR := 0]

#rename long variables
setnames(be,c("Q5Starbucks_TotalVisits","QSB3_MadeAnEffortToGetToKnowMe_slice"), c("visits","Q2_2"))

#create TB variables
be[Q2_2<7, Q2_2_score := 0]; be[Q2_2==7, Q2_2_score := 1]
#create fake variable to calculate response count
be[Q2_2<=7, RSPSN_COUNT := 1]

#ensure it's sorted by year and period for lagging
be <- setorder(be,Month)
#calculate rolling two by vis_bin
be[, lag_TB :=lapply(.SD, function(x) c(NA, x[-.N])), by="ProvenSR", .SDcols="Q2_2_score"]
be[, lag_RSPNS :=lapply(.SD, function(x) c(NA, x[-.N])), by="ProvenSR", .SDcols="RSPSN_COUNT"]
#sum together
be[, R2MTB := rowSums(.SD, na.rm = TRUE), .SDcols=c("Q2_2_score","lag_TB")]
be[, R2MRSPNS := rowSums(.SD, na.rm = TRUE), .SDcols=c("RSPSN_COUNT","lag_RSPNS")]

#keep only FY17 (month 4-15)
be <- be[Month>=4&Month<=15]

#mapvalues
monthnames <- c("2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")
be[, fyfp :=  plyr::mapvalues(be[, Month], from = c(4:15), to = monthnames)]

#create frequency bins (1: 1-5, 2: 6-10, 3: 11-15, 4: 16+)
be[visits<=5, vis_bin := 1]
be[visits>=6&visits<=10, vis_bin := 2]
be[visits>=11&visits<=15, vis_bin := 3]
be[visits>=16, vis_bin := 4]

#aggregate by vis_bin, ProvenSR, and month
be <- be[, list(R2MTB = sum(R2MTB,na.rm=T),
                R2MRSPNS = sum(R2MRSPNS,na.rm=T)),
         by=c("vis_bin","fyfp","ProvenSR")]
be <- setorder(be,vis_bin,fyfp,ProvenSR)

#calculate TB score
be[, tb_score := round(R2MTB/R2MRSPNS,3)]


#bring in our data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_customer_frequency_v2.csv")

#keep only question 2
# ce <- ce[QSTN_ID=="Q2_2"]

#create frequency bins (1: 1-5, 2: 6-10, 3: 11-15, 4: 16+)
ce[TRANS<=5, vis_bin := 1]
ce[TRANS>=6&TRANS<=10, vis_bin := 2]
ce[TRANS>=11&TRANS<=15, vis_bin := 3]
ce[TRANS>=16, vis_bin := 4]

#aggregate by vis_bin and month
ce <- ce[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
                TB_COUNT = sum(TB_COUNT,na.rm=T),
                RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T)),
         by=c("vis_bin","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM")]

#ensure it's sorted by year and period for lagging
ce <- setorder(ce,FSCL_YR_NUM,FSCL_PER_IN_YR_NUM)
#calculate rolling two by vis_bin
ce[, lag_TB :=lapply(.SD, function(x) c(NA, x[-.N])), by="vis_bin", .SDcols="TB_COUNT"]
ce[, lag_RSPNS :=lapply(.SD, function(x) c(NA, x[-.N])), by="vis_bin", .SDcols="RSPSN_COUNT"]
#sum together
ce[, R2MTB := rowSums(.SD, na.rm = TRUE), .SDcols=c("TB_COUNT","lag_TB")]
ce[, R2MRSPNS := rowSums(.SD, na.rm = TRUE), .SDcols=c("RSPSN_COUNT","lag_RSPNS")]
#drop earliest month
ce <- ce[(FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM>=4)|(FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM<=3)]
ce[, tb_score := round(R2MTB/R2MRSPNS,3)]

#create fyfp var
ce[, tempvar := paste0(FSCL_YR_NUM,FSCL_PER_IN_YR_NUM)]
#mapvalues
monthnames <- c("2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")
ce[, fyfp :=  plyr::mapvalues(ce[, tempvar], from = unique(ce[, tempvar]), to = monthnames)]
ce[, tempvar := NULL]

#calculate TB score
# ce[, tb_score := round((TB_COUNT / RSPSN_COUNT),3)]

#sort
setorder(ce,vis_bin,fyfp)

#make fake SR variable for plotting
ce[, ProvenSR := 2]

#rbindlist the datasets together
be <- be[,.(vis_bin,fyfp,ProvenSR,tb_score)]
ce <- ce[,.(vis_bin,fyfp,ProvenSR,tb_score)]

#attach columns from brand equity
cebe <- rbind(ce,be)
setDT(cebe)

#set labels
xlabel <- "Time"
ylabel <- "CC Score"
tlabel <- "Customer Connection Trend"
sublabel <- "January - December 2017 (Rolling 2)"
caption <- "Customer Experience and Brand Equity Surveys"
#manual legend labels
lname1 <- "Survey Group"
llabels1 <- c("BE (Non-SR)","BE (SR)","CE (SR)")
lname2 <- "Monthly Visits"
llabels2 <- c("1-5","6-10","11-15","16+")
#values
pdata <- cebe
px <- cebe[,fyfp]
py <- cebe[,tb_score]
groupvar <- cebe[,vis_bin]
colourvar <- cebe[,ProvenSR]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=px, y=py, colour=factor(colourvar), shape=factor(groupvar), group=interaction(groupvar, colourvar))) + 
  geom_point(size=2.5) + geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_shape_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  guides(shape = guide_legend(override.aes = list(size = 5))) + 
  scale_y_continuous(limits=c(0,.6)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#do for just low freq
#set labels
xlabel <- "Time"
ylabel <- "CC Score"
sublabel <- "Customer Connection Trend"
tlabel <- "Low Frequency Customers (1-5 visits/month)"
caption <- "Customer Experience Survey and Brand Equity Surveys, Jan-Dec 2017 (Rolling 2)"
#manual legend labels
lname <- "Survey Group"
llabels <- c("Brand Equity: Non-SR","Brand Equity: SR","Customer Experience Survey")
#values
pdata <- cebe[vis_bin==1]
px <- cebe[vis_bin==1,fyfp]
py <- cebe[vis_bin==1,tb_score*100]
groupvar <- cebe[vis_bin==1,ProvenSR]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab("") + ylab(ylabel) + 
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,40)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#do for just low/mid freq
#set labels
xlabel <- "Time"
ylabel <- "CC Score"
sublabel <- "Customer Connection Trend"
tlabel <- "Low-Mid Frequency Customers (6-10 visits/month)"
caption <- "Customer Experience Survey and Brand Equity Surveys, Jan-Dec 2017 (Rolling 2)"
#manual legend labels
lname <- "Survey Group"
llabels <- c("Brand Equity: Non-SR","Brand Equity: SR","Customer Experience Survey")
#values
pdata <- cebe[vis_bin==2]
px <- cebe[vis_bin==2,fyfp]
py <- cebe[vis_bin==2,tb_score]
groupvar <- cebe[vis_bin==2,ProvenSR]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab("") + ylab(ylabel) + 
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,.6)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#do for just mid/high freq
#set labels
xlabel <- "Time"
ylabel <- "CC Score"
sublabel <- "Customer Connection Trend"
tlabel <- "Mid-High Frequency Customers (11-15 visits/month)"
caption <- "Customer Experience Survey and Brand Equity Surveys, Jan-Dec 2017 (Rolling 2)"
#manual legend labels
lname <- "Survey Group"
llabels <- c("Brand Equity: Non-SR","Brand Equity: SR","Customer Experience Survey")
#values
pdata <- cebe[vis_bin==3]
px <- cebe[vis_bin==3,fyfp]
py <- cebe[vis_bin==3,tb_score]
groupvar <- cebe[vis_bin==3,ProvenSR]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab("") + ylab(ylabel) + 
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,.6)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#do for just high freq
#set labels
xlabel <- "Time"
ylabel <- "CC Score"
sublabel <- "Customer Connection Trend"
tlabel <- "High Frequency Customers (16+ visits/month)"
caption <- "Customer Experience Survey and Brand Equity Surveys, Jan-Dec 2017 (Rolling 2)"
#manual legend labels
lname <- "Survey Group"
llabels <- c("Brand Equity: Non-SR","Brand Equity: SR","Customer Experience Survey")
#values
pdata <- cebe[vis_bin==4]
px <- cebe[vis_bin==4,fyfp]
py <- cebe[vis_bin==4,tb_score]
groupvar <- cebe[vis_bin==4,ProvenSR]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab("") + ylab(ylabel) + 
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,.6)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#do separately for each survey
#set labels
ylabel <- "CC Score"
tlabel <- "Customer Experience Survey"
sublabel <- "January - December 2017 (Rolling 2)"
#manual legend labels
lname2 <- "Monthly Visits"
llabels2 <- c("1-5","6-10","11-15","16+")
#values
pdata <- cebe[ProvenSR==2]
px <- cebe[ProvenSR==2,fyfp]
py <- cebe[ProvenSR==2,tb_score]
groupvar <- cebe[ProvenSR==2,vis_bin]
#plot itself
plot3 <- ggplot(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  geom_line() +
  xlab("") + ylab(ylabel) + 
  scale_colour_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) + 
  scale_y_continuous(limits=c(0,0.6)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel)
print(plot3)


##EXPERIAN DEMOS
#load data
experian <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/GUIDs_from_experian.csv")
setnames(experian,"GUID_USER_ID","GUID_ID")
guids_cesr <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/GUIDs_for_experian_ceSR.csv")
guids_so <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/GUIDs_soscores_ceSR.csv")
setnames(guids_so,"GUID_USER_ID","GUID_ID")
guids_so <- guids_so[GUID_ID %in% guids_cesr[,GUID_ID]]
#swing so data wide
sow <- dcast.data.table(guids_so, GUID_ID ~ QSTN_ID, value.var=c("TOTAL_TB","TOTAL_RSPNS"))
#remove duplicates
guids_cesr <- subset(guids_cesr, !duplicated(guids_cesr$GUID_ID))
#drop transaction indicator (redundant variable with experian data)
guids_cesr[, TRANS := NULL]
#add CE flag
guids_cesr[, CEtaker := 1]
#merge into experian data
experian <- merge(experian,guids_cesr,all.x=T,by="GUID_ID")
experian <- merge(experian,sow,all.x=T,by="GUID_ID")
#if CE taker is null, make it 0
experian[is.na(CEtaker), CEtaker := 0]

#recode the demographic variables
#sex
experian[GENDER=="M", female := 0]; experian[GENDER=="F", female := 1]
#income
experian[EST_HOUSEHOLD_INCOME_V5=="A", inc_midpt := 6999.5]
experian[EST_HOUSEHOLD_INCOME_V5=="B", inc_midpt := 19999]
experian[EST_HOUSEHOLD_INCOME_V5=="C", inc_midpt := 29999]
experian[EST_HOUSEHOLD_INCOME_V5=="D", inc_midpt := 42499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="E", inc_midpt := 62499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="F", inc_midpt := 87499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="G", inc_midpt := 112499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="H", inc_midpt := 137499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="I", inc_midpt := 162499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="J", inc_midpt := 162499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="K", inc_midpt := 187499.5]
experian[EST_HOUSEHOLD_INCOME_V5=="L", inc_midpt := 250000]
#income
experian[EST_HOUSEHOLD_INCOME_V5=="A", inc_75k := 0]
experian[EST_HOUSEHOLD_INCOME_V5=="B", inc_75k := 0]
experian[EST_HOUSEHOLD_INCOME_V5=="C", inc_75k := 0]
experian[EST_HOUSEHOLD_INCOME_V5=="D", inc_75k := 0]
experian[EST_HOUSEHOLD_INCOME_V5=="E", inc_75k := 0]
experian[EST_HOUSEHOLD_INCOME_V5=="F", inc_75k := 1]
experian[EST_HOUSEHOLD_INCOME_V5=="G", inc_75k := 1]
experian[EST_HOUSEHOLD_INCOME_V5=="H", inc_75k := 1]
experian[EST_HOUSEHOLD_INCOME_V5=="I", inc_75k := 1]
experian[EST_HOUSEHOLD_INCOME_V5=="J", inc_75k := 1]
experian[EST_HOUSEHOLD_INCOME_V5=="K", inc_75k := 1]
experian[EST_HOUSEHOLD_INCOME_V5=="L", inc_75k := 1]
#education - make binary college grad
experian[EDUCATION_MODEL==11|EDUCATION_MODEL==15|EDUCATION_MODEL==51|EDUCATION_MODEL==55, collegegrad := 0]
experian[(EDUCATION_MODEL>=12&EDUCATION_MODEL<=14)|(EDUCATION_MODEL>=52&EDUCATION_MODEL<=54), collegegrad := 1]
#remove letters from age variable
experian[, age := as.numeric(gsub("[^0-9.]", "", COMBINED_AGE))]
#make age category variables
experian[age>=21&age<=36, age_cat := 1] #millennials
experian[age>=37&age<=52, age_cat := 2] #generation x
experian[age>=53&age<=71, age_cat := 3] #baby boomers

#marital status
experian[MARITAL_STATUS=="5S", married := 0]
experian[MARITAL_STATUS=="1M"|MARITAL_STATUS=="5M", married := 1]

#create frequency bins (1: 1-5, 2: 6-10, 3: 11-15, 4: 16+)
experian[TRANS<=5, vis_bin := 1]
experian[TRANS>=6&TRANS<=10, vis_bin := 2]
experian[TRANS>=11&TRANS<=15, vis_bin := 3]
experian[TRANS>=16, vis_bin := 4]

#cc scores by age
cc <- experian[!is.na(age_cat)] %>%
  filter(CEtaker %in% 1) %>%
  group_by(age_cat) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]

#melt for plotting
cc[, respN := NULL]
ccm <- melt(cc,id="age_cat")
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 1: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Generation"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Millennials (21-36), Generation X (37-52), Baby Boomers (23-71)\nDemographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Generation"
llabels <- c("Millennials", "Generation X", "Baby Boomers")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,age_cat]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
  #geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)

#cc scores by gender
cc <- experian[!is.na(female)] %>%
  filter(CEtaker %in% 1) %>%
  group_by(female) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]

#melt for plotting
cc[, respN := NULL]
ccm <- melt(cc,id="female")
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 1: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Gender"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Gender"
llabels <- c("Male", "Female")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,female]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)

#cc scores by education level
cc <- experian[!is.na(collegegrad)] %>%
  filter(CEtaker %in% 1) %>%
  group_by(collegegrad) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]

#melt for plotting
cc[, respN := NULL]
ccm <- melt(cc,id="collegegrad")
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 1: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Education Level"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Education Level"
llabels <- c("No College Degree", "College Degree+")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,collegegrad]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)

#cc scores by marital status
cc <- experian[!is.na(married)] %>%
  filter(CEtaker %in% 1) %>%
  group_by(married) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]

#melt for plotting
cc[, respN := NULL]
ccm <- melt(cc,id="married")
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 1: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Marital Status"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Marital Status"
llabels <- c("Single", "Married/Partnered")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,married]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


#cc scores by income
cc <- experian[!is.na(inc_75k)] %>%
  filter(CEtaker %in% 1) %>%
  group_by(inc_75k) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]

#melt for plotting
cc[, respN := NULL]
ccm <- melt(cc,id="inc_75k")
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 1: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Household Income"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Household Income"
llabels <- c("<$75,000", "$75,000+")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,inc_75k]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
#geom_text(size = 2.5, aes(label=paste0("n=",nvar1a),y=0), stat= "identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)




##interactions
#cc scores by education level AND income
cc <- experian[!is.na(collegegrad)&!is.na(inc_75k)] %>%
  filter(CEtaker %in% 1) %>%
  group_by(collegegrad,inc_75k) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]
#make interaction variable
cc[, cg_inc75k := paste0(collegegrad,"-",inc_75k)]

#melt for plotting
cc[, c("collegegrad","inc_75k") := NULL]
cc[, respN := NULL]
ccm <- melt(cc,id=c("cg_inc75k"))
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 2: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Education Level & Household Income"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Education Level & Household Income"
llabels <- c("No BA <$75,000", "No BA $75,000+",
             "BA+ <$75,000", "BA+ $75,000+")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,cg_inc75k]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) + 
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


##frequency bin #1
#cc scores by education level AND income
cc <- experian[!is.na(collegegrad)&!is.na(inc_75k)] %>%
  filter(CEtaker %in% 1) %>% filter(vis_bin %in% 1) %>%
  group_by(collegegrad,inc_75k) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]
#make interaction variable
cc[, cg_inc75k := paste0(collegegrad,"-",inc_75k)]

#melt for plotting
cc[, c("collegegrad","inc_75k") := NULL]
cc[, respN := NULL]
ccm <- melt(cc,id=c("cg_inc75k"))
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 2: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Education Level & Household Income, 1-5 Visits/Month"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Education Level & Household Income"
llabels <- c("No BA <$75,000", "No BA $75,000+",
             "BA+ <$75,000", "BA+ $75,000+")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,cg_inc75k]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) + 
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)

##frequency bin #2
#cc scores by education level AND income
cc <- experian[!is.na(collegegrad)&!is.na(inc_75k)] %>%
  filter(CEtaker %in% 1) %>% filter(vis_bin %in% 2) %>%
  group_by(collegegrad,inc_75k) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]
#make interaction variable
cc[, cg_inc75k := paste0(collegegrad,"-",inc_75k)]

#melt for plotting
cc[, c("collegegrad","inc_75k") := NULL]
cc[, respN := NULL]
ccm <- melt(cc,id=c("cg_inc75k"))
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 2: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Education Level & Household Income, 6-10 Visits/Month"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Education Level & Household Income"
llabels <- c("No BA <$75,000", "No BA $75,000+",
             "BA+ <$75,000", "BA+ $75,000+")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,cg_inc75k]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) + 
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)

##frequency bin #3
#cc scores by education level AND income
cc <- experian[!is.na(collegegrad)&!is.na(inc_75k)] %>%
  filter(CEtaker %in% 1) %>% filter(vis_bin %in% 3) %>%
  group_by(collegegrad,inc_75k) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]
#make interaction variable
cc[, cg_inc75k := paste0(collegegrad,"-",inc_75k)]

#melt for plotting
cc[, c("collegegrad","inc_75k") := NULL]
cc[, respN := NULL]
ccm <- melt(cc,id=c("cg_inc75k"))
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 2: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Education Level & Household Income, 11-15 Visits/Month"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Education Level & Household Income"
llabels <- c("No BA <$75,000", "No BA $75,000+",
             "BA+ <$75,000", "BA+ $75,000+")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,cg_inc75k]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) + 
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


##frequency bin #4
#cc scores by education level AND income
cc <- experian[!is.na(collegegrad)&!is.na(inc_75k)] %>%
  filter(CEtaker %in% 1) %>% filter(vis_bin %in% 4) %>%
  group_by(collegegrad,inc_75k) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]
#make interaction variable
cc[, cg_inc75k := paste0(collegegrad,"-",inc_75k)]

#melt for plotting
cc[, c("collegegrad","inc_75k") := NULL]
cc[, respN := NULL]
ccm <- melt(cc,id=c("cg_inc75k"))
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 2: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Education Level & Household Income, 16+ Visits/Month"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Demographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Education Level & Household Income"
llabels <- c("No BA <$75,000", "No BA $75,000+",
             "BA+ <$75,000", "BA+ $75,000+")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,cg_inc75k]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) + 
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)



#cc scores by age
cc <- experian[!is.na(age_cat)&!is.na(female)] %>%
  filter(CEtaker %in% 1) %>%
  group_by(age_cat,female) %>%
  summarise(respN = sum(TOTAL_RSPNS,na.rm=T),
            cc_score = round((sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),3)*100,
            so1_score = round((sum(TOTAL_TB_Q2_1,na.rm=T)/sum(TOTAL_RSPNS_Q2_1,na.rm=T)),3)*100,
            so2_score = round((sum(TOTAL_TB_Q2_3,na.rm=T)/sum(TOTAL_RSPNS_Q2_3,na.rm=T)),3)*100,
            so3_score = round((sum(TOTAL_TB_Q2_4,na.rm=T)/sum(TOTAL_RSPNS_Q2_4,na.rm=T)),3)*100,
            so4_score = round((sum(TOTAL_TB_Q2_5,na.rm=T)/sum(TOTAL_RSPNS_Q2_5,na.rm=T)),3)*100,
            so5_score = round((sum(TOTAL_TB_Q2_6,na.rm=T)/sum(TOTAL_RSPNS_Q2_6,na.rm=T)),3)*100,
            so6_score = round((sum(TOTAL_TB_Q2_7,na.rm=T)/sum(TOTAL_RSPNS_Q2_7,na.rm=T)),3)*100)
setDT(cc)
cc[, ceso_score := rowMeans(.SD), .SDcols=colnames(cc)[4:ncol(cc)]]
#make interaction variable
cc[, fem_agecat := paste0(female,"-",age_cat)]

#melt for plotting
cc[, c("age_cat","female") := NULL]
cc[, respN := NULL]
ccm <- melt(cc,id="fem_agecat")
preferred.order <- c("cc_score","ceso_score","so1_score","so2_score",
                     "so3_score","so4_score","so5_score","so6_score")
ccm[, variable := factor(variable, levels=preferred.order)]
ccm <- setorder(ccm,variable)
ccm[, value := round(value,0)]

#plot 1: 
#set labels
xlabels <- c("Customer Connection","Store Ops","Speed","Above & Beyond","Accuracy","Bev Taste","Food Taste","Cleanliness")
ylabel <- "TB Score"
tlabel <- "Customer Experience by Gender & Generation"
sublabel <- "Customer Experience Study, December FY18"
caption <- "Millennials (21-36), Generation X (37-52), Baby Boomers (23-71)\nDemographic data from Alteryx, Update January FY18"
#manual legend labels
lname <- "Gender & Generation"
llabels <- c("Millennial Males", "Generation X Males", "Boomers Males", "Millennial Females", "Generation X Females","Boomers Females")
#values
pdata1a <- ccm
px1a <- ccm[,variable]
py1a <- ccm[,value]
groupvar1a <- ccm[,fem_agecat]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)
















#get percentages
temp <- experian %>% group_by(CEtaker,vis_bin) %>%
  summarise(collegegrad = round(mean(collegegrad,na.rm=T),3)*100,
            age = round(mean(age,na.rm=T),1),
            married = round(mean(married,na.rm=T),3)*100,
            female = round(mean(female,na.rm=T),3)*100,
            inc_med = median(inc_midpt,na.rm=T))
setDT(temp)

temp2 <- experian %>% group_by(CEtaker) %>%
  summarise(collegegrad = round(mean(collegegrad,na.rm=T),3)*100,
            age = round(mean(age,na.rm=T),1),
            married = round(mean(married,na.rm=T),3)*100,
            female = round(mean(female,na.rm=T),3)*100,
            inc_med = median(inc_midpt,na.rm=T))
setDT(temp2)

temp3 <- experian %>% group_by(vis_bin) %>%
  summarise(collegegrad = round(mean(collegegrad,na.rm=T),3)*100,
            age = round(mean(age,na.rm=T),1),
            married = round(mean(married,na.rm=T),3)*100,
            female = round(mean(female,na.rm=T),3)*100,
            inc_med = median(inc_midpt,na.rm=T))
setDT(temp3)

temp4 <- experian %>% 
  summarise(collegegrad = round(mean(collegegrad,na.rm=T),3)*100,
            age = round(mean(age,na.rm=T),1),
            married = round(mean(married,na.rm=T),3)*100,
            female = round(mean(female,na.rm=T),3)*100,
            inc_med = median(inc_midpt,na.rm=T))
setDT(temp4)

#t tests - CE SR population vs SR (non-CE) population
t.test(experian[CEtaker==0,collegegrad],experian[CEtaker==1,collegegrad])
t.test(experian[CEtaker==0,age],experian[CEtaker==1,age])
t.test(experian[CEtaker==0,married],experian[CEtaker==1,married])
t.test(experian[CEtaker==0,female],experian[CEtaker==1,female])



#for t-tests
cct <- experian[CEtaker==1, list(cc_score = TOTAL_TB/TOTAL_RSPNS,
            so1_score = TOTAL_TB_Q2_1/TOTAL_RSPNS_Q2_1,
            so2_score = TOTAL_TB_Q2_3/TOTAL_RSPNS_Q2_3,
            so3_score = TOTAL_TB_Q2_4/TOTAL_RSPNS_Q2_4,
            so4_score = TOTAL_TB_Q2_5/TOTAL_RSPNS_Q2_5,
            so5_score = TOTAL_TB_Q2_6/TOTAL_RSPNS_Q2_6,
            so6_score = TOTAL_TB_Q2_7/TOTAL_RSPNS_Q2_7),
            by=c("GUID_ID","collegegrad","female","age_cat","married","vis_bin","inc_75k")]
setDT(cct)
cct[, ceso_score := rowMeans(.SD), .SDcols=colnames(cct)[9:ncol(cct)]]

t.test(cct[collegegrad==0,cc_score],cct[collegegrad==1,cc_score])
t.test(cct[collegegrad==0,ceso_score],cct[collegegrad==1,ceso_score])

t.test(cct[married==0,cc_score],cct[married==1,cc_score])
t.test(cct[married==0,ceso_score],cct[married==1,ceso_score])

t.test(cct[inc_75k==0,cc_score],cct[inc_75k==1,cc_score])
t.test(cct[inc_75k==0,ceso_score],cct[inc_75k==1,ceso_score])

t.test(cct[female==0,cc_score],cct[female==1,cc_score])
t.test(cct[female==0,ceso_score],cct[female==1,ceso_score])

t.test(cct[age_cat==1,cc_score],cct[age_cat==2,cc_score])
t.test(cct[age_cat==2,cc_score],cct[age_cat==3,cc_score])
t.test(cct[age_cat==1,ceso_score],cct[age_cat==2,ceso_score])
t.test(cct[age_cat==2,ceso_score],cct[age_cat==3,ceso_score])
