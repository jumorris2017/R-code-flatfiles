##SR/Non-SR Analysis using Brand Equity Study##
##December FY 18 data##

#load libraries
library(data.table)
library(foreign)
library(tidyverse)
library(ggthemes)
library(patchwork)

#load data
be <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Foundational/Brand Equity Monitor/$ Brand Equity 2.0/SPSS/DEC FY18 Final.sav", use.value.labels = FALSE, to.data.frame=TRUE)
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

#bring in our data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_customer_frequency.csv")

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
         by=c("vis_bin","QSTN_ID")]
#make fake SR variable for plotting
ce[, ProvenSR := 1]

#calculate TB score
ce[, tb_score := round((TB_COUNT / RSPSN_COUNT)*100,1)]

#plot 1: customer connection from brand equity
#set labels
# xlabel <- "Proven SR"
xlabels <- c("Non-SR","SR")
ylabel <- "CC Score"
tlabel <- "Brand Equity Study"
sublabel <- "Customer Connection"
caption <- "Brand Equity Study, December FY18"
#manual legend labels
lname <- "30-Day Visitation"
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
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
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
pdata1b <- ce[QSTN_ID=="Q2_2"]
px1b <- ce[QSTN_ID=="Q2_2",ProvenSR]
py1b <- ce[QSTN_ID=="Q2_2",tb_score]
groupvar1b <- ce[QSTN_ID=="Q2_2",vis_bin]
nvar1b <- ce[QSTN_ID=="Q2_2",USER_COUNT]
#plot itself
plot1b <- ggplot(data=pdata1b,aes(y=py1b,x=as.factor(px1b),fill=as.factor(groupvar1b))) + 
  geom_bar(stat="identity", width = .7, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
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
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
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
  scale_fill_brewer(palette = 1, guide=FALSE) + theme_economist() +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  xlab(xlabel) + ylab(ylabel) + labs(caption=caption) +
  geom_text(size = 2.5, aes(label=py3,y=0), angle=90, hjust=-0.25, stat="identity", position = position_dodge(0.7))

#set labels
xlabel <- "SR Customers"
xlabels <- c("Speed","CC","Above & Beyond","Order Accuracy","Bev Taste","Food Taste","Cleanliness","Worth")
ylabel <- "Top Box Score"
tlabel <- "Customer Experience Survey"
sublabel <- "Customer Experience"
caption <- "Customer Experience Survey, December FY18"
#manual legend labels
lname <- "30-Day Visitation"
llabels <- c("1-5", "6-10", "11-15", "16+")
#values
pdata4 <- ce
px4 <- ce[,QSTN_ID]
py4 <- ce[,tb_score]
groupvar4 <- ce[,vis_bin]
#plot itself
plot4 <- ggplot(data=pdata4,aes(y=py4,x=as.factor(px4),fill=as.factor(groupvar4))) + 
  geom_bar(stat="identity", width = 0.7, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 2, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 2.5, aes(label=py4,y=0), angle=90, hjust=-0.25, stat="identity", position = position_dodge(0.7))
#combine into one plot
plot5 <- plot2 / plot3 / plot4
print(plot5)


##chart of High freq (6+) SR, non-SR from brand equity
hfnames <- c("2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")
hfsr <- c(0.316734235763449,0.400199086,0.354659141,0.338949766,0.347741418,0.386038913,0.403829059,0.357128164,0.349871869,0.375000117,0.349929275,0.364159199)
hfnsr <- c(0.263260941976257,0.33346994,0.244993283,0.304362228,0.301443643,0.258287159,0.306139404,0.262061505,0.247021047,0.272584936,0.287737543,0.32874728)


#bring in our data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_customer_frequency.csv")

#keep only question 2
ce <- ce[QSTN_ID=="Q2_2"]

#create frequency bins (1: 1-5, 2: 6+)
ce[TRANS<=5, hf6plus := 0]
ce[TRANS>=6, hf6plus := 1]
#keep only high freq
ce <- ce[hf6plus==1]

#aggregate by vis_bin and question
ce <- ce[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
                TB_COUNT = sum(TB_COUNT,na.rm=T),
                RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T)),
         by=c("hf6plus","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM")]

#calculate TB score
ce[, tb_score := round((TB_COUNT / RSPSN_COUNT),3)]

#sort by year and month
setorder(ce,FSCL_YR_NUM,FSCL_PER_IN_YR_NUM)

#attach columns from brand equity
cebe <- cbind(ce,hfnames,hfsr,hfnsr)
setDT(cebe)
cebe <- cebe[,c("hfnames","tb_score","hfsr","hfnsr"),with=FALSE]

#melt by pop
cebe <- melt(cebe,id.var="hfnames")

#set labels
xlabel <- "Time"
ylabel <- "CC Score"
sublabel <- "Customer Connection Trend"
tlabel <- "Higher Frequency Customers (6+ visits/month)"
caption <- "Customer Experience Survey and Brand Equity Surveys, Jan-Dec 2017"
#manual legend labels
lname <- "Survey Group"
llabels <- c("Customer Experience Survey","Brand Equity: SR","Brand Equity: Non-SR")
#values
pdata <- cebe
px <- cebe[,hfnames]
py <- cebe[,value]
groupvar <- cebe[,variable]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab("") + ylab(ylabel) + 
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks), labels = waiver()) +
  scale_y_continuous(limits=c(0,pdata[,max(py)])) + theme_economist() +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

