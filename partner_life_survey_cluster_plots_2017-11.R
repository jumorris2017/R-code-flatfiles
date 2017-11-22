##Analysis of Partner Life Survey Results:
###including, descriptives and correlations of identified clusters
##Note: use after "partner_life_recoding_descriptives.R"
##Author: Julie Morris

##load libraries
library(foreign)
library(data.table)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(eeptools)
library(tableone)
library(ReporteRs)
library(magrittr)
library(PerformanceAnalytics)

##set directories
plot_dir <- ("C:/Users/jumorris/Desktop/temp_plots/temp_111417/")

##plots
#clusdt <- copy(db)

#Question 1_1: NUMBER_OF_JOBS
#subset to vars of interest
vars <- c("role_prev","more_than_one_job","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(more_than_one_job, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#set up unique elements
maintitle <- "Question 1_1: How many jobs do you currently have, including Starbucks? \n- More than 1 job"
ylabel <- "% More than 1 job"
legendtitle <- "Store Role"
xvar <- DT[,role_prev]
groupvar <- DT[,Store_Role]
yvar <- DT[,value]
yvarcount <- DT[,nval]
pdata <- DT
#plot
plot1 <- ggplot(data = pdata, aes(x = xvar, y = yvar*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill = groupvar)) + theme_bw() + 
  ggtitle(maintitle) + 
  scale_y_continuous(limits=c(0,100)) + labs(fill=legendtitle) +
  geom_text(size = 2, aes(label=round(yvarcount*yvar,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",yvarcount),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(yvar*100,0),"%"),y=yvar*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = ylabel) 
#create PDF file
filename <- (paste0(plot_dir, "clust_que1_1.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 1_3: PRIMARY_JOB
#subset to vars of interest
vars <- c("role_prev","sbux_primary","Store_Role")
db2 <- clusdt[more_than_one_job==1, vars, with=FALSE]
#lapply to create percentages
db2<- db2[, list(pval = mean(sbux_primary, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 1_3: Which do you consider your primary job? - Starbucks") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Starbucks primary job") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que1_3.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

###Question 1_3: PRIMARY_JOB REASON - non-starbucks jobs
#subset to vars of interest
db2 <- clusdt[sbux_primary==0]
vars <- c("role_prev","Store_Role",
          "prijob_career","prijob_income","prijob_health","prijob_retire",
          "prijob_hours","prijob_company","prijob_advance")
db2 <- na.omit(db2[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Working towards my long-term career",
                       "My main source of income",
                       "Provides me with healthcare benefits",
                       "Provides me with retirement benefits",
                       "Provides consistent hours",
                       "It's at a reputable company", 
                       "Offers career advancement opportunities")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE)
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que1_3nonsbux.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 1_3 \n Why do you consider another job your as your primary job?"))
graphics.off()

###Question 1_3: PRIMARY_JOB REASON - starbucks
#subset to vars of interest
db2 <- clusdt[sbux_primary==1]
vars <- c("role_prev","Store_Role",
          "prijob_career","prijob_income","prijob_health","prijob_retire",
          "prijob_hours","prijob_company","prijob_advance")
db2 <- na.omit(db2[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Working towards my long-term career",
                       "My main source of income",
                       "Provides me with healthcare benefits",
                       "Provides me with retirement benefits",
                       "Provides consistent hours",
                       "It's at a reputable company", 
                       "Offers career advancement opportunities")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que1_3sbux.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 1_3 \n Why do you consider Starbucks as your primary job?"))
graphics.off()

#Question 1_5: IF_MORE_HOURS
#subset to vars of interest
vars <- c("role_prev","more_hours_keep_oth_job","Store_Role")
db2 <- clusdt[more_than_one_job==1, vars, with=FALSE]
#lapply to create percentages
db2<- db2[, list(pval = mean(more_hours_keep_oth_job, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 1_5: If you were able to get more hours at Starbucks, would you still \nwork your other job(s)? - Yes") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Keep other job(s)") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que1_5.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

###Question 1_9: PREVENT_HOURS
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "prev_hours_few_mgr","prev_hours_many_mgr","prev_hours_not_avail","prev_hours_need_money",
          "prev_hours_sched")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2 <- db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("My manager doesn't schedule enough hours for me",
                       "My manager schedules too many hours for me",
                       "I'm not available to work more hours",
                       "I work more hours than I want because I need the money",
                       "My schedule doesn't work with the available shifts")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Completely Agree") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que1_9.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 1_9 \n In your opinion, what prevents you from getting the number of hours \n you want in a week at Starbucks? - Completely agree"))
graphics.off()

#Question 2_1: schedule consistency
#subset to vars of interest
vars <- c("role_prev","sched_very_consistent","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(sched_very_consistent, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 2_1: How would you rate the consistency of your schedule at Starbucks \nweek to week?") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Very consistent") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que2_1.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 2_2: schedule consistency importance
#subset to vars of interest
vars <- c("role_prev","sched_consis_imp","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(sched_consis_imp, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 2_2: How important is schedule consistency to you? - Very important") + 
  scale_y_continuous(limits=c(0,100)) +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Very important") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que2_2.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 2_4: schedule consistency description
#reshape data.table
DT <- clusdt[!is.na(sched_consis_without_other)] %>%
  group_by(role_prev,sched_consis_without_other) %>%
  summarise(count=n()) %>%
  mutate(percent=(count/sum(count))*100)
#plot
plot1 <- ggplot(data=DT,aes(x=role_prev, y=percent, fill=sched_consis_without_other)) + theme_bw() +
  geom_bar(stat="identity", position=position_fill(reverse=TRUE)) + 
  labs(x = "Cluster", y= "Percent") +
  ggtitle("Question 2_4: Schedule Consistency Decription") +
  geom_text(size = 2,aes(label=round(percent,0)),vjust=1,position=position_fill(reverse=TRUE)) +
  scale_y_continuous(breaks=c(.2,.4,.6,.8,1),labels=c("20","40","60","80","100")) +
  scale_fill_discrete(name= "Which of the following is most important to you \nin a consistent schedule?", guide = guide_legend(reverse=TRUE))
#create PDF file
filename <- (paste0(plot_dir, "clust_que2_4.png"))
png(filename, width = 8, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

###Question 3_1: Reason_JOIN
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "reason_join_brand","reason_join_cap","reason_join_career","reason_join_cust",
          "reason_join_flex_sched","reason_join_fun","reason_join_health",
          "reason_join_oth_benef","reason_join_pay","reason_join_prodbev","reason_join_values")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Brand Reputation","College Achievement Plan","Career Advancement Opportunities",
                       "Opportunity to connect with customers","Flexbility of schedule",
                       "Seemed like a fun place to work","Health coverage","Other financial benefits",
                       "Pay","Products (beverages, food, etc.)","Mission and values")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que3_1.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 3_1: \n Which of the following factors most influenced \n your decision to work for Starbucks?"))
graphics.off()

#Question 3_2: Store Choice
#reshape data.table
DT <- clusdt[!is.na(store_choice_without_other)] %>%
  group_by(role_prev,store_choice_without_other) %>%
  summarise(count=n()) %>%
  mutate(percent=(count/sum(count))*100)
#plot
plot1 <- ggplot(data=DT,aes(x=role_prev, y=percent, fill=store_choice_without_other)) + theme_bw() +
  geom_bar(stat="identity", position=position_fill(reverse=TRUE)) + 
  labs(x = "Cluster", y= "Percent") +
  ggtitle("Question 3_2: Store Choice") +
  geom_text(size = 2,aes(label=round(percent,0)),vjust=1,position=position_fill(reverse=TRUE)) +
  scale_y_continuous(breaks=c(.2,.4,.6,.8,1),labels=c("20","40","60","80","100")) +
  scale_fill_discrete(name= "Primary reason chose this store", guide = guide_legend(reverse=TRUE))
#create PDF file
filename <- (paste0(plot_dir, "clust_que3_2.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

###Question 3_5: Agree with
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "agree_true_self","agree_reco_great","agree_great_team","agree_decisions",
          "agree_problem_solve")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Starbucks allows me to be my true self at work",
                       "I recommend my store as a great place to work",
                       "I am part of a great team",
                       "I am able to make decisions about how my work gets done",
                       "I am able to creatively solve problems at work")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Completely Agree") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que3_5.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 3_5 \n How much do you agree with the following statements? - Completely agree"))
graphics.off()

###Question 3_8: TEAM_DIVERSITY
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "team_like_me","team_perspectives","team_spend_time","team_work_well")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Are just like me",
                       "Bring diverse perspectives",
                       "Spend time together outside of work",
                       "Work well together")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Completely Agree") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que3_8.png"))
png(filename, width = 8, height = 9, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 3_8 \n How much do you agree or disagree that each of \n the following items describe the partners on your team? - Completely agree"))
graphics.off()

###Question 4_1: JOB_CHARACTERISTICS
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "job_char_diff","job_char_fun","job_char_stress","job_char_overwhelm",
          "job_char_exciting","job_char_social")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Difficult","Fun","Stressful","Overwhelming","Exciting","Social")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Completely Agree") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que4_1.png"))
png(filename, width = 8, height = 10, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 4_1 \n How much do you agree or disagree that each of \n the following items describe your job? - Completely agree"))
graphics.off()

###Question 4_2a: products proud of making
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "prod_proud_bakery","prod_proud_bfsandwich","prod_proud_brewed",
          "prod_proud_espresso","prod_proud_frapp","prod_proud_lunch",
          "prod_proud_none","prod_proud_packaged","prod_proud_refresh",
          "prod_proud_reserve","prod_proud_icetea")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Bakery items","Breakfast sandwiches","Brewed coffee",
                       "Espresso-based beverages","Frappuccino","Lunch items",
                       "None of these","Packaged food","Refreshers",
                       "Reserve coffee","Shaken iced tea")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que4_2a.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 4_2(a): \n Which products are you proud of?"))
graphics.off()

###Question 4_2b: products enjoy making
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "prod_enjoy_bakery","prod_enjoy_bfsandwich","prod_enjoy_brewed",
          "prod_enjoy_espresso","prod_enjoy_frapp","prod_enjoy_lunch",
          "prod_enjoy_none","prod_enjoy_packaged","prod_enjoy_refresh",
          "prod_enjoy_reserve","prod_enjoy_icetea")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Bakery items","Breakfast sandwiches","Brewed coffee",
                       "Espresso-based beverages","Frappuccino","Lunch items",
                       "None of these","Packaged food","Refreshers",
                       "Reserve coffee","Shaken iced tea")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que4_2b.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 4_2(b): \n Which do you enjoy making?"))
graphics.off()

###Question 4_2c: products do not enjoy making
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "prod_notenjoy_bakery","prod_notenjoy_bfsandwich","prod_notenjoy_brewed",
          "prod_notenjoy_espresso","prod_notenjoy_frapp","prod_notenjoy_lunch",
          "prod_notenjoy_none","prod_notenjoy_packaged","prod_notenjoy_refresh",
          "prod_notenjoy_reserve","prod_notenjoy_icetea")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Bakery items","Breakfast sandwiches","Brewed coffee",
                       "Espresso-based beverages","Frappuccino","Lunch items",
                       "None of these","Packaged food","Refreshers",
                       "Reserve coffee","Shaken iced tea")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que4_2c.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 4_2(c): \n Which products do you NOT enjoy making?"))
graphics.off()


###Question 5_11: PRIORITIES
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "prio_fit","prio_car","prio_vol","prio_env",
          "prio_expfam","prio_fisav","prio_fistab","prio_love",
          "prio_career","prio_growth","prio_faith","prio_fun",
          "prio_house","prio_debt","prio_pets","prio_educ",
          "prio_hobbies","prio_sigoth","prio_family","prio_friends",
          "prio_carefam","prio_travel","prio_other")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Being healthy/physically fit","Buying a car","Community involvement/volunteering",
                       "Environmental/social issues",
                       "Expanding my family","Financial savings goals",
                       "Financial stability","Finding love/getting married",
                       "Focusing on my job/career",
                       "Focusing on personal growth/goals","Focusing on spirituality/faith","Having fun",
                       "Improving/changing my housing situation",
                       "Paying down debt","Pet(s)","Pursuing education","Pursuing hobbies",
                       "Relationship with my significant other",
                       "Spending time with family","Spending time with friends","Taking care of my family/loved ones",
                       "Traveling","Other")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 8, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que5_11.png"))
png(filename, width = 14, height = 14, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 5_11: \n What are your top priorities in life right now?"))
graphics.off()


###Question 6_1: SUPERPOWERS
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "super_craft","super_create","super_depend","super_emp",
          "super_enth","super_humor","super_init","super_mem",
          "super_optim","super_org","super_pat","super_pres",
          "super_prob","super_warmth","super_idk")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Craft","Creativity","Dependability","Empathy",
                       "Enthusiasm","Humor","Initiative","Memory",
                       "Optimisim","Organization","Patience","Presence",
                       "Problem Solving","Warmth","I don't know my Superpower")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que6_1.png"))
png(filename, width = 14, height = 14, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 6_1: \n What are your Starbucks Superpowers?"))
graphics.off()


###Question 6_4: AGREE_WITH
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "agree_worry_money","agree_optimistic","agree_better_parents","agree_better_bc_sbux",
          "agree_dev_talents", "agree_thinker")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("I worry about having enough money",
                       "I am optimistic about the future",
                       "I believe I will be better off than my parents",
                       "I believe I will have a better future because I work at Starbucks",
                       "My talents can be developed",
                       "I am an original, creative thinker")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Completely Agree") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que6_4.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 6_4: \n How much do you agree or disagree with each of the following statements? - Completely agree"))
graphics.off()

###Question 6_5: money worries
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "money_lack_savings","money_retire",
          "money_expenses","money_othbills","money_lack_credcard",
          "money_rent","money_studloans","money_unexpected","money_none")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Lack of savings",
                       "Not being able to have enough money to retire",
                       "Not having enough money to cover your expenses",
                       "Paying other bills such as utilities, medical, or legal",
                       "Paying your credit card bill",
                       "Paying your rent or mortgage",
                       "Paying your student loans",
                       "Unexpected expenses","I didn't worry about any of these")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que6_5.png"))
png(filename, width = 14, height = 17, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 6_5: \n In the past month, did you every worry about:"))
graphics.off()

#Question 6_6: making ends meet
#subset to vars of interest
vars <- c("role_prev","hard_ends_meet","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(hard_ends_meet, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 6_6: How much do you agree or disagree that an unexpected event in \nthe past 2 months made it harder for you to make ends meet?") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Completely Agree") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que6_6.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 8_1: marital status
#subset to vars of interest
vars <- c("role_prev","marital_married_part","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(marital_married_part, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 8_1: What is your marital status? - Married/Remarried or Living with Partner") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Married/Remarried or Living with Partner") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que8_1.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

###Question 8_2: HOUSEHOLD_MEMBERS
#subset to vars of interest
vars <- c("role_prev","Store_Role",
          "house_child","house_extf","house_alone","house_parents",
          "house_roommate","house_spouse","house_other")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2[, nval := .N, by=c("role_prev","Store_Role")]
db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by=c("role_prev","Store_Role","nval")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plyr::mapvalues to question name
vars_to_match <- unique(DT[,variable])
new_vec_of_values <- c("Child", "Extended family", "No one else - I live alone",
                       "Parent(s)", "Roommate/friend","Spouse/Significant other","Other")
DT[, que_name :=  plyr::mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
#plot
plotlist <- list()
for(i in vars[-(1:2)]) {
  plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = role_prev, y = value*100)) +
    geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
    ggtitle(DT[variable==i,que_name]) + 
    scale_y_continuous(limits=c(0,100)) +
    geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
    geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
    geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
          plot.title = element_text(size = 10, face = "bold")) + 
    labs(x = NULL, y = "% Selected") + scale_fill_discrete(guide=FALSE) 
}
#create PDF file
filename <- (paste0(plot_dir, "clust_que8_2.png"))
png(filename, width = 10, height = 12, units = "in", res = 100)
do.call("grid.arrange", c(plotlist, top = "Question 8_2 \n Besides you, who else lives in your household?"))
graphics.off()

#Question 8_4: INCOME_CONTRIBUTION
#subset to vars of interest
vars <- c("role_prev","Q8_4_Primary_Income_Flag","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(Q8_4_Primary_Income_Flag, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 8_4: Monetarily, how much does your income from your current job(s) \ncontribute to your household expenses? - Providing primary income") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Providing primary income") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que8_4.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 8_5: HEALTH_COVERAGE
#subset to vars of interest
vars <- c("role_prev","health_sbux","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(health_sbux, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 8_5: Which of the following best describes your primary health coverage? \n- Through Starbucks") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Through Starbucks") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que8_5.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 8_6: STUDENT_STATUS
#subset to vars of interest
vars <- c("role_prev","student","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(student, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 8_6: What is your current status as a student? - Student") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Student") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que8_6.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 8_7: EDUCATION_LEVEL
#subset to vars of interest
vars <- c("role_prev","educ_highschool","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(educ_highschool, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 8_7: What is the highest level of education you have completed? \n- High School Diploma +") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% High School Diploma +") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que8_7hs.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question 8_7: EDUCATION_LEVEL
#subset to vars of interest
vars <- c("role_prev","educ_bach","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(educ_bach, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Question 8_7: What is the highest level of education you have completed? \n- Bachelor's Degree +") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% Bachelor's Degree +") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que8_7bach.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question: KIDS_FLAG
#subset to vars of interest
vars <- c("role_prev","Kids_Flag","Store_Role")
db2 <- na.omit(clusdt[, vars, with=FALSE])
#lapply to create percentages
db2<- db2[, list(pval = mean(Kids_Flag, na.rm=T), 
                         nval=.N), by=c("role_prev","Store_Role")]
#reshape from wide to long
DT <- melt(db2, id=c("role_prev","Store_Role","nval"))
#plot
plot1 <- ggplot(data=DT, aes(x = role_prev, y = value*100)) +
  geom_bar(stat="identity", width = 0.7, aes(fill=Store_Role)) + theme_bw() + 
  ggtitle("Do you have kids? - Yes") + 
  scale_y_continuous(limits=c(0,100)) + labs(fill="Store Role") +
  geom_text(size = 2, aes(label=round(nval*value,0),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2, aes(label=paste0("n=",nval),y=0), stat= "identity", vjust = 1) +
  geom_text(size = 3, aes(label=paste0(round(value*100,0),"%"),y=value*100), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = "% With kids") 
#create PDF file
filename <- (paste0(plot_dir, "clust_que_kids.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question: AGE ROLLUP
#reshape data.table
DT <- clusdt[!is.na(Age_Rollup)] %>%
  group_by(role_prev,Age_Rollup) %>%
  summarise(count=n()) %>%
  mutate(percent=(count/sum(count))*100)
#plot
plot1 <- ggplot(data=DT,aes(x=role_prev, y=percent, fill=Age_Rollup)) + theme_bw() +
  geom_bar(stat="identity", position=position_fill(reverse=TRUE)) + 
  labs(x = "Cluster", y= "Percent") +
  ggtitle("Age group") +
  geom_text(size = 2,aes(label=round(percent,0)),vjust=1,position=position_fill(reverse=TRUE)) +
  scale_y_continuous(breaks=c(.2,.4,.6,.8,1),labels=c("20","40","60","80","100")) +
  scale_fill_discrete(name= "Age group", guide = guide_legend(reverse=TRUE))
#create PDF file
filename <- (paste0(plot_dir, "clust_que_age_rollup.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question: TENURE ROLLUP
#reshape data.table
DT <- clusdt[!is.na(Tenure_Rollup)] %>%
  group_by(role_prev,Tenure_Rollup) %>%
  summarise(count=n()) %>%
  mutate(percent=(count/sum(count))*100)
#plot
plot1 <- ggplot(data=DT,aes(x=role_prev, y=percent, fill=Tenure_Rollup)) + theme_bw() +
  geom_bar(stat="identity", position=position_fill(reverse=TRUE)) + 
  labs(x = "Cluster", y= "Percent") +
  ggtitle("Tenure") +
  geom_text(size = 2,aes(label=round(percent,0)),vjust=1,position=position_fill(reverse=TRUE)) +
  scale_y_continuous(breaks=c(.2,.4,.6,.8,1),labels=c("20","40","60","80","100")) +
  scale_fill_discrete(name= "Tenure", guide = guide_legend(reverse=TRUE))
#create PDF file
filename <- (paste0(plot_dir, "clust_que_tenure_rollup.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()

#Question: HEALTHCARE COVERAGE FULL LIST OF OPTIONS
#reshape data.table
DT <- clusdt[!is.na(Q8_5_Health_Coverage)] %>%
    group_by(role_prev,Q8_5_Health_Coverage) %>%
    summarise(count=n()) %>%
    mutate(percent=(count/sum(count))*100)
#plot
plot1 <- ggplot(data=DT,aes(x=role_prev, y=percent, fill=Q8_5_Health_Coverage)) + theme_bw() +
  geom_bar(stat="identity", position=position_fill(reverse=TRUE)) + 
  labs(x = "Cluster", y= "Percent") +
  ggtitle("Health Coverage") +
  geom_text(size = 2,aes(label=round(percent,0)),vjust=1,position=position_fill(reverse=TRUE)) +
  scale_y_continuous(breaks=c(.2,.4,.6,.8,1),labels=c("20","40","60","80","100")) +
  scale_fill_discrete(name= "Health Coverage", guide = guide_legend(reverse=TRUE))
#create PDF file
filename <- (paste0(plot_dir, "clust_que_healthcare_rollup.png"))
png(filename, width = 6.5, height = 6.5, units = "in", res = 100)
print(plot1)
graphics.off()


###create table 1
#create Table 1; library(tableone)
##subset to only variales utilized for analysis, to make a more reasonably-sized dt
binaryvars <- c("more_than_one_job","sbux_primary","prijob_career","prijob_income",
                "prijob_health","prijob_retire","prijob_hours","prijob_company","prijob_advance",
                "more_hours_keep_oth_job","prev_hours_few_mgr","prev_hours_many_mgr","prev_hours_not_avail",
                "prev_hours_need_money","prev_hours_sched","sched_very_consistent","sched_consis_imp",
                "reason_join_brand","reason_join_cap","reason_join_career","reason_join_cust",
                "reason_join_flex_sched","reason_join_fun","reason_join_health",
                "reason_join_oth_benef","reason_join_pay","reason_join_prodbev","reason_join_values",
                "agree_true_self","agree_reco_great","agree_great_team","agree_decisions",
                "agree_problem_solve","team_like_me","team_perspectives","team_spend_time","team_work_well",
                "job_char_diff","job_char_fun","job_char_stress","job_char_overwhelm",
                "job_char_exciting","job_char_social","prod_proud_bakery","prod_proud_bfsandwich","prod_proud_brewed",
                "prod_proud_espresso","prod_proud_frapp","prod_proud_lunch","prod_proud_none",
                "prod_proud_packaged","prod_proud_refresh","prod_proud_reserve","prod_proud_icetea","prod_enjoy_bakery",
                "prod_enjoy_bfsandwich","prod_enjoy_brewed","prod_enjoy_espresso","prod_enjoy_frapp","prod_enjoy_lunch",
                "prod_enjoy_none","prod_enjoy_packaged","prod_enjoy_refresh","prod_enjoy_reserve","prod_enjoy_icetea",
                "prod_notenjoy_bakery","prod_notenjoy_bfsandwich","prod_notenjoy_brewed","prod_notenjoy_espresso",
                "prod_notenjoy_frapp","prod_notenjoy_lunch","prod_notenjoy_none","prod_notenjoy_packaged","prod_notenjoy_refresh",
                "prod_notenjoy_reserve","prod_notenjoy_icetea","agree_worry_money","agree_optimistic",
                "agree_better_parents","agree_better_bc_sbux","agree_dev_talents", "agree_thinker","money_lack_savings","money_retire",
                "money_expenses","money_othbills","money_lack_credcard","money_rent","money_studloans","money_unexpected",
                "money_none","hard_ends_meet","marital_married_part","house_child","house_extf","house_alone","house_parents",
                "house_roommate","house_spouse","house_other","Q8_4_Primary_Income_Flag","health_sbux","student",
                "educ_highschool","educ_bach","Kids_Flag")
contvars <- c("Q1_7_Starbucks_Hours_Worked_Remove_Outliers","Q1_8_Starbucks_Ideal_Hours_Remove_Outliers",
              "Hours_Delta_Ideal_From_Stated","age")
catvars <- c("Store_Role","sched_consis_without_other","store_choice_without_other","Tenure_Rollup")
###
varstokeep <- c(catvars,binaryvars,contvars)
DT <- clusdt[,varstokeep,with=FALSE]

#Stratified by cluster
#Remove Store Role from list of vars
table1 <- CreateTableOne(varstokeep[2:length(varstokeep)], dt, catvars, strata = catvars[1])
#export Table 1 to Microsoft Word; library(ReporteRs); library(magrittr)
table1 <- print(table1)
# The script
docx( ) %>% 
  addFlexTable(table1 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold( color = "white" ),
                           add.rownames = TRUE ) %>%
                 setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = (paste0(plot_dir, "clust_table1.docx")))

