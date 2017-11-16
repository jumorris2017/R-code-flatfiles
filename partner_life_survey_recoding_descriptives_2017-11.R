##Analysis of Partner Life Survey Results: 
###including recoding, descriptives, correlations, and identification of clusters
##Using "Partner Life_FinalData_CLEAN_1.sav"
##Author: Julie Morris

##load libraries
library(foreign)
library(data.table)
library(ggplot2)
library(gridExtra)
library(eeptools)
library(tableone)
library(ReporteRs)
library(magrittr)
library(PerformanceAnalytics)
library(NbClust)
library(cluster)
library(Hmisc)
library(xlsx)
library(tibble)
library(janitor)
library(dplyr)
library(tidyverse)

##set seed for clustering
set.seed(1234)

##write functions
#correlation matrix with p-values
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
#create function to dump the cor.prob output to a 4 column matrix
#with row/column indices, correlation, and p-value.
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
#convert SPSS date format into R date format
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")
#cluster plotting
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}


##read in SPSS dataset (as data.frame) and set as data.table
db <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/Partner Life_FinalData_CLEAN_1.sav", to.data.frame=TRUE)
setDT(db)
setnames(db,"PanelistIdQuestion","RID")
##read in tenure data
tenuredt <- read.csv("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/partner_tenure.csv", fileEncoding="UTF-8-BOM")
setDT(tenuredt)
#subset tenuredt to only active PartnerIDs for recoding, pre-mergin
pids <- unique(db[,PartnerID])
tenuredt <- tenuredt[PartnerID %in% pids]
#convert serial date format into R date format
tenuredt[, (names(select(tenuredt,contains("date")))) := lapply(.SD, function(x) excel_numeric_to_date(as.numeric(as.character(x), date_system = "modern"))),
         .SDcols = names(select(tenuredt,contains("date")))]
#calculate time in previous role
#assume baristas = 0
#assume shifts were baristas first
tenuredt[job_title==11&prev_job_title==10, prevrole_days := difftime(job_date,hire_date,units="days")]
#create binary internal/external hire variable
tenuredt[job_title==14&prev_job_title==15, external_hire := 1]
tenuredt[job_title==14&prev_job_title!=15, external_hire := 0]
# #calculate for store managers who were RMT's previously
# tenuredt[job_title==14&prev_job_title==15, prevrole_days := difftime(job_date,hire_date,units="days")]
# #calculate for store managers who were shifts previously
# tenuredt[job_title==14&prev_job_title==11, prevrole_days := difftime(job_date,hire_date,units="days")]
tenuredt[, prevrole_days := as.numeric(prevrole_days)]
tenuredt[, prevrole_months := round(prevrole_days/30.4167,0)]
tenuredt[, prevrole_years := round(prevrole_days/365,1)]
#merge in tenure data
db <- left_join(db,tenuredt,by="PartnerID")
setDT(db)
#load open-end data
oedt <- fread("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/Open End Codes/partner_life_verbatims_coded.csv")
oedt[, V1 := NULL]
oedt <- oedt[,names(oedt)[-(2:8)],with=FALSE]
#merge in oe data
db <- left_join(db,oedt,by="RID")


#convert SPSS date format into R date format
db[, dateofbirth := spss2date(db[,DOB])]
db[, age := floor(age_calc(db[,dateofbirth], units = "years"))]

##recode store role for clustering
db[Store_Role=="Barista", role := 1]
db[Store_Role=="Shift supervisor", role := 2]
db[Store_Role=="Store manager", role := 3]
# #
##recode tenure rollup for clustering
db[Tenure_Rollup=="Less than 3 months", tenure := 1]
db[Tenure_Rollup=="At least 3, but less than 6 months", tenure := 2]
db[Tenure_Rollup=="At least 6, but less than 12 months", tenure := 3]
db[Tenure_Rollup=="At least 1 year, but less than 2 years", tenure := 4]
db[Tenure_Rollup=="At least 2 years, but less than 3 years", tenure := 5]
db[Tenure_Rollup=="At least 3 years, but less than 5 years", tenure := 6]
db[Tenure_Rollup=="At least 5 years, but less than 10 years", tenure := 7]
db[Tenure_Rollup=="10 or more years", tenure := 8]
# #
##fill in missing data using Tenure_Rollup. Arbitrary decision: first month of next category.
db[is.na(job_months)&Tenure_Rollup=="Less than 3 months", job_months := 1]
db[is.na(job_months)&Tenure_Rollup=="At least 3, but less than 6 months", job_months := 4]
db[is.na(job_months)&Tenure_Rollup=="At least 6, but less than 12 months", job_months := 7]
db[is.na(job_months)&Tenure_Rollup=="At least 1 year, but less than 2 years", job_months := 13]
db[is.na(job_months)&Tenure_Rollup=="At least 2 years, but less than 3 years", job_months := 25]
db[is.na(job_months)&Tenure_Rollup=="At least 3 years, but less than 5 years", job_months := 37]
db[is.na(job_months)&Tenure_Rollup=="At least 5 years, but less than 10 years", job_months := 61]
db[is.na(job_months)&Tenure_Rollup=="10 or more years", job_months := 121]
##tenure
##split into categories for clustering
prob = c(0.25, .5, 0.75, 1)
temp <- db %>% group_by(Store_Role) %>% summarise( 
    ten25 = quantile(job_months, probs = prob[1], na.rm = T), 
    ten50 = quantile(job_months, probs = prob[2], na.rm = T),
    ten75 = quantile(job_months, probs = prob[3], na.rm = T), 
    ten100 = quantile(job_months, probs = prob[4], na.rm = T)
  )
db <- left_join(db, temp,by="Store_Role")
setDT(db)
#recode tenure based on quartiles
db[job_months <= ten25, tenure := 1]
db[job_months > ten25 & job_months <= ten50, tenure := 2]
db[job_months > ten50 & job_months <= ten75, tenure := 3]
db[job_months > ten75 & job_months <= ten100, tenure := 4]
##create new var that makes unique tenure and role pairings
db[tenure==1, role_ten := paste0(role,"-","Q1")]
db[tenure==2, role_ten := paste0(role,"-","Q2")]
db[tenure==3, role_ten := paste0(role,"-","Q3")]
db[tenure==4, role_ten := paste0(role,"-","Q4")]
#recode tenure_groups_groups based on groups
db[job_months <= 12, tenure_groups := 1]
db[job_months > 12 & job_months <= 24, tenure_groups := 2]
db[job_months > 24 & job_months <= 36, tenure_groups := 3]
db[job_months > 36, tenure_groups := 4]
##create new var that makes unique tenure_groups and role pairings
db[tenure_groups==1, role_ten_groups := paste0(role,": ","<1y")]
db[tenure_groups==2, role_ten_groups := paste0(role,": ","1-2y")]
db[tenure_groups==3, role_ten_groups := paste0(role,": ","2-3y")]
db[tenure_groups==4, role_ten_groups := paste0(role,": ","3+y")]
##age
##split into categories for clustering
prob = c(0.25, .5, 0.75, 1)
temp <- db %>% group_by(Store_Role) %>% summarise( 
    age25 = quantile(age, probs = prob[1], na.rm = T), 
    age50 = quantile(age, probs = prob[2], na.rm = T),
    age75 = quantile(age, probs = prob[3], na.rm = T), 
    age100 = quantile(age, probs = prob[4], na.rm = T)
  )
db <- left_join(db, temp,by="Store_Role")
setDT(db)
#recode ageure based on quartiles
db[age <= age25, ageure := 1]
db[age > age25 & age <= age50, ageure := 2]
db[age > age50 & age <= age75, ageure := 3]
db[age > age75 & age <= age100, ageure := 4]
##create new var that makes unique tenure and role pairings
db[ageure==1, role_age := paste0(role,"-","Q1")]
db[ageure==2, role_age := paste0(role,"-","Q2")]
db[ageure==3, role_age := paste0(role,"-","Q3")]
db[ageure==4, role_age := paste0(role,"-","Q4")]
#recode ageure based on arbitrary cut-offs
db[age <= 24, age_groups := 1]
db[age > 24 & age <= 29, age_groups := 2]
db[age > 29 & age <= 34, age_groups := 3]
db[age > 34, age_groups := 4]
##create new var that makes unique tenure and role pairings
db[age_groups==1, role_age_groups := paste0(role,"-","<25")]
db[age_groups==2, role_age_groups := paste0(role,"-","25-29")]
db[age_groups==3, role_age_groups := paste0(role,"-","30-34")]
db[age_groups==4, role_age_groups := paste0(role,"-","35+")]
##age
##time in previous job
prob = c(0.25, .5, 0.75, 1)
temp <- db %>% group_by(Store_Role) %>% summarise( 
  prev25 = quantile(job_months, probs = prob[1], na.rm = T), 
  prev50 = quantile(job_months, probs = prob[2], na.rm = T),
  prev75 = quantile(job_months, probs = prob[3], na.rm = T), 
  prev100 = quantile(job_months, probs = prob[4], na.rm = T)
)
db <- left_join(db, temp,by="Store_Role")
setDT(db)
#recode based on quartiles
db[job_months <= prev25, job_monthsure := 1]
db[job_months > prev25 & job_months <= prev50, job_monthsure := 2]
db[job_months > prev50 & job_months <= prev75, job_monthsure := 3]
db[job_months > prev75 & job_months <= prev100, job_monthsure := 4]
##create new var that makes unique tenure and role pairings
# db[job_monthsure==1, role_prev := paste0(role,"-","<25")]
# db[job_monthsure==2, role_prev := paste0(role,"-","25-29")]
# db[job_monthsure==3, role_prev := paste0(role,"-","30-34")]
# db[job_monthsure==4, role_prev := paste0(role,"-","35+")]
db[job_monthsure==1, role_prev := paste0("Sh","-","<10")]
db[job_monthsure==2, role_prev := paste0("Sh","-","10-20")]
db[job_monthsure==3, role_prev := paste0("Sh","-","21-39")]
db[job_monthsure==4, role_prev := paste0("Sh","-","40+")]

###recode binary variables as 0/1
#more than one job (yes=1;no=0)
db[Q1_1_Recode_Flag_MT1_Job=="No", more_than_one_job := 0]
db[Q1_1_Recode_Flag_MT1_Job=="Yes", more_than_one_job := 1]
#is starbucks your primary job (yes=1;no=0)
db[Q1_3_recode_Primary_Starbucks_Flag=="No", sbux_primary := 0]
db[Q1_3_recode_Primary_Starbucks_Flag=="Yes", sbux_primary := 1]
###reason primary job
#
db[Q1_4_Primary_Job_Reason_Workingtowardsmylongtermcareer=="Not Selected", prijob_career := 0]
db[Q1_4_Primary_Job_Reason_Workingtowardsmylongtermcareer=="Selected", prijob_career := 1]
#
db[Q1_4_Primary_Job_Reason_Mymainsourceofincome=="Not Selected", prijob_income := 0]
db[Q1_4_Primary_Job_Reason_Mymainsourceofincome=="Selected", prijob_income := 1]
#
db[Q1_4_Primary_Job_Reason_Providesmewithhealthcarebenefits=="Not Selected", prijob_health := 0]
db[Q1_4_Primary_Job_Reason_Providesmewithhealthcarebenefits=="Selected", prijob_health := 1]
#
db[Q1_4_Primary_Job_Reason_Providesmewithretirementbenefits=="Not Selected", prijob_retire := 0]
db[Q1_4_Primary_Job_Reason_Providesmewithretirementbenefits=="Selected", prijob_retire := 1]
#
db[Q1_4_Primary_Job_Reason_Providesconsistenthours=="Not Selected", prijob_hours := 0]
db[Q1_4_Primary_Job_Reason_Providesconsistenthours=="Selected", prijob_hours := 1]
#
db[Q1_4_Primary_Job_Reason_Itsatareputablecompany=="Not Selected", prijob_company := 0]
db[Q1_4_Primary_Job_Reason_Itsatareputablecompany=="Selected", prijob_company := 1]
#
db[Q1_4_Primary_Job_Reason_Offerscareeradvancementopportunities=="Not Selected", prijob_advance := 0]
db[Q1_4_Primary_Job_Reason_Offerscareeradvancementopportunities=="Selected", prijob_advance := 1]
#if able to get more hours at Starbucks, would you continue working other job? (yes=1;no=0)
db[Q1_5_Recode_If_More_Hours_Flag=="No", more_hours_keep_oth_job := 0]
db[Q1_5_Recode_If_More_Hours_Flag=="Yes", more_hours_keep_oth_job := 1]
##what prevents you from getting the number of hours you want at sbux
#manager schedules too few
db[Q1_9_Prevents_Hours_Mymanagerdoesntscheduleenoughhoursforme=="Not Selected", prev_hours_few_mgr := 0]
db[Q1_9_Prevents_Hours_Mymanagerdoesntscheduleenoughhoursforme=="Selected", prev_hours_few_mgr := 1]
#manager schedules too many
db[Q1_9_Prevents_Hours_Mymanagerschedulestoomanyhoursforme=="Not Selected", prev_hours_many_mgr := 0]
db[Q1_9_Prevents_Hours_Mymanagerschedulestoomanyhoursforme=="Selected", prev_hours_many_mgr := 1]
#not available for more
db[Q1_9_Prevents_Hours_Imnotavailabletoworkmorehours=="Not Selected", prev_hours_not_avail := 0]
db[Q1_9_Prevents_Hours_Imnotavailabletoworkmorehours=="Selected", prev_hours_not_avail := 1]
#I need the money
db[Q1_9_Prevents_Hours_IworkmorehoursthanIwantbecauseIneedthemoney=="Not Selected", prev_hours_need_money := 0]
db[Q1_9_Prevents_Hours_IworkmorehoursthanIwantbecauseIneedthemoney=="Selected", prev_hours_need_money := 1]
#My schedule doesn't work with the available shifts
db[Q1_9_Prevents_Hours_Myscheduledoesntworkwiththeavailableshifts=="Not Selected", prev_hours_sched := 0]
db[Q1_9_Prevents_Hours_Myscheduledoesntworkwiththeavailableshifts=="Selected", prev_hours_sched := 1]
#consistent work schedules (very consistent = 1; else = 0)
db[Q2_1_Schedule_Consistency=="Very consistent", sched_very_consistent := 1]
db[Q2_1_Schedule_Consistency=="Somewhat consistent", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Neutral", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Somewhat inconsistent", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Very inconsistent", sched_very_consistent := 0]
#schedule consistent importance (very consistent = 1; else = 0)
db[Q2_2_Consistency_Importance=="Very important", sched_consis_imp := 1]
db[Q2_2_Consistency_Importance=="Somewhat important", sched_consis_imp := 0]
db[Q2_2_Consistency_Importance=="Neutral", sched_consis_imp := 0]
db[Q2_2_Consistency_Importance=="Somewhat unimportant", sched_consis_imp := 0]
db[Q2_2_Consistency_Importance=="Not at all important", sched_consis_imp := 0]
#question 2_4: schedule consistency (remove "other" for plotting)
db[Q2_4_Consistency_Description!="Other", sched_consis_without_other := Q2_4_Consistency_Description]
db[Q2_4_Consistency_Description=="Other", sched_consis_without_other := NA]
###which factors most influenced decision to join starbucks
#brand reputation
db[Q3_1_Reason_Join_Brandreputation=="Not Selected", reason_join_brand := 0]
db[Q3_1_Reason_Join_Brandreputation=="Selected", reason_join_brand := 1]
#career advancement
db[Q3_1_Reason_Join_Careeradvancementopportunities=="Not Selected", reason_join_career := 0]
db[Q3_1_Reason_Join_Careeradvancementopportunities=="Selected", reason_join_career := 1]
#company culture
db[Q3_1_Reason_Join_Companyculture=="Not Selected", reason_join_culture := 0]
db[Q3_1_Reason_Join_Companyculture=="Selected", reason_join_culture := 1]
#flexible schedule
db[Q3_1_Reason_Join_Flexibilityofschedule=="Not Selected", reason_join_flex_sched := 0]
db[Q3_1_Reason_Join_Flexibilityofschedule=="Selected", reason_join_flex_sched := 1]
#health coverage
db[Q3_1_Reason_Join_Healthcoveragemedicaldentalvision=="Not Selected", reason_join_health := 0]
db[Q3_1_Reason_Join_Healthcoveragemedicaldentalvision=="Selected", reason_join_health := 1]
#connect with customers
db[Q3_1_Reason_Join_Opportunitytoconnectwithcustomers=="Not Selected", reason_join_cust := 0]
db[Q3_1_Reason_Join_Opportunitytoconnectwithcustomers=="Selected", reason_join_cust := 1]
#other benefits
db[Q3_1_Reason_Join_OtherfinancialbenefitsFutureRoast401kBeanStockS=="Not Selected", reason_join_oth_benef := 0]
db[Q3_1_Reason_Join_OtherfinancialbenefitsFutureRoast401kBeanStockS=="Selected", reason_join_oth_benef := 1]
#pay
db[Q3_1_Reason_Join_Pay=="Not Selected", reason_join_pay := 0]
db[Q3_1_Reason_Join_Pay=="Selected", reason_join_pay := 1]
#CAP
db[Q3_1_Reason_Join_StarbucksCollegeAchievementPlan=="Not Selected", reason_join_cap := 0]
db[Q3_1_Reason_Join_StarbucksCollegeAchievementPlan=="Selected", reason_join_cap := 1]
#mission and values
db[Q3_1_Reason_Join_StarbucksMissionandValues=="Not Selected", reason_join_values := 0]
db[Q3_1_Reason_Join_StarbucksMissionandValues=="Selected", reason_join_values := 1]
#products and beverages
db[Q3_1_Reason_Join_Starbucksproductsbeveragesfoodetc=="Not Selected", reason_join_prodbev := 0]
db[Q3_1_Reason_Join_Starbucksproductsbeveragesfoodetc=="Selected", reason_join_prodbev := 1]
#fun place
db[Q3_1_Reason_Join_Starbucksseemedlikeafunplacetowork=="Not Selected", reason_join_fun := 0]
db[Q3_1_Reason_Join_Starbucksseemedlikeafunplacetowork=="Selected", reason_join_fun := 1]
#question 3_2: store choice (remove "other" for plotting)
db[Q3_2_Store_Choice!="Other", store_choice_without_other := Q3_2_Store_Choice]
db[Q3_2_Store_Choice=="Other", store_choice_without_other := NA]
##agree with the following statements (completely agree = 1; else = 0)
#be true self
db[Q3_5_0=="Completely Agree", agree_true_self := 1]
db[Q3_5_0=="Somewhat Agree", agree_true_self := 0]
db[Q3_5_0=="Neither Agree nor Disagree", agree_true_self := 0]
db[Q3_5_0=="Somewhat Disagree", agree_true_self := 0]
db[Q3_5_0=="Completely Disagree", agree_true_self := 0]
#recommend store as great place to work
db[Q3_5_1=="Completely Agree", agree_reco_great := 1]
db[Q3_5_1=="Somewhat Agree", agree_reco_great := 0]
db[Q3_5_1=="Neither Agree nor Disagree", agree_reco_great := 0]
db[Q3_5_1=="Somewhat Disagree", agree_reco_great := 0]
db[Q3_5_1=="Completely Disagree", agree_reco_great := 0]
#great team
db[Q3_5_2=="Completely Agree", agree_great_team := 1]
db[Q3_5_2=="Somewhat Agree", agree_great_team := 0]
db[Q3_5_2=="Neither Agree nor Disagree", agree_great_team := 0]
db[Q3_5_2=="Somewhat Disagree", agree_great_team := 0]
db[Q3_5_2=="Completely Disagree", agree_great_team := 0]
#make decisions
db[Q3_5_3=="Completely Agree", agree_decisions := 1]
db[Q3_5_3=="Somewhat Agree", agree_decisions := 0]
db[Q3_5_3=="Neither Agree nor Disagree", agree_decisions := 0]
db[Q3_5_3=="Somewhat Disagree", agree_decisions := 0]
db[Q3_5_3=="Completely Disagree", agree_decisions := 0]
#creatively problem solve
db[Q3_5_4=="Completely Agree", agree_problem_solve := 1]
db[Q3_5_4=="Somewhat Agree", agree_problem_solve := 0]
db[Q3_5_4=="Neither Agree nor Disagree", agree_problem_solve := 0]
db[Q3_5_4=="Somewhat Disagree", agree_problem_solve := 0]
db[Q3_5_4=="Completely Disagree", agree_problem_solve := 0]
##agree with the following items describing partners on your team  (completely agree = 1; else = 0)
#are just like me
db[Q3_8_Team_Diversity_0=="Completely Agree", team_like_me := 1]
db[Q3_8_Team_Diversity_0=="Somewhat Agree", team_like_me := 0]
db[Q3_8_Team_Diversity_0=="Neither Agree nor Disagree", team_like_me := 0]
db[Q3_8_Team_Diversity_0=="Somewhat Disagree", team_like_me := 0]
db[Q3_8_Team_Diversity_0=="Completely Disagree", team_like_me := 0]
#bring diverse perspectives
db[Q3_8_Team_Diversity_1=="Completely Agree", team_perspectives := 1]
db[Q3_8_Team_Diversity_1=="Somewhat Agree", team_perspectives := 0]
db[Q3_8_Team_Diversity_1=="Neither Agree nor Disagree", team_perspectives := 0]
db[Q3_8_Team_Diversity_1=="Somewhat Disagree", team_perspectives := 0]
db[Q3_8_Team_Diversity_1=="Completely Disagree", team_perspectives := 0]
#spend time together outside of work
db[Q3_8_Team_Diversity_2=="Completely Agree", team_spend_time := 1]
db[Q3_8_Team_Diversity_2=="Somewhat Agree", team_spend_time := 0]
db[Q3_8_Team_Diversity_2=="Neither Agree nor Disagree", team_spend_time := 0]
db[Q3_8_Team_Diversity_2=="Somewhat Disagree", team_spend_time := 0]
db[Q3_8_Team_Diversity_2=="Completely Disagree", team_spend_time := 0]
#work well together
db[Q3_8_Team_Diversity_3=="Completely Agree", team_work_well := 1]
db[Q3_8_Team_Diversity_3=="Somewhat Agree", team_work_well := 0]
db[Q3_8_Team_Diversity_3=="Neither Agree nor Disagree", team_work_well := 0]
db[Q3_8_Team_Diversity_3=="Somewhat Disagree", team_work_well := 0]
db[Q3_8_Team_Diversity_3=="Completely Disagree", team_work_well := 0]
##agree with the following items describing partners on your team  (completely agree = 1; else = 0)
#difficult
db[Q4_1_Job_Characteristics_0=="Completely Agree", job_char_diff := 1]
db[Q4_1_Job_Characteristics_0=="Somewhat Agree", job_char_diff := 0]
db[Q4_1_Job_Characteristics_0=="Neither Agree nor Disagree", job_char_diff := 0]
db[Q4_1_Job_Characteristics_0=="Somewhat Disagree", job_char_diff := 0]
db[Q4_1_Job_Characteristics_0=="Completely Disagree", job_char_diff := 0]
#fun
db[Q4_1_Job_Characteristics_1=="Completely Agree", job_char_fun := 1]
db[Q4_1_Job_Characteristics_1=="Somewhat Agree", job_char_fun := 0]
db[Q4_1_Job_Characteristics_1=="Neither Agree nor Disagree", job_char_fun := 0]
db[Q4_1_Job_Characteristics_1=="Somewhat Disagree", job_char_fun := 0]
db[Q4_1_Job_Characteristics_1=="Completely Disagree", job_char_fun := 0]
#stressful
db[Q4_1_Job_Characteristics_2=="Completely Agree", job_char_stress := 1]
db[Q4_1_Job_Characteristics_2=="Somewhat Agree", job_char_stress := 0]
db[Q4_1_Job_Characteristics_2=="Neither Agree nor Disagree", job_char_stress := 0]
db[Q4_1_Job_Characteristics_2=="Somewhat Disagree", job_char_stress := 0]
db[Q4_1_Job_Characteristics_2=="Completely Disagree", job_char_stress := 0]
#overwhelming
db[Q4_1_Job_Characteristics_3=="Completely Agree", job_char_overwhelm := 1]
db[Q4_1_Job_Characteristics_3=="Somewhat Agree", job_char_overwhelm := 0]
db[Q4_1_Job_Characteristics_3=="Neither Agree nor Disagree", job_char_overwhelm := 0]
db[Q4_1_Job_Characteristics_3=="Somewhat Disagree", job_char_overwhelm := 0]
db[Q4_1_Job_Characteristics_3=="Completely Disagree", job_char_overwhelm := 0]
#exciting
db[Q4_1_Job_Characteristics_4=="Completely Agree", job_char_exciting := 1]
db[Q4_1_Job_Characteristics_4=="Somewhat Agree", job_char_exciting := 0]
db[Q4_1_Job_Characteristics_4=="Neither Agree nor Disagree", job_char_exciting := 0]
db[Q4_1_Job_Characteristics_4=="Somewhat Disagree", job_char_exciting := 0]
db[Q4_1_Job_Characteristics_4=="Completely Disagree", job_char_exciting := 0]
#social
db[Q4_1_Job_Characteristics_5=="Completely Agree", job_char_social := 1]
db[Q4_1_Job_Characteristics_5=="Somewhat Agree", job_char_social := 0]
db[Q4_1_Job_Characteristics_5=="Neither Agree nor Disagree", job_char_social := 0]
db[Q4_1_Job_Characteristics_5=="Somewhat Disagree", job_char_social := 0]
db[Q4_1_Job_Characteristics_5=="Completely Disagree", job_char_social := 0]
###Which products are you proud of?
#
db[Q4_2_Products_0_Bakeryitems=="Not Selected", prod_proud_bakery := 0]
db[Q4_2_Products_0_Bakeryitems=="Selected", prod_proud_bakery := 1]
#
db[Q4_2_Products_0_BreakfastSandwiches=="Not Selected", prod_proud_bfsandwich := 0]
db[Q4_2_Products_0_BreakfastSandwiches=="Selected", prod_proud_bfsandwich := 1]
#
db[Q4_2_Products_0_BrewedCoffee=="Not Selected", prod_proud_brewed := 0]
db[Q4_2_Products_0_BrewedCoffee=="Selected", prod_proud_brewed := 1]
#
db[Q4_2_Products_0_Espressobasedbeverages=="Not Selected", prod_proud_espresso := 0]
db[Q4_2_Products_0_Espressobasedbeverages=="Selected", prod_proud_espresso := 1]
#
db[Q4_2_Products_0_Frappuccino=="Not Selected", prod_proud_frapp := 0]
db[Q4_2_Products_0_Frappuccino=="Selected", prod_proud_frapp := 1]
#
db[Q4_2_Products_0_Lunchitems=="Not Selected", prod_proud_lunch := 0]
db[Q4_2_Products_0_Lunchitems=="Selected", prod_proud_lunch := 1]
#
db[Q4_2_Products_0_Noneofthese=="Not Selected", prod_proud_none := 0]
db[Q4_2_Products_0_Noneofthese=="Selected", prod_proud_none := 1]
#
db[Q4_2_Products_0_PackagedFood=="Not Selected", prod_proud_packaged := 0]
db[Q4_2_Products_0_PackagedFood=="Selected", prod_proud_packaged := 1]
#
db[Q4_2_Products_0_Refreshers=="Not Selected", prod_proud_refresh := 0]
db[Q4_2_Products_0_Refreshers=="Selected", prod_proud_refresh := 1]
#
db[Q4_2_Products_0_ReserveCoffee=="Not Selected", prod_proud_reserve := 0]
db[Q4_2_Products_0_ReserveCoffee=="Selected", prod_proud_reserve := 1]
#
db[Q4_2_Products_0_ShakenIcedTea=="Not Selected", prod_proud_icetea := 0]
db[Q4_2_Products_0_ShakenIcedTea=="Selected", prod_proud_icetea := 1]
###Which products do you enjoy making?
#
db[Q4_2_Products_1_Bakeryitems=="Not Selected", prod_enjoy_bakery := 0]
db[Q4_2_Products_1_Bakeryitems=="Selected", prod_enjoy_bakery := 1]
#
db[Q4_2_Products_1_BreakfastSandwiches=="Not Selected", prod_enjoy_bfsandwich := 0]
db[Q4_2_Products_1_BreakfastSandwiches=="Selected", prod_enjoy_bfsandwich := 1]
#
db[Q4_2_Products_1_BrewedCoffee=="Not Selected", prod_enjoy_brewed := 0]
db[Q4_2_Products_1_BrewedCoffee=="Selected", prod_enjoy_brewed := 1]
#
db[Q4_2_Products_1_Espressobasedbeverages=="Not Selected", prod_enjoy_espresso := 0]
db[Q4_2_Products_1_Espressobasedbeverages=="Selected", prod_enjoy_espresso := 1]
#
db[Q4_2_Products_1_Frappuccino=="Not Selected", prod_enjoy_frapp := 0]
db[Q4_2_Products_1_Frappuccino=="Selected", prod_enjoy_frapp := 1]
#
db[Q4_2_Products_1_Lunchitems=="Not Selected", prod_enjoy_lunch := 0]
db[Q4_2_Products_1_Lunchitems=="Selected", prod_enjoy_lunch := 1]
#
db[Q4_2_Products_1_Noneofthese=="Not Selected", prod_enjoy_none := 0]
db[Q4_2_Products_1_Noneofthese=="Selected", prod_enjoy_none := 1]
#
db[Q4_2_Products_1_PackagedFood=="Not Selected", prod_enjoy_packaged := 0]
db[Q4_2_Products_1_PackagedFood=="Selected", prod_enjoy_packaged := 1]
#
db[Q4_2_Products_1_Refreshers=="Not Selected", prod_enjoy_refresh := 0]
db[Q4_2_Products_1_Refreshers=="Selected", prod_enjoy_refresh := 1]
#
db[Q4_2_Products_1_ReserveCoffee=="Not Selected", prod_enjoy_reserve := 0]
db[Q4_2_Products_1_ReserveCoffee=="Selected", prod_enjoy_reserve := 1]
#
db[Q4_2_Products_1_ShakenIcedTea=="Not Selected", prod_enjoy_icetea := 0]
db[Q4_2_Products_1_ShakenIcedTea=="Selected", prod_enjoy_icetea := 1]
###Which products do you not enjoy making?
#
db[Q4_2_Products_2_Bakeryitems=="Not Selected", prod_notenjoy_bakery := 0]
db[Q4_2_Products_2_Bakeryitems=="Selected", prod_notenjoy_bakery := 1]
#
db[Q4_2_Products_2_BreakfastSandwiches=="Not Selected", prod_notenjoy_bfsandwich := 0]
db[Q4_2_Products_2_BreakfastSandwiches=="Selected", prod_notenjoy_bfsandwich := 1]
#
db[Q4_2_Products_2_BrewedCoffee=="Not Selected", prod_notenjoy_brewed := 0]
db[Q4_2_Products_2_BrewedCoffee=="Selected", prod_notenjoy_brewed := 1]
#
db[Q4_2_Products_2_Espressobasedbeverages=="Not Selected", prod_notenjoy_espresso := 0]
db[Q4_2_Products_2_Espressobasedbeverages=="Selected", prod_notenjoy_espresso := 1]
#
db[Q4_2_Products_2_Frappuccino=="Not Selected", prod_notenjoy_frapp := 0]
db[Q4_2_Products_2_Frappuccino=="Selected", prod_notenjoy_frapp := 1]
#
db[Q4_2_Products_2_Lunchitems=="Not Selected", prod_notenjoy_lunch := 0]
db[Q4_2_Products_2_Lunchitems=="Selected", prod_notenjoy_lunch := 1]
#
db[Q4_2_Products_2_Noneofthese=="Not Selected", prod_notenjoy_none := 0]
db[Q4_2_Products_2_Noneofthese=="Selected", prod_notenjoy_none := 1]
#
db[Q4_2_Products_2_PackagedFood=="Not Selected", prod_notenjoy_packaged := 0]
db[Q4_2_Products_2_PackagedFood=="Selected", prod_notenjoy_packaged := 1]
#
db[Q4_2_Products_2_Refreshers=="Not Selected", prod_notenjoy_refresh := 0]
db[Q4_2_Products_2_Refreshers=="Selected", prod_notenjoy_refresh := 1]
#
db[Q4_2_Products_2_ReserveCoffee=="Not Selected", prod_notenjoy_reserve := 0]
db[Q4_2_Products_2_ReserveCoffee=="Selected", prod_notenjoy_reserve := 1]
#
db[Q4_2_Products_2_ShakenIcedTea=="Not Selected", prod_notenjoy_icetea := 0]
db[Q4_2_Products_2_ShakenIcedTea=="Selected", prod_notenjoy_icetea := 1]
##priorities: what are your top 3 in life right now
#
db[Q5_11_Priorities_Beinghealthyphysicallyfit=="Not Selected", prio_fit := 0]
db[Q5_11_Priorities_Beinghealthyphysicallyfit=="Selected", prio_fit := 1]
#
db[Q5_11_Priorities_Buyingacar=="Not Selected", prio_car := 0]
db[Q5_11_Priorities_Buyingacar=="Selected", prio_car := 1]
#
db[Q5_11_Priorities_Communityinvolvementvolunteering=="Not Selected", prio_vol := 0]
db[Q5_11_Priorities_Communityinvolvementvolunteering=="Selected", prio_vol := 1]
#
db[Q5_11_Priorities_Environmentalsocialissues=="Not Selected", prio_env := 0]
db[Q5_11_Priorities_Environmentalsocialissues=="Selected", prio_env := 1]
#
db[Q5_11_Priorities_Expandingmyfamily=="Not Selected", prio_expfam := 0]
db[Q5_11_Priorities_Expandingmyfamily=="Selected", prio_expfam := 1]
#
db[Q5_11_Priorities_Financialsavingsgoals=="Not Selected", prio_fisav := 0]
db[Q5_11_Priorities_Financialsavingsgoals=="Selected", prio_fisav := 1]
#
db[Q5_11_Priorities_Financialstability=="Not Selected", prio_fistab := 0]
db[Q5_11_Priorities_Financialstability=="Selected", prio_fistab := 1]
#
db[Q5_11_Priorities_Findinglovegettingmarried=="Not Selected", prio_love := 0]
db[Q5_11_Priorities_Findinglovegettingmarried=="Selected", prio_love := 1]
#
db[Q5_11_Priorities_Focusingonmyjobcareer=="Not Selected", prio_career := 0]
db[Q5_11_Priorities_Focusingonmyjobcareer=="Selected", prio_career := 1]
#
db[Q5_11_Priorities_Focusingonpersonalgrowthgoals=="Not Selected", prio_growth := 0]
db[Q5_11_Priorities_Focusingonpersonalgrowthgoals=="Selected", prio_growth := 1]
#
db[Q5_11_Priorities_Focusingonspiritualityfaith=="Not Selected", prio_faith := 0]
db[Q5_11_Priorities_Focusingonspiritualityfaith=="Selected", prio_faith := 1]
#
db[Q5_11_Priorities_Havingfun=="Not Selected", prio_fun := 0]
db[Q5_11_Priorities_Havingfun=="Selected", prio_fun := 1]
#
db[Q5_11_Priorities_Improvingchangingmyhousingsituation=="Not Selected", prio_house := 0]
db[Q5_11_Priorities_Improvingchangingmyhousingsituation=="Selected", prio_house := 1]
#
db[Q5_11_Priorities_Payingdowndebt=="Not Selected", prio_debt := 0]
db[Q5_11_Priorities_Payingdowndebt=="Selected", prio_debt := 1]
#
db[Q5_11_Priorities_Pets=="Not Selected", prio_pets := 0]
db[Q5_11_Priorities_Pets=="Selected", prio_pets := 1]
#
db[Q5_11_Priorities_Pursuingeducation=="Not Selected", prio_educ := 0]
db[Q5_11_Priorities_Pursuingeducation=="Selected", prio_educ := 1]
#
db[Q5_11_Priorities_Pursuinghobbies=="Not Selected", prio_hobbies := 0]
db[Q5_11_Priorities_Pursuinghobbies=="Selected", prio_hobbies := 1]
#
db[Q5_11_Priorities_Relationshipwithmysignificantother=="Not Selected", prio_sigoth := 0]
db[Q5_11_Priorities_Relationshipwithmysignificantother=="Selected", prio_sigoth := 1]
#
db[Q5_11_Priorities_Spendingtimewithfamily=="Not Selected", prio_family := 0]
db[Q5_11_Priorities_Spendingtimewithfamily=="Selected", prio_family := 1]
#
db[Q5_11_Priorities_Spendingtimewithfriends=="Not Selected", prio_friends := 0]
db[Q5_11_Priorities_Spendingtimewithfriends=="Selected", prio_friends := 1]
#
db[Q5_11_Priorities_Takingcareoffamilylovedones=="Not Selected", prio_carefam := 0]
db[Q5_11_Priorities_Takingcareoffamilylovedones=="Selected", prio_carefam := 1]
#
db[Q5_11_Priorities_Traveling=="Not Selected", prio_travel := 0]
db[Q5_11_Priorities_Traveling=="Selected", prio_travel := 1]
#
db[Q5_11_Priorities_Other=="Not Selected", prio_other := 0]
db[Q5_11_Priorities_Other=="Selected", prio_other := 1]
##starbucks super power
#
db[Q6_1_Superpowers_Craft=="Not Selected", super_craft := 0]
db[Q6_1_Superpowers_Craft=="Selected", super_craft := 1]
#
db[Q6_1_Superpowers_Creativity=="Not Selected", super_create := 0]
db[Q6_1_Superpowers_Creativity=="Selected", super_create := 1]
#
db[Q6_1_Superpowers_Dependability=="Not Selected", super_depend := 0]
db[Q6_1_Superpowers_Dependability=="Selected", super_depend := 1]
#
db[Q6_1_Superpowers_Empathy=="Not Selected", super_emp := 0]
db[Q6_1_Superpowers_Empathy=="Selected", super_emp := 1]
#
db[Q6_1_Superpowers_Enthusiasm=="Not Selected", super_enth := 0]
db[Q6_1_Superpowers_Enthusiasm=="Selected", super_enth := 1]
#
db[Q6_1_Superpowers_Humor=="Not Selected", super_humor := 0]
db[Q6_1_Superpowers_Humor=="Selected", super_humor := 1]
#
db[Q6_1_Superpowers_Initiative=="Not Selected", super_init := 0]
db[Q6_1_Superpowers_Initiative=="Selected", super_init := 1]
#
db[Q6_1_Superpowers_Memory=="Not Selected", super_mem := 0]
db[Q6_1_Superpowers_Memory=="Selected", super_mem := 1]
#
db[Q6_1_Superpowers_Optimism=="Not Selected", super_optim := 0]
db[Q6_1_Superpowers_Optimism=="Selected", super_optim := 1]
#
db[Q6_1_Superpowers_Organization=="Not Selected", super_org := 0]
db[Q6_1_Superpowers_Organization=="Selected", super_org := 1]
#
db[Q6_1_Superpowers_Patience=="Not Selected", super_pat := 0]
db[Q6_1_Superpowers_Patience=="Selected", super_pat := 1]
#
db[Q6_1_Superpowers_Presence=="Not Selected", super_pres := 0]
db[Q6_1_Superpowers_Presence=="Selected", super_pres := 1]
#
db[Q6_1_Superpowers_ProblemSolving=="Not Selected", super_prob := 0]
db[Q6_1_Superpowers_ProblemSolving=="Selected", super_prob := 1]
#
db[Q6_1_Superpowers_Warmth=="Not Selected", super_warmth := 0]
db[Q6_1_Superpowers_Warmth=="Selected", super_warmth := 1]
#
db[Q6_1_Superpowers_IdontknowmyStarbucksSuperpowers=="Not Selected", super_idk := 0]
db[Q6_1_Superpowers_IdontknowmyStarbucksSuperpowers=="Selected", super_idk := 1]
##how much do you agree with following statments
#I worry about having enough money
db[Q6_4_0=="Completely Agree", agree_worry_money := 1]
db[Q6_4_0=="Somewhat Agree", agree_worry_money := 0]
db[Q6_4_0=="Neither Agree nor Disagree", agree_worry_money := 0]
db[Q6_4_0=="Somewhat Disagree", agree_worry_money := 0]
db[Q6_4_0=="Completely Disagree", agree_worry_money := 0]
#I am optimistic about future
db[Q6_4_1=="Completely Agree", agree_optimistic := 1]
db[Q6_4_1=="Somewhat Agree", agree_optimistic := 0]
db[Q6_4_1=="Neither Agree nor Disagree", agree_optimistic := 0]
db[Q6_4_1=="Somewhat Disagree", agree_optimistic := 0]
db[Q6_4_1=="Completely Disagree", agree_optimistic := 0]
#I believe I'll be better off than my parents
db[Q6_4_2=="Completely Agree", agree_better_parents := 1]
db[Q6_4_2=="Somewhat Agree", agree_better_parents := 0]
db[Q6_4_2=="Neither Agree nor Disagree", agree_better_parents := 0]
db[Q6_4_2=="Somewhat Disagree", agree_better_parents := 0]
db[Q6_4_2=="Completely Disagree", agree_better_parents := 0]
#I will have a better future because of sbux
db[Q6_4_3=="Completely Agree", agree_better_bc_sbux := 1]
db[Q6_4_3=="Somewhat Agree", agree_better_bc_sbux := 0]
db[Q6_4_3=="Neither Agree nor Disagree", agree_better_bc_sbux := 0]
db[Q6_4_3=="Somewhat Disagree", agree_better_bc_sbux := 0]
db[Q6_4_3=="Completely Disagree", agree_better_bc_sbux := 0]
#My talents can be developed
db[Q6_4_4=="Completely Agree", agree_dev_talents := 1]
db[Q6_4_4=="Somewhat Agree", agree_dev_talents := 0]
db[Q6_4_4=="Neither Agree nor Disagree", agree_dev_talents := 0]
db[Q6_4_4=="Somewhat Disagree", agree_dev_talents := 0]
db[Q6_4_4=="Completely Disagree", agree_dev_talents := 0]
#I am an original, creative thinker
db[Q6_4_5=="Completely Agree", agree_thinker := 1]
db[Q6_4_5=="Somewhat Agree", agree_thinker := 0]
db[Q6_4_5=="Neither Agree nor Disagree", agree_thinker := 0]
db[Q6_4_5=="Somewhat Disagree", agree_thinker := 0]
db[Q6_4_5=="Completely Disagree", agree_thinker := 0]
###Did you worry about
#
db[Q6_5_Money_Ididntworryaboutanyofthese=="Not Selected", money_none := 0]
db[Q6_5_Money_Ididntworryaboutanyofthese=="Selected", money_none := 1]
#
db[Q6_5_Money_Lackofsavings=="Not Selected", money_lack_savings := 0]
db[Q6_5_Money_Lackofsavings=="Selected", money_lack_savings := 1]
#
db[Q6_5_Money_Notbeingabletohaveenoughmoneytoretire=="Not Selected", money_retire := 0]
db[Q6_5_Money_Notbeingabletohaveenoughmoneytoretire=="Selected", money_retire := 1]
#
db[Q6_5_Money_Nothavingenoughmoneytocoveryourexpenses=="Not Selected", money_expenses := 0]
db[Q6_5_Money_Nothavingenoughmoneytocoveryourexpenses=="Selected", money_expenses := 1]
#
db[Q6_5_Money_Payingotherbillssuchasutilitiesmedicalorlegal=="Not Selected", money_othbills := 0]
db[Q6_5_Money_Payingotherbillssuchasutilitiesmedicalorlegal=="Selected", money_othbills := 1]
#
db[Q6_5_Money_Payingyourcreditcardbill=="Not Selected", money_lack_credcard := 0]
db[Q6_5_Money_Payingyourcreditcardbill=="Selected", money_lack_credcard := 1]
#
db[Q6_5_Money_Payingyourrentormortgage=="Not Selected", money_rent := 0]
db[Q6_5_Money_Payingyourrentormortgage=="Selected", money_rent := 1]
#
db[Q6_5_Money_Payingyourstudentloans=="Not Selected", money_studloans := 0]
db[Q6_5_Money_Payingyourstudentloans=="Selected", money_studloans := 1]
#
db[Q6_5_Money_Unexpectedexpensesegcarrepairspetcare=="Not Selected", money_unexpected := 0]
db[Q6_5_Money_Unexpectedexpensesegcarrepairspetcare=="Selected", money_unexpected := 1]
##how much do you agree or disagree an unexpected event, make ends meet
db[Q6_6=="Completely Agree", hard_ends_meet := 1]
db[Q6_6=="Somewhat Agree", hard_ends_meet := 0]
db[Q6_6=="Neither Agree nor Disagree", hard_ends_meet := 0]
db[Q6_6=="Somewhat Disagree", hard_ends_meet := 0]
db[Q6_6=="Completely Disagree", hard_ends_meet := 0]
###demographics
##marital status: married
db[Q8_1_Marital_Status=="Living with a partner", marital_married := 0]
db[Q8_1_Marital_Status=="Married/remarried", marital_married := 1]
db[Q8_1_Marital_Status=="Single, never been married", marital_married := 0]
db[Q8_1_Marital_Status=="Prefer not to answer", marital_married := NA]
db[Q8_1_Marital_Status=="Separated", marital_married := 0]
db[Q8_1_Marital_Status=="Divorced or widowed", marital_married := 0]
##marital status: married or living with partner
db[Q8_1_Marital_Status=="Living with a partner", marital_married_part := 1]
db[Q8_1_Marital_Status=="Married/remarried", marital_married_part := 1]
db[Q8_1_Marital_Status=="Single, never been married", marital_married_part := 0]
db[Q8_1_Marital_Status=="Prefer not to answer", marital_married_part := NA]
db[Q8_1_Marital_Status=="Separated", marital_married_part := 0]
db[Q8_1_Marital_Status=="Divorced or widowed", marital_married_part := 0]
##marital status: single, never married
db[Q8_1_Marital_Status=="Living with a partner", marital_single_nevermar := 0]
db[Q8_1_Marital_Status=="Married/remarried", marital_single_nevermar := 0]
db[Q8_1_Marital_Status=="Single, never been married", marital_single_nevermar := 1]
db[Q8_1_Marital_Status=="Prefer not to answer", marital_single_nevermar := NA]
db[Q8_1_Marital_Status=="Separated", marital_single_nevermar := 0]
db[Q8_1_Marital_Status=="Divorced or widowed", marital_single_nevermar := 0]
###household members
#
db[Q8_2_Household_Members_Child=="Not Selected", house_child := 0]
db[Q8_2_Household_Members_Child=="Selected", house_child := 1]
#
db[Q8_2_Household_Members_ExtendedFamily=="Not Selected", house_extf := 0]
db[Q8_2_Household_Members_ExtendedFamily=="Selected", house_extf := 1]
#
db[Q8_2_Household_Members_NooneelseIlivealone=="Not Selected", house_alone := 0]
db[Q8_2_Household_Members_NooneelseIlivealone=="Selected", house_alone := 1]
#
db[Q8_2_Household_Members_Parents=="Not Selected", house_parents := 0]
db[Q8_2_Household_Members_Parents=="Selected", house_parents := 1]
#
db[Q8_2_Household_Members_RoommateFriend=="Not Selected", house_roommate := 0]
db[Q8_2_Household_Members_RoommateFriend=="Selected", house_roommate := 1]
#
db[Q8_2_Household_Members_SpouseSignificantother=="Not Selected", house_spouse := 0]
db[Q8_2_Household_Members_SpouseSignificantother=="Selected", house_spouse := 1]
#
db[Q8_2_Household_Members_Other=="Not Selected", house_other := 0]
db[Q8_2_Household_Members_Other=="Selected", house_other := 1]
###student status
db[Q8_6_Student_Flag=="Not Student", student := 0]
db[Q8_6_Student_Flag=="Student", student := 1]
###healthcare
##starbucks
#revalue, for labels
db[Q8_5_Health_Coverage=="Affordable Care Act (also referred to as Obamacare or the Health Insurance Marketplace)", Q8_5_Health_Coverage := "Affordable Care Act"]
#
db[Q8_5_Health_Coverage=="Through Starbucks", health_sbux := 1]
db[Q8_5_Health_Coverage=="Affordable Care Act", health_sbux := 0]
db[Q8_5_Health_Coverage=="Through my parents", health_sbux := 0]
db[Q8_5_Health_Coverage=="Through my spouse", health_sbux := 0]
db[Q8_5_Health_Coverage=="Other (e.g., school, military)", health_sbux := 0]
db[Q8_5_Health_Coverage=="I don't currently have health insurance", health_sbux := 0]
db[Q8_5_Health_Coverage=="Prefer not to answer", health_sbux := NA]
###highest education level
##high school
db[Q8_7_Education_Level=="Some college or Associate degree", educ_highschool := 1]
db[Q8_7_Education_Level=="Graduated college/Bachelor's degree", educ_highschool := 1]
db[Q8_7_Education_Level=="Some Graduate school", educ_highschool := 1]
db[Q8_7_Education_Level=="Advanced degree (Master's, Ph.D.)", educ_highschool := 1]
db[Q8_7_Education_Level=="High school graduate or GED", educ_highschool := 1]
db[Q8_7_Education_Level=="Some high school", educ_highschool := 0]
db[Q8_7_Education_Level=="Trade/technical school degree", educ_highschool := 1]
db[Q8_7_Education_Level=="Other", educ_highschool := NA]
db[Q8_7_Education_Level=="Prefer not to answer", educ_highschool := NA]
##bachelor's degree
db[Q8_7_Education_Level=="Some college or Associate degree", educ_bach := 0]
db[Q8_7_Education_Level=="Graduated college/Bachelor's degree", educ_bach := 1]
db[Q8_7_Education_Level=="Some Graduate school", educ_bach := 1]
db[Q8_7_Education_Level=="Advanced degree (Master's, Ph.D.)", educ_bach := 1]
db[Q8_7_Education_Level=="High school graduate or GED", educ_bach := 0]
db[Q8_7_Education_Level=="Some high school", educ_bach := 0]
db[Q8_7_Education_Level=="Trade/technical school degree", educ_bach := 0]
db[Q8_7_Education_Level=="Other", educ_bach := NA]
db[Q8_7_Education_Level=="Prefer not to answer", educ_bach := NA]
#question 8_7 without other (for plotting)
db[Q8_7_Education_Level!="Other", educ_level_without_other := Q8_7_Education_Level]
db[Q8_7_Education_Level=="Other", educ_level_without_other := NA]
db[Q8_7_Education_Level=="Prefer not to answer", educ_level_without_other := NA]

##outliers
#drop values where Sbux hours worked is 0 or >60 (same as Q1_7_Starbucks_Hours_Worked_Remove_Outliers)
db[Q1_7_Starbucks_Hours_Worked>0 & Q1_7_Starbucks_Hours_Worked<61, sbux_hours_worked := Q1_7_Starbucks_Hours_Worked]

##start using the SAP question. only for hoursly; all managers *should* be 40 
##(not what is reported; most managers report >40 hours)
db[role==3, SAP_Avg_Hrs_per_Wk_8_weeks := 40]

#summary table of hours (reported)
temp <- db[, list(
  hrs_min = round(min(sbux_hours_worked,na.rm=T),1),
  hrs_max = round(max(sbux_hours_worked,na.rm=T),1),
  hrs_rng = round(max(sbux_hours_worked,na.rm=T) - min(sbux_hours_worked,na.rm=T),1),
  hrs_avg = round(mean(sbux_hours_worked,na.rm=T),1),
  hrs_med = round(median(sbux_hours_worked,na.rm=T),1),
  pct_20p = round(length(which(sbux_hours_worked>=20))/.N,4),
  pct_32p = round(length(which(sbux_hours_worked>=32))/.N,4),
  pct_40p = round(length(which(sbux_hours_worked>=40))/.N,4)
),by="Store_Role"]
#summary table of hours (SAP)
temp <- db[, list(
  hrs_min = round(min(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
  hrs_max = round(max(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
  hrs_rng = round(max(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T) - min(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
  hrs_avg = round(mean(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
  hrs_med = round(median(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
  pct_20p = round(length(which(SAP_Avg_Hrs_per_Wk_8_weeks>=20))/.N,4),
  pct_32p = round(length(which(SAP_Avg_Hrs_per_Wk_8_weeks>=32))/.N,4),
  pct_40p = round(length(which(SAP_Avg_Hrs_per_Wk_8_weeks>=40))/.N,4)
),by="Store_Role"]

##cut by full time and not full time
db[(role==1|role==2) & sbux_hours_worked<32, full_time := 0]
db[(role==1|role==2) & sbux_hours_worked>=32, full_time := 1]
db[role==3, full_time := 1]

#create new variable for plots
db[full_time==0, role_ft := paste0(role,"-","PT")]
db[full_time==1, role_ft := paste0(role,"-","FT")]

#create new reason join - career variable for plots
db[reason_join_career==0, role_rjc := paste0(role,"-","No")]
db[reason_join_career==1, role_rjc := paste0(role,"-","Yes")]

#create new reason join - fun variable for plots
db[reason_join_fun==0, role_rjf := paste0(role,"-","No")]
db[reason_join_fun==1, role_rjf := paste0(role,"-","Yes")]



###ANALYSIS
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
dt <- db[,varstokeep,with=FALSE]

###create table 1
#create Table 1; library(tableone)
#Create a variable list which we want in Table 1 (varstokeep)
#Define categorical variables (catvars)
#Stratified by Store Role
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
  writeDoc(file = (paste0(plot_dir, "frequency_table.docx")))




###CORRELATION MATRICES
#subset to variables for correlation matrices 
binaryvars <- c("more_than_one_job",
                "sched_very_consistent",
                "agree_true_self","agree_reco_great","agree_great_team",
                "agree_problem_solve",
                "agree_worry_money",
                "money_lack_savings",
                "money_expenses",
                "hard_ends_meet","marital_married_part",
                "Q8_4_Primary_Income_Flag","health_sbux","student","Kids_Flag")
contvars <- c("Q1_7_Starbucks_Hours_Worked_Remove_Outliers","age")
#catvars <- c("Store_Role")
varstokeep <- c(binaryvars,contvars)
#subset data
dtcorr <- db[,varstokeep,with=FALSE]
# dtcorr <- db[Store_Role=="Barista",varstokeep,with=FALSE]
# dtcorr <- db[Store_Role=="Store manager",varstokeep,with=FALSE]
# dtcorr <- db[Store_Role=="Shift supervisor",varstokeep,with=FALSE]

###correlation matrices
#flatten the table
flattenSquareMatrix(cor.prob(dtcorr))
#plot the data
chart.Correlation(dtcorr)




##CLUSTERING
##subset by store role
bars <- db[Store_Role=="Barista"]
ssh <- db[Store_Role=="Shift supervisor"]
magnr <- db[Store_Role=="Store manager"]

##CLUSTERING
binaryvars <- c("reason_join_health",
                "agree_worry_money",
                "marital_married",
                "prio_educ",
                "prio_fistab",
                "educ_highschool", 
                "agree_optimistic",
                "job_months",
                "student",
                "house_parents")
contvars <- c("Q1_8_Starbucks_Ideal_Hours_Remove_Outliers",
              "age")

##baristas
###
#split by variable type
kmeandt <- bars[,c(contvars,binaryvars),with=FALSE]

##METHOD 1
# # Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2      # Hypothesized minimum number of segments
maxClust <- 8      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(na.omit(kmeandt), centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(na.omit(kmeandt))))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}
# 
# # Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) +
  geom_point(size=3) +
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")


##METHOD 2
xquant <- bars[,c(contvars,binaryvars),with=FALSE]
#xqual <- bars[,c("tenure"),with=FALSE]
#dbc <- cbind(xquant,xqual)
#d <- daisy(dbc, metric="gower")
d <- daisy(xquant, metric="gower")
fit <- hclust(d=d, method="complete") 
plot(fit, hang=-1)
Nclust <- 3
groups <- cutree(fit, k=Nclust)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=Nclust, border="red") # draw dendogram with red borders around the 4 clusters 
#produce a k-means plot
kfit <- kmeans(d, Nclust)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#You can add the cluster information into the data set. To add a variable titled "Cluster":
bars[,"cluster"] <- kfit$cluster

#create new labeled cluster variable for plotting
#
bars[cluster==1, clustername := "C01"]
bars[cluster==2, clustername := "C02"]
bars[cluster==3, clustername := "C03"]


##Shift managers
#split by variable type
kmeandt <- ssh[,c(contvars,binaryvars),with=FALSE]
##METHOD 1
# # Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2      # Hypothesized minimum number of segments
maxClust <- 8      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(na.omit(kmeandt), centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(na.omit(kmeandt))))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}
# 
# # Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) +
  geom_point(size=3) +
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")


##METHOD 2
xquant <- ssh[,c(contvars,binaryvars),with=FALSE]
#xqual <- ssh[,c("tenure"),with=FALSE]
#dbc <- cbind(xquant,xqual)
#d <- daisy(dbc, metric="gower")
d <- daisy(xquant, metric="gower")
fit <- hclust(d=d, method="complete") 
plot(fit, hang=-1)
Nclust <- 3
groups <- cutree(fit, k=Nclust)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=Nclust, border="red") # draw dendogram with red borders around the clusters 
#produce a k-means plot
kfit <- kmeans(d, Nclust)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#You can add the cluster information into the data set. To add a variable titled "Cluster":
ssh[,"cluster"] <- kfit$cluster

#create new labeled cluster variable for plotting
#
ssh[cluster==1, clustername := "C04"]
ssh[cluster==2, clustername := "C05"]
ssh[cluster==3, clustername := "C06"]

##STORE MANAGERS
##CLUSTERING
binaryvars2 <- c("agree_dev_talents",
                "agree_worry_money",
                "marital_married",
                "Kids_Flag",
                "prio_growth",
                "prio_career",
                "prio_fistab",
                "reason_join_health",
                "agree_better_bc_sbux",
                "more_than_one_job",
                "agree_optimistic")
contvars2 <- c("job_months","age")

#split by variable type
kmeandt <- magnr[,c(contvars2,binaryvars2),with=FALSE]

##METHOD 1
# # Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2      # Hypothesized minimum number of segments
maxClust <- 8      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(na.omit(kmeandt), centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(na.omit(kmeandt))))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}
# 
# # Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) +
  geom_point(size=3) +
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")
# 
# ##METHOD 2
xquant <- magnr[,c(contvars2,binaryvars2),with=FALSE]
#xqual <- magnr[,c("tenure"),with=FALSE]
#dbc <- cbind(xquant,xqual)
#d <- daisy(dbc, metric="gower")
d <- daisy(xquant, metric="gower")
fit <- hclust(d=d, method="complete") 
plot(fit, hang=-1)
Nclust <- 3
groups <- cutree(fit, k=Nclust)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=Nclust, border="red") # draw dendogram with red borders around the 4 clusters 
#produce a k-means plot
kfit <- kmeans(d, Nclust)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#You can add the cluster information into the data set. To add a variable titled "Cluster":
magnr[,"cluster"] <- kfit$cluster

#
magnr[cluster==1, clustername := "C07"]
magnr[cluster==2, clustername := "C08"]
magnr[cluster==3, clustername := "C09"]
# magnr[cluster==4, clustername := "C10"]



#rbind store roles
l = list(bars,ssh,magnr)
clusdt <- rbindlist(l, use.names=TRUE, fill=TRUE)
#reduce to only clustername variable and PartnerID for merge to original db
temp <- clusdt[,c("PartnerID","clustername"),with=FALSE]
clusdt <- Reduce(function(x, y) {merge(x, y, by="PartnerID", all = TRUE)}, list(db, temp))




###keep only newly created variables for correlations
temp <- db %>%
  select_if(is.numeric) %>%
  select(-starts_with("Q"),-starts_with("prod_"))
rc <- rcorr(as.matrix(temp),type="pearson")
#round to 3 decimal points
rmat <- as.data.table(rc$r)
rmat <- rmat %>% mutate_all(funs(round(.,3)))
#round to 3 decimal points
pmat <- as.data.table(rc$P)
pmat <- pmat %>% mutate_all(funs(round(.,3)))
#for easier reading, anything not-sig turns to missing
pmat[pmat>0.05] <- NA
#write to .csv's
write.xlsx(rmat,file=paste0(plot_dir,"rc_r.xlsx"))
write.xlsx(pmat,file=paste0(plot_dir,"rc_p.xlsx"))













# ##set directories
# plot_dir <- ("C:/Users/jumorris/Desktop/temp_plots/")
# 
# #Question 1_1: NUMBER_OF_JOBS
# #subset to vars of interest
# vars <- c("Store_Role","more_than_one_job")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 1_1: How many jobs do you currently have, including Starbucks? - More than 1 job") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% More than 1 job") 
# #create PDF file
# filename <- (paste0(plot_dir, "que1_1.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question 1_3: PRIMARY_JOB
# #subset to vars of interest
# vars <- c("Store_Role","sbux_primary")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 1_3: Which do you consider your primary job? - Starbucks") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Starbucks primary job") 
# #create PDF file
# filename <- (paste0(plot_dir, "que1_3.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question 1_3: PRIMARY_JOB REASON - non-starbucks jobs
# #subset to vars of interest
# db2 <- db[sbux_primary==0]
# vars <- c("Store_Role",
#           "prijob_career","prijob_income","prijob_health","prijob_retire",
#           "prijob_hours","prijob_company","prijob_advance")
# db2 <- db2[,vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Working towards my long-term career",
#                        "My main source of income",
#                        "Provides me with healthcare benefits",
#                        "Provides me with retirement benefits",
#                        "Provides consistent hours",
#                        "It's at a reputable company", 
#                        "Offers career advancement opportunities")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que1_3nonsbux.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 1_3 \n Why do you consider another job your as your primary job?"))
# graphics.off()
# 
# 
# 
# ###Question 1_3: PRIMARY_JOB REASON - starbucks
# #subset to vars of interest
# db2 <- db[sbux_primary==1]
# vars <- c("Store_Role",
#           "prijob_career","prijob_income","prijob_health","prijob_retire",
#           "prijob_hours","prijob_company","prijob_advance")
# db2 <- db2[,vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Working towards my long-term career",
#                        "My main source of income",
#                        "Provides me with healthcare benefits",
#                        "Provides me with retirement benefits",
#                        "Provides consistent hours",
#                        "It's at a reputable company", 
#                        "Offers career advancement opportunities")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que1_3sbux.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 1_3 \n Why do you consider Starbucks as your primary job?"))
# graphics.off()
# 
# 
# 
# #Question 1_5: IF_MORE_HOURS
# #subset to vars of interest
# vars <- c("Store_Role","more_hours_keep_oth_job")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 1_5: If you were able to get more hours at Starbucks, would you still work your other job(s)?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Keep other job(s)") 
# #create PDF file
# filename <- (paste0(plot_dir, "que1_5.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question 1_7: HOURS_WORKED
# plot1 <- ggplot() + 
#   geom_histogram(data=db, aes(Q1_7_Starbucks_Hours_Worked_Remove_Outliers), color="black", fill="white", binwidth = 2) + 
#   theme_bw() + ggtitle("Question 1_7: Hours worked at Starbucks (outliers removed)") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_x_continuous(name = "Hours", breaks = seq(0, 60, 10), limits=c(0, 61))
# #create PDF file
# filename <- (paste0(plot_dir, "que1_7.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question 1_8: IDEAL_WORKED
# plot1 <- ggplot() + 
#   geom_histogram(data=db, aes(Q1_8_Starbucks_Ideal_Hours_Remove_Outliers), color="black", fill="white", binwidth = 2) + 
#   theme_bw() + ggtitle("Question 1_8: Ideal hours at Starbucks (outliers removed)") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_x_continuous(name = "Hours", breaks = seq(0, 40, 10), limits=c(0, 41))
# #create PDF file
# filename <- (paste0(plot_dir, "que1_8.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question: HOURS_WORKED - difference from ideal
# plot1 <- ggplot() + 
#   geom_histogram(data=db, aes(Hours_Delta_Ideal_From_Stated), color="black", fill="white", binwidth = 2) + 
#   theme_bw() + ggtitle("Question 1_7 & 1_8: Difference between hours worked and ideal hours at Starbucks") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_x_continuous(name = "Hours", breaks = seq(-30, 40, 10), limits=c(-31, 41))
# #create PDF file
# filename <- (paste0(plot_dir, "que1_7-8.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# 
# ###Question 1_9: PREVENT_HOURS
# #subset to vars of interest
# vars <- c("Store_Role",
#           "prev_hours_few_mgr","prev_hours_many_mgr","prev_hours_not_avail","prev_hours_need_money",
#           "prev_hours_sched")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("My manager doesn't schedule enough hours for me",
#                        "My manager schedules too many hours for me",
#                        "I'm not available to work more hours",
#                        "I work more hours than I want because I need the money",
#                        "My schedule doesn't work with the available shifts")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Completely Agree") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que1_9.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 1_9 \n In your opinion, what prevents you from getting the \n number of hours you want in a week at Starbucks?"))
# graphics.off()
# 
# 
# 
# #Question 2_1: schedule consistency
# #subset to vars of interest
# vars <- c("Store_Role","sched_very_consistent")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 2_1: How would you rate the consistency of your schedule at Starbucks week to week?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Very consistent") 
# #create PDF file
# filename <- (paste0(plot_dir, "que2_1.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question 2_2: schedule consistency importance
# #subset to vars of interest
# vars <- c("Store_Role","sched_consis_imp")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 2_2: How important is schedule consistency to you?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Very important") 
# #create PDF file
# filename <- (paste0(plot_dir, "que2_2.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question 2_4: schedule consistency description
# #plot
# plot1 <- ggplot(data=db[!is.na(sched_consis_without_other)],aes(x=Store_Role,fill=sched_consis_without_other)) +
#   theme_bw() +
#   geom_bar(position = position_fill(reverse = TRUE)) + 
#   ggtitle("Question 2_4: Schedule Consistency Decription") + ylab("Proportion") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8),
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_fill_discrete(name= "Which of the following is most important to you \nin a consistent schedule?", guide = guide_legend(reverse=TRUE))
# #create PDF file
# filename <- (paste0(plot_dir, "que2_4.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question 3_1: Reason_JOIN
# #subset to vars of interest
# vars <- c("Store_Role",
#           "reason_join_brand","reason_join_cap","reason_join_career","reason_join_cust",
#           "reason_join_flex_sched","reason_join_fun","reason_join_health",
#           "reason_join_oth_benef","reason_join_pay","reason_join_prodbev","reason_join_values")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Brand Reputation","College Achievement Plan","Career Advancement Opportunities",
#                        "Opportunity to connect with customers","Flexbility of schedule",
#                        "Seemed like a fun place to work","Health coverage","Other financial benefits",
#                        "Pay","Products (beverages, food, etc.)","Mission and values")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que3_1.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 3_1: \n Which of the following factors most influenced \n your decision to work for Starbucks?"))
# graphics.off()
# 
# 
# 
# #Question 3_2: Store Choice
# #plot
# plot1 <- ggplot(data=db[!is.na(store_choice_without_other)],aes(x=Store_Role,fill=store_choice_without_other)) +
#   theme_bw() +
#   geom_bar(position = position_fill(reverse = TRUE)) + ylab("Proportion") +
#   ggtitle("Question 3_2: Store Choice") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8),
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_fill_discrete(name= "Primary reason chose this store", guide = guide_legend(reverse=TRUE))
# #create PDF file
# filename <- (paste0(plot_dir, "que3_2.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question 3_5
# #subset to vars of interest
# vars <- c("Store_Role",
#           "agree_true_self","agree_reco_great","agree_great_team","agree_decisions",
#           "agree_problem_solve")
# db2 <- db[, vars, with=FALSE]
# 
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Starbucks allows me to be my true self at work",
#                        "I recommend my store as a great place to work",
#                        "I am part of a great team",
#                        "I am able to make decisions about how my work gets done",
#                        "I am able to creatively solve problems at work")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Completely Agree") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que3_5.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 3_5 \n How much do you agree with the following statements?"))
# graphics.off()
# 
# 
# 
# ###Question 3_8: TEAM_DIVERSITY
# #subset to vars of interest
# vars <- c("Store_Role",
#           "team_like_me","team_perspectives","team_spend_time","team_work_well")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Are just like me",
#                        "Bring diverse perspectives",
#                        "Spend time together outside of work",
#                        "Work well together")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Completely Agree") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que3_8.pdf"))
# pdf(filename)
# do.call("grid.arrange", c(plotlist, top = "Question 3_8 \n How much do you agree or disagree that each of \n the following items describe the partners on your team?"))
# graphics.off()
# 
# 
# 
# ###Question 4_1: JOB_CHARACTERISTICS
# #subset to vars of interest
# vars <- c("Store_Role",
#           "job_char_diff","job_char_fun","job_char_stress","job_char_overwhelm",
#           "job_char_exciting","job_char_social")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Difficult","Fun","Stressful","Overwhelming","Exciting","Social")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Completely Agree") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que4_1.pdf"))
# pdf(filename)
# do.call("grid.arrange", c(plotlist, top = "Question 4_1 \n How much do you agree or disagree that each of \n the following items describe your job?"))
# graphics.off()
# 
# 
# 
# ###Question 4_2a: products proud of making
# #subset to vars of interest
# vars <- c("Store_Role",
#           "prod_proud_bakery","prod_proud_bfsandwich","prod_proud_brewed",
#           "prod_proud_espresso","prod_proud_frapp","prod_proud_lunch",
#           "prod_proud_none","prod_proud_packaged","prod_proud_refresh",
#           "prod_proud_reserve","prod_proud_icetea")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Bakery items","Breakfast sandwiches","Brewed coffee",
#                        "Espresso-based beverages","Frappuccino","Lunch items",
#                        "None of these","Packaged food","Refreshers",
#                        "Reserve coffee","Shaken iced tea")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que4_2a.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 4_2(a): \n Which products are you proud of?"))
# graphics.off()
# 
# 
# 
# ###Question 4_2b: products enjoy making
# #subset to vars of interest
# vars <- c("Store_Role",
#           "prod_enjoy_bakery","prod_enjoy_bfsandwich","prod_enjoy_brewed",
#           "prod_enjoy_espresso","prod_enjoy_frapp","prod_enjoy_lunch",
#           "prod_enjoy_none","prod_enjoy_packaged","prod_enjoy_refresh",
#           "prod_enjoy_reserve","prod_enjoy_icetea")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Bakery items","Breakfast sandwiches","Brewed coffee",
#                        "Espresso-based beverages","Frappuccino","Lunch items",
#                        "None of these","Packaged food","Refreshers",
#                        "Reserve coffee","Shaken iced tea")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que4_2b.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 4_2(b): \n Which do you enjoy making?"))
# graphics.off()
# 
# 
# 
# ###Question 4_2c: products do not enjoy making
# #subset to vars of interest
# vars <- c("Store_Role",
#           "prod_notenjoy_bakery","prod_notenjoy_bfsandwich","prod_notenjoy_brewed",
#           "prod_notenjoy_espresso","prod_notenjoy_frapp","prod_notenjoy_lunch",
#           "prod_notenjoy_none","prod_notenjoy_packaged","prod_notenjoy_refresh",
#           "prod_notenjoy_reserve","prod_notenjoy_icetea")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Bakery items","Breakfast sandwiches","Brewed coffee",
#                        "Espresso-based beverages","Frappuccino","Lunch items",
#                        "None of these","Packaged food","Refreshers",
#                        "Reserve coffee","Shaken iced tea")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que4_2c.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 4_2(c): \n Which products do you NOT enjoy making?"))
# graphics.off()
# 
# 
# 
# ###Question 6_4: AGREE_WITH
# #subset to vars of interest
# vars <- c("Store_Role",
#           "agree_worry_money","agree_optimistic","agree_better_parents","agree_better_bc_sbux",
#           "agree_dev_talents", "agree_thinker")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("I worry about having enough money",
#                        "I am optimistic about the future",
#                        "I believe I will be better off than my parents",
#                        "I believe I will have a better future because I work at Starbucks",
#                        "My talents can be developed",
#                        "I am an original, creative thinker")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Completely Agree") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que6_4.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 6_4: \n How much do you agree or disagree with each of the following statements?"))
# graphics.off()
# 
# 
# 
# ###Question 6_5: money worries
# #subset to vars of interest
# vars <- c("Store_Role",
#           "money_lack_savings","money_retire",
#           "money_expenses","money_othbills","money_lack_credcard",
#           "money_rent","money_studloans","money_unexpected","money_none")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Lack of savings",
#                        "Not being able to have enough money to retire",
#                        "Not having enough money to cover your expenses",
#                        "Paying other bills such as utilities, medical, or legal",
#                        "Paying your credit card bill",
#                        "Paying your rent or mortgage",
#                        "Paying your student loans",
#                        "Unexpected expenses","I didn't worry about any of these")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que6_5.pdf"))
# pdf(filename, width = 14, height = 17)
# do.call("grid.arrange", c(plotlist, top = "Question 6_5: \n In the past month, did you every worry about:"))
# graphics.off()
# 
# 
# 
# #Question 6_6: making ends meet
# #subset to vars of interest
# vars <- c("Store_Role","hard_ends_meet")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 6_6: How much do you agree or disagree that an unexpected event in the past \n2 months made it harder for you to make ends meet?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Completely Agree") 
# #create PDF file
# filename <- (paste0(plot_dir, "que6_6.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# 
# #Question 8_1: marital status
# #subset to vars of interest
# vars <- c("Store_Role","marital_married_part")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 8_1: What is your marital status?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Married/Remarried or Living with Partner") 
# #create PDF file
# filename <- (paste0(plot_dir, "que8_1.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question 8_2: HOUSEHOLD_MEMBERS
# #subset to vars of interest
# vars <- c("Store_Role",
#           "house_child","house_extf","house_alone","house_parents",
#           "house_roommate","house_spouse","house_other")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #mapvalues to question name
# vars_to_match <- unique(DT[,variable])
# new_vec_of_values <- c("Child", "Extended family", "No one else - I live alone",
#                        "Parent(s)", "Roommate/friend","Spouse/Significant other","Other")
# DT[, que_name :=  mapvalues(DT[, variable], from = vars_to_match, to = new_vec_of_values)]
# #plot
# plotlist <- list()
# for(i in vars[-1]) {
#   plotlist[[i]] <- ggplot(data=DT[variable==i], aes(x = Store_Role, y = value*100)) +
#     geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#     ggtitle(DT[variable==i,que_name]) + 
#     scale_y_continuous(limits=c(0,100)) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#           plot.title = element_text(size = 10, face = "bold")) + 
#     labs(x = NULL, y = "% Selected") 
# }
# #create PDF file
# filename <- (paste0(plot_dir, "que8_2.pdf"))
# pdf(filename, width = 10, height = 12)
# do.call("grid.arrange", c(plotlist, top = "Question 8_2 \n Besides you, who else lives in your household?"))
# graphics.off()
# 
# 
# 
# #Question 8_4: INCOME_CONTRIBUTION
# #subset to vars of interest
# vars <- c("Store_Role","Q8_4_Primary_Income_Flag")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 8_4: Monetarily, how much does your income from your current job(s) \ncontribute to your household expenses?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Providing primary income") 
# #create PDF file
# filename <- (paste0(plot_dir, "que8_4.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question 8_5: HEALTH_COVERAGE
# #subset to vars of interest
# vars <- c("Store_Role","health_sbux")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 8_5: Which of the following best describes your primary health coverage? - Through Starbucks") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Through Starbucks") 
# #create PDF file
# filename <- (paste0(plot_dir, "que8_5.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question 8_6: STUDENT_STATUS
# #subset to vars of interest
# vars <- c("Store_Role","student")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 8_6: What is your current status as a student?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Student") 
# #create PDF file
# filename <- (paste0(plot_dir, "que8_6.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question 8_7: EDUCATION_LEVEL
# #subset to vars of interest
# vars <- c("Store_Role","educ_highschool")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 8_7: What is the highest level of education you have completed? - High School Diploma +") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% High School Diploma +") 
# #create PDF file
# filename <- (paste0(plot_dir, "que8_7hs.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question 8_7: EDUCATION_LEVEL
# #subset to vars of interest
# vars <- c("Store_Role","educ_bach")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Question 8_7: What is the highest level of education you have completed? - Bachelor's Degree +") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% Bachelor's Degree +") 
# #create PDF file
# filename <- (paste0(plot_dir, "que8_7bach.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question: KIDS_FLAG
# #subset to vars of interest
# vars <- c("Store_Role","Kids_Flag")
# db2 <- db[, vars, with=FALSE]
# #lapply to create percentages
# db2<- db2[, lapply(.SD, mean, na.rm=TRUE), by="Store_Role"]
# #reshape from wide to long
# DT <- melt(db2, id="Store_Role")
# #plot
# plot1 <- ggplot(data=DT, aes(x = Store_Role, y = value*100)) +
#   geom_bar(stat="identity", width = 0.7, fill="forestgreen") + theme_bw() + 
#   ggtitle("Do you have kids?") + 
#   scale_y_continuous(limits=c(0,100)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = NULL, y = "% With kids") 
# #create PDF file
# filename <- (paste0(plot_dir, "que_kids.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question: AGE ROLLUP
# #plot
# plot1 <- ggplot(data=db[!is.na(Age_Rollup)],aes(x=Store_Role,fill=Age_Rollup)) + theme_bw() +
#   geom_bar(position = position_fill(reverse = TRUE)) + 
#   ggtitle("Age group (from date of birth)") + ylab("Proportion") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8),
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_fill_discrete(name= "Age", guide = guide_legend(reverse=TRUE))
# #create PDF file
# filename <- (paste0(plot_dir, "que_age_rollup.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# ###Question: AGE YEARS
# plot1 <- ggplot() + 
#   geom_histogram(data=db, aes(age), color="black", fill="white", binwidth = 2) + 
#   theme_bw() + ggtitle("Age (from date of birth)") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_x_continuous(name = "Age", breaks = seq(15, 75, 10), limits=c(14, 76))
# #create PDF file
# filename <- (paste0(plot_dir, "que_age_years.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()
# 
# 
# 
# #Question: TENURE ROLLUP
# #plot
# plot1 <- ggplot(data=db[!is.na(Tenure_Rollup)],aes(x=Store_Role,fill=Tenure_Rollup)) + theme_bw() +
#   geom_bar(position = position_fill(reverse = TRUE)) + 
#   ggtitle("Tenure") + ylab("Proportion") +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8),
#         plot.title = element_text(size = 10, face = "bold")) +
#   scale_fill_discrete(name= "Tenure", guide = guide_legend(reverse=TRUE))
# #create PDF file
# filename <- (paste0(plot_dir, "que_tenure_rollup.pdf"))
# pdf(filename)
# print(plot1)
# graphics.off()

