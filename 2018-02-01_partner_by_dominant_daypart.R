##Analysis of Partner Demographics by Dominant Day-Part Worked
##Author: Julie Morris

##Analysis of Partner Demographics by Dominant Day-Part Worked
##Author: Julie Morris

##load libraries
library(foreign)
library(data.table)
library(ggplot2)
library(tidyverse)
library(eeptools) #age_calc
library(xlsx)
library(janitor) #excel_numeric_to_date
set.seed(98115)

###PART 1: PROFILES BY DOMINANT DAY PART
###DATA: (1) PARTNER LIFE SURVEY (SEPT 2017)
###DATA: (2) TENURE DATA (FLAT FILE FROM LISA)
###DATA: (3) DOMINANT DAY PART BY PARTNER NUMBER (411)
###DATA: (4) RETENTION DATA (415ss)

#set up functions
#calculate mode
calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
#convert SPSS date format into R date format
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")

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
tenuredt[, (names(select(tenuredt,contains("date")))) := 
           lapply(.SD, function(x) excel_numeric_to_date(as.numeric(as.character(x), date_system = "modern"))),
         .SDcols = names(select(tenuredt,contains("date")))]
#calculate days between hire date and sept 15th 2017
tenuredt[, survey_date := '2017-09-15']
tenuredt[, sbux_days := difftime(survey_date,hire_date,units="days")]
tenuredt[, sbux_days := as.numeric(sbux_days)]
tenuredt[, sbux_months := round(sbux_days/30.4167,0)]
tenuredt[, sbux_years := round(sbux_days/365,1)]

#binary for less than 6 months job tenure
tenuredt[job_months<6, less6mo_jobmonths := 1]; tenuredt[job_months>=6, less6mo_jobmonths := 0]
#binary for less than 3 months job tenure
tenuredt[job_months<3, less3mo_jobmonths := 1]; tenuredt[job_months>=3, less3mo_jobmonths := 0]

#merge in tenure data
db <- left_join(db,tenuredt,by="PartnerID")
setDT(db)

#read in daypart data
dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partner_domdaypart.csv") #baristas and shifts
setnames(dp,"PRTNR_NUM","PartnerID")

#recalcualate PM_DOM as necessary
dp[, earlyam_prp := EARLYAM_SHIFTS/SHIFTS_WORKED]
dp[, am_prp := AM_SHIFTS/SHIFTS_WORKED]
dp[, midday_prp := MIDDAY_SHIFTS/SHIFTS_WORKED]
dp[, pm_prp := PM_SHIFTS/SHIFTS_WORKED]
dp[, latepm_prp := LATEPM_SHIFTS/SHIFTS_WORKED]
#calculate dominate daypart
dp[, ddp_tie_first := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]
dp[, ddp_tie_last := colnames(.SD)[max.col(.SD, ties.method="last")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]

#create indicator for a tie
dp[ddp_tie_first==ddp_tie_last, ddp_tie_exists := 0]
dp[ddp_tie_first!=ddp_tie_last, ddp_tie_exists := 1]

#CREATE binarys for if they work dayparts at all
dp[pm_prp==0, YESPM := 0];dp[pm_prp>0, YESPM := 1]
dp[pm_prp==0&latepm_prp==0, YESpmORLATEPM := 0];dp[pm_prp>0|latepm_prp>0, YESpmORLATEPM := 1]
dp[earlyam_prp==0&am_prp==0&midday_prp==0, YESEAMorAMorMID := 0];dp[earlyam_prp>0|am_prp>0|midday_prp>0, YESEAMorAMorMID := 1]

#CREATE binarys for if they do NOT work dayparts, for pie chart
dp[earlyam_prp>0, NOEAM := 0];dp[earlyam_prp==0, NOEAM := 1]
dp[am_prp>0, NOAM := 0];dp[am_prp==0, NOAM := 1]
dp[midday_prp>0, NOMID := 0];dp[midday_prp==0, NOMID := 1]
dp[pm_prp>0, NOPM := 0];dp[pm_prp==0, NOPM := 1]
dp[latepm_prp>0, NOLATEPM := 0];dp[latepm_prp==0, NOLATEPM := 1]
pie2 <- dp[, lapply(.SD,sum,na.rm=T),by=c("JOB_ID"), .SDcols=colnames(dp)[20:24]]
pie2[, N := rowSums(.SD,na.rm=T), .SDcols=(colnames(pie2)[2:6])]
pie2[, NOEAM_prp := round(NOEAM/N,3)]
pie2[, NOAM_prp := round(NOAM/N,3)]
pie2[, NOMID_prp := round(NOMID/N,3)]
pie2[, NOPM_prp := round(NOPM/N,3)]
pie2[, NOLATEPM_prp := round(NOLATEPM/N,3)]

#percent earlyAM/AM/MIDers who also work PM (baristas), etc.
#baristas
dp[JOB_ID==50000362&(ddp_tie_first=="earlyam_prp"|ddp_tie_first=="am_prp"|ddp_tie_first=="midday_prp"), 
   .N/nrow(dp[JOB_ID==50000362&(ddp_tie_first=="earlyam_prp"|ddp_tie_first=="am_prp"|ddp_tie_first=="midday_prp")]), by="YESPM"]
dp[JOB_ID==50000362&ddp_tie_first=="pm_prp", .N/nrow(dp[JOB_ID==50000362&ddp_tie_first=="pm_prp"]), by="YESEAMorAMorMID"]
#shifts
dp[JOB_ID==50000358&(ddp_tie_first=="earlyam_prp"|ddp_tie_first=="am_prp"|ddp_tie_first=="midday_prp"), 
   .N/nrow(dp[JOB_ID==50000358&(ddp_tie_first=="earlyam_prp"|ddp_tie_first=="am_prp"|ddp_tie_first=="midday_prp")]), by="YESpmORLATEPM"]
dp[JOB_ID==50000358&(ddp_tie_first=="pm_prp"|ddp_tie_first=="latepm_prp"), 
   .N/nrow(dp[JOB_ID==50000358&(ddp_tie_first=="pm_prp"|ddp_tie_first=="latepm_prp")]), by="YESEAMorAMorMID"]

#pie chart by percent of shifts in each day part
pie <- dp[, lapply(.SD,sum,na.rm=T),by=c("JOB_ID"), .SDcols=colnames(dp)[2:7]]
pie[, earlyam_prp := round(EARLYAM_SHIFTS/SHIFTS_WORKED,3)]
pie[, am_prp := round(AM_SHIFTS/SHIFTS_WORKED,3)]
pie[, midday_prp := round(MIDDAY_SHIFTS/SHIFTS_WORKED,3)]
pie[, pm_prp := round(PM_SHIFTS/SHIFTS_WORKED,3)]
pie[, latepm_prp := round(LATEPM_SHIFTS/SHIFTS_WORKED,3)]

#distribtion of partners by dominant daypart
dp[ddp_tie_exists==0&JOB_ID==50000362,.N/nrow(dp[ddp_tie_exists==0&JOB_ID==50000362]),by="ddp_tie_first"]
dp[ddp_tie_exists==0&JOB_ID==50000358,.N/nrow(dp[ddp_tie_exists==0&JOB_ID==50000358]),by="ddp_tie_first"]

#rename so they're ordered
dp[ddp_tie_first=="earlyam_prp", DOM5 := "1-EarlyAM"]
dp[ddp_tie_first=="am_prp", DOM5 := "2-AM"]
dp[ddp_tie_first=="midday_prp", DOM5 := "3-Midday"]
dp[ddp_tie_first=="pm_prp", DOM5 := "4-PM"]
dp[ddp_tie_first=="latepm_prp", DOM5 := "5-LatePM"]

#group into 3 groups
dp[ddp_tie_first=="earlyam_prp"|ddp_tie_first=="am_prp"|ddp_tie_first=="midday_prp", DOM3 := "1-AMMidday"]
dp[ddp_tie_first=="pm_prp", DOM3 := "2-PM"]
dp[ddp_tie_first=="latepm_prp", DOM3 := "3-LatePM"]

#group into 2 groups
dp[ddp_tie_first=="earlyam_prp"|ddp_tie_first=="am_prp"|ddp_tie_first=="midday_prp", DOM2 := "1-AMMidday"]
dp[ddp_tie_first=="pm_prp"|ddp_tie_first=="latepm_prp", DOM2 := "2-PMLatePM"]

#CREATE binarys for if they work dayparts at all
dp[pm_prp==0, YESPM := 0];dp[pm_prp>0, YESPM := 1]
dp[pm_prp==0&latepm_prp==0, YESpmORLATEPM := 0];dp[pm_prp>0|latepm_prp>0, YESpmORLATEPM := 1]
dp[earlyam_prp==0&am_prp==0&midday_prp==0, YESEAMorAMorMID := 0];dp[earlyam_prp>0|am_prp>0|midday_prp>0, YESEAMorAMorMID := 1]

#merge in daypart data
db <- left_join(db,dp,by=c("PartnerID"))
setDT(db)
#drop baristas missing dominant daypart info
db <- na.omit(db,cols="ddp_tie_first")

##ATTACH TO RETENTION DATA (1/30/18)
#pull in retention data
ap <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/activepartners_2018-01-30.csv") 
setnames(ap,"Personnel_Number","PartnerID")
ap <- ap[PartnerID %in% unique(db[,PartnerID])]
ap <- ap[!duplicated(ap$PartnerID),]
ap[, retainedFY18FP4 := 1]
#merge in
db <- left_join(db,ap,by=c("PartnerID"))
setDT(db)

#convert retained NA's to 0's
db[is.na(db[,retainedFY18FP4]), retainedFY18FP4 := 0]

##outliers
#drop values where Sbux hours worked is 0 or >60 (same as Q1_7_Starbucks_Hours_Worked_Remove_Outliers)
db[Q1_7_Starbucks_Hours_Worked>0 & Q1_7_Starbucks_Hours_Worked<61, sbux_hours_worked := Q1_7_Starbucks_Hours_Worked]

#convert SPSS date format into R date format
db[, dateofbirth := spss2date(db[,DOB])]
db[, age := floor(age_calc(db[,dateofbirth], units = "years"))]

###recode binary variables as 0/1
#more than one job (yes=1;no=0)
db[Q1_1_Recode_Flag_MT1_Job=="No", more_than_one_job := 0]
db[Q1_1_Recode_Flag_MT1_Job=="Yes", more_than_one_job := 1]

##marital status: married or living with partner
db[Q8_1_Marital_Status=="Living with a partner", marital_married_part := 1]
db[Q8_1_Marital_Status=="Married/remarried", marital_married_part := 1]
db[Q8_1_Marital_Status=="Single, never been married", marital_married_part := 0]
db[Q8_1_Marital_Status=="Prefer not to answer", marital_married_part := NA]
db[Q8_1_Marital_Status=="Separated", marital_married_part := 0]
db[Q8_1_Marital_Status=="Divorced or widowed", marital_married_part := 0]

###student status
db[Q8_6_Student_Flag=="Not Student", student := 0]
db[Q8_6_Student_Flag=="Student", student := 1]

#consistent work schedules (very consistent = 1; else = 0)
db[Q2_1_Schedule_Consistency=="Very consistent", sched_very_consistent := 1]
db[Q2_1_Schedule_Consistency=="Somewhat consistent", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Neutral", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Somewhat inconsistent", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Very inconsistent", sched_very_consistent := 0]

#revalue, for labels
db[Q8_5_Health_Coverage=="Affordable Care Act (also referred to as Obamacare or the Health Insurance Marketplace)", Q8_5_Health_Coverage := "Affordable Care Act"]
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

###FOR BARISTAS - 3 dayparts (EARLY AM/AM/MID vs. PM): 
#frequency table: binary & continuous variables
db_bar <- db[Store_Role=="Barista"]
tempbin <- db_bar %>%
  group_by(DOM3) %>%
  summarize(n = n(), 
            more_than_one_job = round(mean(more_than_one_job,na.rm=T),3),
            age = round(mean(age,na.rm=T),1),
            marital_married_part = round(mean(marital_married_part,na.rm=T),3),
            Kids_Flag = round(mean(Kids_Flag,na.rm=T),3),
            student = round(mean(student,na.rm=T),3),
            primary_income = round(mean(Q8_4_Primary_Income_Flag,na.rm=T),3),
            sched_very_consistent = round(mean(sched_very_consistent,na.rm=T),3),
            health_sbux = round(mean(health_sbux,na.rm=T),3),
            educ_highschool = round(mean(educ_highschool,na.rm=T),3),
            educ_bach = round(mean(educ_bach,na.rm=T),3),
            job_months = round(mean(job_months,na.rm=T),1),
            less6mo_jobmonths = round(mean(less6mo_jobmonths,na.rm=T),3),
            less3mo_jobmonths = round(mean(less3mo_jobmonths,na.rm=T),3),
            sbux_months = round(mean(sbux_months,na.rm=T),1),
            sbux_years = round(mean(sbux_years,na.rm=T),1),
            hrs_avg_selfrpt = round(mean(sbux_hours_worked,na.rm=T),1),
            hrs_med_selfrpt = round(median(sbux_hours_worked,na.rm=T),1),
            hrs_avg_SAP = round(mean(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            hrs_med_SAP = round(median(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            reason_join_brand = round(mean(reason_join_brand,na.rm=T),3),
            reason_join_cap = round(mean(reason_join_cap,na.rm=T),3),           
            reason_join_career = round(mean(reason_join_career,na.rm=T),3),
            reason_join_culture = round(mean(reason_join_culture,na.rm=T),3),
            reason_join_flex_sched = round(mean(reason_join_flex_sched,na.rm=T),3),
            reason_join_fun = round(mean(reason_join_fun,na.rm=T),3),
            reason_join_health = round(mean(reason_join_health,na.rm=T),3),
            reason_join_pay = round(mean(reason_join_pay,na.rm=T),3),
            reason_join_prodbev = round(mean(reason_join_prodbev,na.rm=T),3),
            reason_join_values = round(mean(reason_join_values,na.rm=T),3),
            agree_decisions = round(mean(agree_decisions,na.rm=T),3),
            agree_great_team = round(mean(agree_great_team,na.rm=T),3),
            agree_problem_solve = round(mean(agree_problem_solve,na.rm=T),3),
            agree_reco_great = round(mean(agree_reco_great,na.rm=T),3),
            agree_true_self = round(mean(agree_true_self,na.rm=T),3),
            job_char_diff = round(mean(job_char_diff,na.rm=T),3),
            job_char_exciting = round(mean(job_char_exciting,na.rm=T),3),
            job_char_fun = round(mean(job_char_fun,na.rm=T),3),
            job_char_overwhelm = round(mean(job_char_overwhelm,na.rm=T),3),
            job_char_social = round(mean(job_char_social,na.rm=T),3),
            job_char_stress = round(mean(job_char_stress,na.rm=T),3),
            retainedN = sum(retainedFY18FP4,na.rm=T),
            retained = round(mean(retainedFY18FP4,na.rm=T),3))
setDT(tempbin)
#write to .xlsx
write.xlsx(tempbin,"O:/CoOp/CoOp194_PROReportng&OM/Julie/partnerdemos_domdaypart_bar_3cat.xlsx")

#t.tests: PM vs. AM
dbpmam <- db_bar[DOM3=="1-AMMidday"|DOM3=="2-PM"]
t.test(age ~ DOM3, data=dbpmam)
t.test(marital_married_part ~ DOM3, data=dbpmam)
t.test(Kids_Flag ~ DOM3, data=dbpmam)
t.test(student ~ DOM3, data=dbpmam)
t.test(educ_bach ~ DOM3, data=dbpmam)
t.test(job_months ~ DOM3, data=dbpmam)
t.test(sbux_months ~ DOM3, data=dbpmam)
t.test(sbux_hours_worked ~ DOM3, data=dbpmam)
t.test(reason_join_brand ~ DOM3, data=dbpmam)
t.test(reason_join_cap ~ DOM3, data=dbpmam)
t.test(reason_join_career ~ DOM3, data=dbpmam)
t.test(reason_join_culture ~ DOM3, data=dbpmam)
t.test(reason_join_flex_sched ~ DOM3, data=dbpmam)
t.test(reason_join_fun ~ DOM3, data=dbpmam)
t.test(reason_join_health ~ DOM3, data=dbpmam)
t.test(reason_join_pay ~ DOM3, data=dbpmam)
t.test(reason_join_prodbev ~ DOM3, data=dbpmam)
t.test(reason_join_values ~ DOM3, data=dbpmam)
t.test(agree_decisions ~ DOM3, data=dbpmam)
t.test(agree_great_team ~ DOM3, data=dbpmam)
t.test(agree_problem_solve ~ DOM3, data=dbpmam)
t.test(agree_reco_great ~ DOM3, data=dbpmam)
t.test(agree_true_self ~ DOM3, data=dbpmam)
t.test(job_char_diff ~ DOM3, data=dbpmam)
t.test(job_char_exciting ~ DOM3, data=dbpmam)
t.test(job_char_fun ~ DOM3, data=dbpmam)
t.test(job_char_overwhelm ~ DOM3, data=dbpmam)
t.test(job_char_social ~ DOM3, data=dbpmam)
t.test(job_char_stress ~ DOM3, data=dbpmam)
t.test(Hours_Delta_Ideal_From_Stated ~ DOM3, data=dbpmam)
t.test(less6mo_jobmonths ~ DOM3, data=dbpmam)
t.test(less3mo_jobmonths ~ DOM3, data=dbpmam)
t.test(retainedFY18FP4 ~ DOM3, data=dbpmam)

###FOR SHIFTS - 2 dayparts (EARLY AM/AM/MID vs. PM/LATE PM): 

#frequency table: binary & continuous variables
db_ss <- db[Store_Role=="Shift supervisor"]
tempbin <- db_ss %>%
  group_by(DOM2) %>%
  summarize(n = n(), 
            more_than_one_job = round(mean(more_than_one_job,na.rm=T),3),
            age = round(mean(age,na.rm=T),1),
            marital_married_part = round(mean(marital_married_part,na.rm=T),3),
            Kids_Flag = round(mean(Kids_Flag,na.rm=T),3),
            student = round(mean(student,na.rm=T),3),
            primary_income = round(mean(Q8_4_Primary_Income_Flag,na.rm=T),3),
            sched_very_consistent = round(mean(sched_very_consistent,na.rm=T),3),
            health_sbux = round(mean(health_sbux,na.rm=T),3),
            educ_highschool = round(mean(educ_highschool,na.rm=T),3),
            educ_bach = round(mean(educ_bach,na.rm=T),3),
            job_months = round(mean(job_months,na.rm=T),1),
            less6mo_jobmonths = round(mean(less6mo_jobmonths,na.rm=T),3),
            sbux_months = round(mean(sbux_months,na.rm=T),1),
            sbux_years = round(mean(sbux_years,na.rm=T),1),
            hrs_avg_selfrpt = round(mean(sbux_hours_worked,na.rm=T),1),
            hrs_med_selfrpt = round(median(sbux_hours_worked,na.rm=T),1),
            hrs_avg_SAP = round(mean(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            hrs_med_SAP = round(median(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            reason_join_brand = round(mean(reason_join_brand,na.rm=T),3),
            reason_join_cap = round(mean(reason_join_cap,na.rm=T),3),           
            reason_join_career = round(mean(reason_join_career,na.rm=T),3),
            reason_join_culture = round(mean(reason_join_culture,na.rm=T),3),
            reason_join_flex_sched = round(mean(reason_join_flex_sched,na.rm=T),3),
            reason_join_fun = round(mean(reason_join_fun,na.rm=T),3),
            reason_join_health = round(mean(reason_join_health,na.rm=T),3),
            reason_join_pay = round(mean(reason_join_pay,na.rm=T),3),
            reason_join_prodbev = round(mean(reason_join_prodbev,na.rm=T),3),
            reason_join_values = round(mean(reason_join_values,na.rm=T),3),
            agree_decisions = round(mean(agree_decisions,na.rm=T),3),
            agree_great_team = round(mean(agree_great_team,na.rm=T),3),
            agree_problem_solve = round(mean(agree_problem_solve,na.rm=T),3),
            agree_reco_great = round(mean(agree_reco_great,na.rm=T),3),
            agree_true_self = round(mean(agree_true_self,na.rm=T),3),
            job_char_diff = round(mean(job_char_diff,na.rm=T),3),
            job_char_exciting = round(mean(job_char_exciting,na.rm=T),3),
            job_char_fun = round(mean(job_char_fun,na.rm=T),3),
            job_char_overwhelm = round(mean(job_char_overwhelm,na.rm=T),3),
            job_char_social = round(mean(job_char_social,na.rm=T),3),
            job_char_stress = round(mean(job_char_stress,na.rm=T),3),
            Q1_7_Starbucks_Hours_Worked_Remove_Outliers = round(mean(Q1_7_Starbucks_Hours_Worked_Remove_Outliers,na.rm=T),1),
            Q1_8_Starbucks_Ideal_Hours_Remove_Outliers = round(mean(Q1_8_Starbucks_Ideal_Hours_Remove_Outliers,na.rm=T),1),
            Hours_Delta_Ideal_From_Stated = round(mean(Hours_Delta_Ideal_From_Stated,na.rm=T),1))
setDT(tempbin)
#write to .xlsx
write.xlsx(tempbin,"O:/CoOp/CoOp194_PROReportng&OM/Julie/partnerdemos_domdaypart_shift_2cat.xlsx")

#t.tests: eam/am/mid vs pm/latepm
dbamlpm <- db_ss[DOM2=="1-AMMidday"|DOM2=="2-PMLatePM"]
t.test(age ~ DOM2, data=dbamlpm)
t.test(marital_married_part ~ DOM2, data=dbamlpm)
t.test(Kids_Flag ~ DOM2, data=dbamlpm)
t.test(student ~ DOM2, data=dbamlpm)
t.test(educ_bach ~ DOM2, data=dbamlpm)
t.test(job_months ~ DOM2, data=dbamlpm)
t.test(sbux_hours_worked ~ DOM2, data=dbamlpm)
t.test(reason_join_brand ~ DOM2, data=dbamlpm)
t.test(reason_join_cap ~ DOM2, data=dbamlpm)
t.test(reason_join_career ~ DOM2, data=dbamlpm)
t.test(reason_join_culture ~ DOM2, data=dbamlpm)
t.test(reason_join_flex_sched ~ DOM2, data=dbamlpm)
t.test(reason_join_fun ~ DOM2, data=dbamlpm)
t.test(reason_join_health ~ DOM2, data=dbamlpm)
t.test(reason_join_pay ~ DOM2, data=dbamlpm)
t.test(reason_join_prodbev ~ DOM2, data=dbamlpm)
t.test(reason_join_values ~ DOM2, data=dbamlpm)
t.test(agree_decisions ~ DOM2, data=dbamlpm)
t.test(agree_great_team ~ DOM2, data=dbamlpm)
t.test(agree_problem_solve ~ DOM2, data=dbamlpm)
t.test(agree_reco_great ~ DOM2, data=dbamlpm)
t.test(agree_true_self ~ DOM2, data=dbamlpm)
t.test(job_char_diff ~ DOM2, data=dbamlpm)
t.test(job_char_exciting ~ DOM2, data=dbamlpm)
t.test(job_char_fun ~ DOM2, data=dbamlpm)
t.test(job_char_overwhelm ~ DOM2, data=dbamlpm)
t.test(job_char_social ~ DOM2, data=dbamlpm)
t.test(job_char_stress ~ DOM2, data=dbamlpm)
t.test(less6mo_jobmonths ~ DOM2, data=dbamlpm)
t.test(retainedFY18FP4 ~ DOM2, data=dbamlpm)


###FOR BARISTAS WHO *DO NOT WORK PM*
#frequency table: binary & continuous variables
tempbin <- db_bar[Store_Role=="Barista"&YESPM==0] %>%
  group_by(YESPM) %>%
  summarize(n = n(), 
            more_than_one_job = round(mean(more_than_one_job,na.rm=T),3),
            age = round(mean(age,na.rm=T),1),
            marital_married_part = round(mean(marital_married_part,na.rm=T),3),
            Kids_Flag = round(mean(Kids_Flag,na.rm=T),3),
            student = round(mean(student,na.rm=T),3),
            primary_income = round(mean(Q8_4_Primary_Income_Flag,na.rm=T),3),
            sched_very_consistent = round(mean(sched_very_consistent,na.rm=T),3),
            health_sbux = round(mean(health_sbux,na.rm=T),3),
            educ_highschool = round(mean(educ_highschool,na.rm=T),3),
            educ_bach = round(mean(educ_bach,na.rm=T),3),
            job_months = round(mean(job_months,na.rm=T),1),
            less6mo_jobmonths = round(mean(less6mo_jobmonths,na.rm=T),3),
            sbux_months = round(mean(sbux_months,na.rm=T),1),
            sbux_years = round(mean(sbux_years,na.rm=T),1),
            hrs_avg_selfrpt = round(mean(sbux_hours_worked,na.rm=T),1),
            hrs_med_selfrpt = round(median(sbux_hours_worked,na.rm=T),1),
            hrs_avg_SAP = round(mean(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            hrs_med_SAP = round(median(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            reason_join_brand = round(mean(reason_join_brand,na.rm=T),3),
            reason_join_cap = round(mean(reason_join_cap,na.rm=T),3),           
            reason_join_career = round(mean(reason_join_career,na.rm=T),3),
            reason_join_culture = round(mean(reason_join_culture,na.rm=T),3),
            reason_join_flex_sched = round(mean(reason_join_flex_sched,na.rm=T),3),
            reason_join_fun = round(mean(reason_join_fun,na.rm=T),3),
            reason_join_health = round(mean(reason_join_health,na.rm=T),3),
            reason_join_pay = round(mean(reason_join_pay,na.rm=T),3),
            reason_join_prodbev = round(mean(reason_join_prodbev,na.rm=T),3),
            reason_join_values = round(mean(reason_join_values,na.rm=T),3),
            agree_decisions = round(mean(agree_decisions,na.rm=T),3),
            agree_great_team = round(mean(agree_great_team,na.rm=T),3),
            agree_problem_solve = round(mean(agree_problem_solve,na.rm=T),3),
            agree_reco_great = round(mean(agree_reco_great,na.rm=T),3),
            agree_true_self = round(mean(agree_true_self,na.rm=T),3),
            job_char_diff = round(mean(job_char_diff,na.rm=T),3),
            job_char_exciting = round(mean(job_char_exciting,na.rm=T),3),
            job_char_fun = round(mean(job_char_fun,na.rm=T),3),
            job_char_overwhelm = round(mean(job_char_overwhelm,na.rm=T),3),
            job_char_social = round(mean(job_char_social,na.rm=T),3),
            job_char_stress = round(mean(job_char_stress,na.rm=T),3),
            retainedN = sum(retainedFY18FP4,na.rm=T),
            retained = round(mean(retainedFY18FP4,na.rm=T),3))
setDT(tempbin)
#write to .xlsx
write.xlsx(tempbin,"O:/CoOp/CoOp194_PROReportng&OM/Julie/partnerdemos_domdaypart_bar_nopm_3cat.xlsx")



###PART 2: SELECT PANEL RESPONDENTS FOR DAY PART SURVEY
###DATA: (1) DOMINANT DAY PART BY PARTNER NUMBER (411)
###DATA: (2) PANEL IDS / PARTNER IDS (FLAT FILE FROM SARAH RIGOR)

panel <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/panelistsfordaypart.csv") 
setnames(panel,c("PanelistAlternateId"),c("PRTNR_NUM"))
#keep dp rows where partner numbers are in the panelist id file
dp[, PRTNR_NUM := PartnerID]
dp <- dp[PRTNR_NUM %in% unique(panel[,PRTNR_NUM])]
dp <- dp[!duplicated(dp$PRTNR_NUM),]
panel <- panel[PRTNR_NUM %in% unique(dp[,PRTNR_NUM])]
panel <- panel[!duplicated(panel$PRTNR_NUM),]
dpanel <- merge(dp,panel,by="PRTNR_NUM",all=F)
#exclude ties
dpanel <- dpanel[ddp_tie_exists==0]
dpanel[JOB_ID==50000362&ddp_tie_first=="am_prp"&pm_prp==0&latepm_prp==0, group := 1] #AM BAR (NO PM)
dpanel[JOB_ID==50000362&ddp_tie_first=="am_prp"&(pm_prp>0|latepm_prp>0), group := 2] #AM BAR (some PM)
dpanel[JOB_ID==50000358&ddp_tie_first=="am_prp"&pm_prp==0&latepm_prp==0, group := 3] #AM SHIFT (NO PM)
dpanel[JOB_ID==50000358&ddp_tie_first=="am_prp"&(pm_prp>0|latepm_prp>0), group := 4] #AM SHIFT (some PM)
dpanel[JOB_ID==50000362&ddp_tie_first=="pm_prp", group := 5] #PM BAR
dpanel[JOB_ID==50000358&ddp_tie_first=="pm_prp", group := 6] #PM SHIFT
dpanel[JOB_ID==50000358&ddp_tie_first=="latepm_prp", group := 7] #LATE PM SHIFT
dpanel <- na.omit(dpanel,cols="group")
#randomly assign values
randcol <- runif(nrow(dpanel), min=0, max=1)
dpanel <- cbind(dpanel,randcol)
#rank by group
dpanel <- dpanel %>%
  group_by(group) %>%
  mutate(rankcol = order(randcol,decreasing=TRUE))
setDT(dpanel)
#select sample
dpanel <- dpanel[(group==1&rankcol<=300)|(group==2&rankcol<=200)|(group==3&rankcol<=300)|(group==4&rankcol<=200)|(group==5&rankcol<=500)|(group==6&rankcol<=257)|(group==7&rankcol<=243)]
#keep only partner id and panelist id
panelists <- dpanel[, .(PanelistIdQuestion)]
#write to .xlsx
write.xlsx(panelists,"O:/CoOp/CoOp194_PROReportng&OM/Julie/panelists_for_daypart_survey.xlsx")



###PART 3: ANALYZE DAY PART SURVEY
###DATA: (1) DOMINANT DAY PART BY PARTNER NUMBER (411)
###DATA: (2) PANEL IDS / PARTNER IDS (FLAT FILE FROM SARAH RIGOR)

#READ IN SURVEY RESULTS
surv <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Day Part Research/Final_30.01.18_12.06.12.PM.sav", use.value.labels = FALSE, to.data.frame=TRUE)
setDT(surv)
#merge
dpanelsurv <- merge(dpanel,surv,by="PanelistIdQuestion")
# #matrix of JOB_IDs, dominant daypart from punch data, and dominant daypart from survey
# tempmat <- dpanelsurv[,.N,by=c("JOB_ID","ddp_tie_first","Q3_Daypart_MostOften")]
# setorder(tempmat,JOB_ID,ddp_tie_first,Q3_Daypart_MostOften)

#subset by question
temp <- dpanelsurv[,.(JOB_ID,ddp_tie_first,Q2_Dayparts_Worked_EarlyAMbefore7am,
                  Q2_Dayparts_Worked_AM711am,Q2_Dayparts_Worked_Midday11am2pm,Q2_Dayparts_Worked_PM25pm,
                 Q2_Dayparts_Worked_LatePMafter5pm,Q3_Daypart_MostOften,
                 Q4a_Why_NotWork_AM,Q4b_Why_NotWork_PM,
                 Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,
                 Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,
                 Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,
                 Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,
                 Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,
                 Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday)]
#descriptive tables of why baristas don't work am / pm and why shifts don't work am / pm
temp[JOB_ID==50000362&!is.na(Q4a_Why_NotWork_AM),.N/nrow(temp[JOB_ID==50000362&!is.na(Q4a_Why_NotWork_AM)]),by=c("Q4a_Why_NotWork_AM")]
temp[JOB_ID==50000358&!is.na(Q4a_Why_NotWork_AM),.N/nrow(temp[JOB_ID==50000358&!is.na(Q4a_Why_NotWork_AM)]),by=c("Q4a_Why_NotWork_AM")]
temp[JOB_ID==50000362&!is.na(Q4b_Why_NotWork_PM),.N/nrow(temp[JOB_ID==50000362&!is.na(Q4b_Why_NotWork_PM)]),by=c("Q4b_Why_NotWork_PM")]
temp[JOB_ID==50000358&!is.na(Q4b_Why_NotWork_PM),.N/nrow(temp[JOB_ID==50000358&!is.na(Q4b_Why_NotWork_PM)]),by=c("Q4b_Why_NotWork_PM")]

#descriptives by job id
tempbin <- dpanelsurv %>%
  group_by(JOB_ID) %>%
  summarize(n = n(), 
            q5a_schoolN = sum(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5a_school = round(mean(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5a_careN = sum(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5a_care = round(mean(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5a_jobN = sum(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5a_job = round(mean(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3),
            q5b_schoolN = sum(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5b_school = round(mean(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5b_careN = sum(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5b_care = round(mean(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5b_jobN = sum(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5b_job = round(mean(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3))
setDT(tempbin)
write.xlsx(tempbin,"O:/CoOp/CoOp194_PROReportng&OM/Julie/temp_final_survey_q5.xlsx")

#descriptives by job id and daypart
tempbin2 <- dpanelsurv %>%
  group_by(JOB_ID,Q3_Daypart_MostOften) %>%
  summarize(n = n(), 
            q5a_schoolN = sum(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5a_school = round(mean(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5a_careN = sum(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5a_care = round(mean(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5a_jobN = sum(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5a_job = round(mean(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3),
            q5b_schoolN = sum(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5b_school = round(mean(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5b_careN = sum(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5b_care = round(mean(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5b_jobN = sum(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5b_job = round(mean(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3))
setDT(tempbin2)
write.xlsx(tempbin2,"O:/CoOp/CoOp194_PROReportng&OM/Julie/temp_final_survey_q5_bydaypart.xlsx")



###PART 4: PARTNER EXPERIENCE SURVEY
###DATA: (1) DOMINANT DAY PART BY PARTNER NUMBER (411)
###DATA: (2) PARTNER EXPERIENCE STUDY (WAVE 8, SEPT 2017)

##CONNECTING TO PARTNER EXPERIENCE SURVEY
#pull in data
tr <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Experience Study/11_Wave8_Retail (Sept 2017)/02 Data/PES _Retail_W8_Sept_2017_CLEAN_US.sav", use.value.labels = FALSE, to.data.frame=TRUE)
setDT(tr)
tr <- tr[,.(PartnerID,BlockA_4_TB,BlockA_7_TB,BlockB_7_TB,BlockC_8_TB)]
setnames(tr,"PartnerID","PRTNR_NUM")
dptr <- merge(dp,tr,by="PRTNR_NUM")
#a5.	I like working for my manager
#a8.	My work schedule fits my life
#b18.	I receive the training I need to do my job

#t.tests: PM vs. AM
dptrpmam <- dptr[DOM3=="1-AMMidday"|DOM3=="2-PM"]
t.test(BlockA_4_TB ~ DOM3, data=dptrpmam[JOB_ID==50000362]) #like my manager
t.test(BlockA_4_TB ~ DOM3, data=dptrpmam[JOB_ID==50000358]) #like my manager
t.test(BlockA_7_TB ~ DOM3, data=dptrpmam[JOB_ID==50000362]) #sched
t.test(BlockA_7_TB ~ DOM3, data=dptrpmam[JOB_ID==50000358]) #sched
t.test(BlockB_7_TB ~ DOM3, data=dptrpmam[JOB_ID==50000362]) #training
t.test(BlockB_7_TB ~ DOM3, data=dptrpmam[JOB_ID==50000358]) #training
t.test(BlockC_8_TB ~ DOM3, data=dptrpmam[JOB_ID==50000362]) #engaged
t.test(BlockC_8_TB ~ DOM3, data=dptrpmam[JOB_ID==50000358]) #engaged

#frequency table: binary & continuous variables
tempbin <- dptr %>%
  group_by(JOB_ID,ddp_tie_first) %>%
  summarize(n = n(), 
            likemymgrN = sum(BlockA_4_TB,na.rm=T),
            likemymgr = round(mean(BlockA_4_TB,na.rm=T),3),
            schedulefitslifeN = sum(BlockA_7_TB,na.rm=T),
            schedulefitslife = round(mean(BlockA_7_TB,na.rm=T),3),
            trainingineedN = sum(BlockB_7_TB,na.rm=T),
            trainingineed = round(mean(BlockB_7_TB,na.rm=T),3),
            engageN = sum(BlockC_8_TB,na.rm=T),
            engage = round(mean(BlockC_8_TB,na.rm=T),3))
setDT(tempbin)
write.xlsx(tempbin,"O:/CoOp/CoOp194_PROReportng&OM/Julie/partnerexp_grouped.xlsx")



###PART 5: TENURE DATA FROM FULL SAMPLE
###DATA: (1) DOMINANT DAY PART BY PARTNER NUMBER (411)
###DATA: (2) TENURE DATA (FLAT FILE FROM LISA)

##read in tenure data
tenuredt <- read.csv("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/partner_tenure.csv", fileEncoding="UTF-8-BOM")
setDT(tenuredt)
#subset tenuredt to only active PartnerIDs for recoding, pre-mergin
setnames(tenuredt,"PartnerID","PRTNR_NUM")

#convert serial date format into R date format
tenuredt[, (names(select(tenuredt,contains("date")))) := 
           lapply(.SD, function(x) excel_numeric_to_date(as.numeric(as.character(x), date_system = "modern"))),
         .SDcols = names(select(tenuredt,contains("date")))]
#calculate days between hire date and sept 15th 2017
tenuredt[, survey_date := '2017-09-15']
tenuredt[, sbux_days := difftime(survey_date,hire_date,units="days")]
tenuredt[, sbux_days := as.numeric(sbux_days)]
tenuredt[, sbux_months := round(sbux_days/30.4167,0)]
tenuredt[, sbux_years := round(sbux_days/365,1)]

#binary for less than 6 months job tenure
tenuredt[job_months<6, less6mo_jobmonths := 1]; tenuredt[job_months>=6, less6mo_jobmonths := 0]
#binary for less than 3 months job tenure
tenuredt[job_months<3, less3mo_jobmonths := 1]; tenuredt[job_months>=3, less3mo_jobmonths := 0]
#merge
dptenure <- merge(dp,tenuredt,by="PRTNR_NUM")

#sample sizes
dptenure[,.N,by=c("JOB_ID","DOM3")]

#do not work PM
dptenure[pm_prp==0, YESPM := 0];dptenure[pm_prp>0, YESPM := 1]
dptenure[JOB_ID==50000362&YESPM==0,mean(less6mo_jobmonths,na.rm=T)]

tempbin <- dptenure %>%
  group_by(JOB_ID,DOM3) %>%
  summarize(n = n(), 
            less6mo_jobmonthsN = sum(less6mo_jobmonths,na.rm=T),
            less6mo_jobmonths = round(mean(less6mo_jobmonths,na.rm=T),3))
setDT(tempbin)

