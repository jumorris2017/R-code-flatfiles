# library(data.table)
# 
# #load data
# rm <- fread("C:/Users/jumorris/Documents/GUIDs_transactions_homestore.csv")
# 
# #if visits <= 2, then no home store
# rm[TRANS<=2, HOMESTORE := 0]
# #if visits >= 3, and went to a store at least twice, then yes home store
# rm[, trans_min_stores := TRANS - UNIQUE_STORES]
# rm[TRANS>=3&trans_min_stores>=1, HOMESTORE := 1]
# rm[TRANS>=3&trans_min_stores==0, HOMESTORE := 0]
# rm[, trans_min_stores := NULL]
# 
# write.csv(rm, file="C:/Users/jumorris/Documents/GUIDs_transactions_homestore_P8FY18.csv")


#401(k) MaxDiff Analysis
#June 2018

library(data.table)
library(foreign)
library(tidyverse)
md <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/401k Investment Options/Data/FinalDataFile_18.06.18_08.01.39.AM.sav", use.value.labels = FALSE, to.data.frame=TRUE)
setDT(md)

#make binary variables
md[NonStore_Flag==1, Store_Flag := 0]
md[NonEnrollee_Flag==1, Enrollee_Flag := 0]
md[Age_Rollup<=7, Age50plus := 0];md[Age_Rollup>=8, Age50plus := 1]
md[Q2_1_InvestmentChoices==1, SingleTDF := 1]
md[Q2_1_InvestmentChoices>=2&Q2_1_InvestmentChoices<=5, SingleTDF := 0]
md[Q2_5_OtherInvestments==1, Sbux401k50pctplus := 1]
md[Q2_5_OtherInvestments==2, Sbux401k50pctplus := 0]

#build out groups
md[Enrollee_Flag==1&Store_Flag==1, group1 := 1]
md[Enrollee_Flag==1&Store_Flag==0, group2 := 1]
md[Enrollee_Flag==1&Store_Flag==1&Age50plus==1, group3 := 1]
md[Enrollee_Flag==1&Store_Flag==0&Age50plus==1, group4 := 1]
md[Enrollee_Flag==1&Store_Flag==1&SingleTDF==1, group5 := 1]
md[Enrollee_Flag==1&Store_Flag==0&SingleTDF==1, group6 := 1]
md[Enrollee_Flag==1&Store_Flag==1&Sbux401k50pctplus==1, group7 := 1]
md[Enrollee_Flag==1&Store_Flag==0&Sbux401k50pctplus==1, group8 := 1]

#maxdiff scenarios
#sum up every response
md[Q3_4_ESG_MaxDiff_1==1, mdA1 := 1];md[Q3_4_ESG_MaxDiff_1==2, mdA1 := 0]
md[Q3_4_ESG_MaxDiff_1==2, mdB1 := 1];md[Q3_4_ESG_MaxDiff_1==1, mdB1 := 0]

md[Q3_4_ESG_MaxDiff_2==1, mdA2 := 1];md[Q3_4_ESG_MaxDiff_2==2, mdA2 := 0]
md[Q3_4_ESG_MaxDiff_2==2, mdC1 := 1];md[Q3_4_ESG_MaxDiff_2==1, mdC1 := 0]

md[Q3_4_ESG_MaxDiff_3==1, mdB2 := 1];md[Q3_4_ESG_MaxDiff_3==2, mdB2 := 0]
md[Q3_4_ESG_MaxDiff_3==2, mdC2 := 1];md[Q3_4_ESG_MaxDiff_3==1, mdC2 := 0]

md[Q3_4_ESG_MaxDiff_4==1, mdA3 := 1];md[Q3_4_ESG_MaxDiff_4==2, mdA3 := 0]
md[Q3_4_ESG_MaxDiff_4==2, mdD1 := 1];md[Q3_4_ESG_MaxDiff_4==1, mdD1 := 0]

md[Q3_4_ESG_MaxDiff_5==1, mdD2 := 1];md[Q3_4_ESG_MaxDiff_5==2, mdD2 := 0]
md[Q3_4_ESG_MaxDiff_5==2, mdC3 := 1];md[Q3_4_ESG_MaxDiff_5==1, mdC3 := 0]

md[Q3_4_ESG_MaxDiff_6==1, mdD3 := 1];md[Q3_4_ESG_MaxDiff_6==2, mdD3 := 0]
md[Q3_4_ESG_MaxDiff_6==2, mdB3 := 1];md[Q3_4_ESG_MaxDiff_6==1, mdB3 := 0]

#sum up
md[, mdAsum := rowSums(.SD, na.rm = TRUE), .SDcols = c("mdA1", "mdA2", "mdA3")]
md[, mdBsum := rowSums(.SD, na.rm = TRUE), .SDcols = c("mdB1", "mdB2", "mdB3")]
md[, mdCsum := rowSums(.SD, na.rm = TRUE), .SDcols = c("mdC1", "mdC2", "mdC3")]
md[, mdDsum := rowSums(.SD, na.rm = TRUE), .SDcols = c("mdD1", "mdD2", "mdD3")]
#total responses
md[, mdAresp := rowSums(!is.na(.SD)), .SDcols = c("mdA1", "mdA2", "mdA3")]
md[, mdBresp := rowSums(!is.na(.SD)), .SDcols = c("mdB1", "mdB2", "mdB3")]
md[, mdCresp := rowSums(!is.na(.SD)), .SDcols = c("mdC1", "mdC2", "mdC3")]
md[, mdDresp := rowSums(!is.na(.SD)), .SDcols = c("mdD1", "mdD2", "mdD3")]

#RESULTS!
#GROUP 1 - ENROLLED STORE PARTNERS
g1 <- md %>%
  filter(group1==1) %>%
  summarise (group = "group_1", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g1)
#GROUP 2 - ENROLLED NON-STORE PARTNERS
g2 <- md %>%
  filter(group2==1) %>%
  summarise (group = "group_2", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g2)
#GROUP 3 - ENROLLED STORE PARTNERS AGED 50+
g3 <- md %>%
  filter(group3==1) %>%
  summarise (group = "group_3", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g3)
#GROUP 4 - ENROLLED NON-STORE PARTNERS AGED 50+
g4 <- md %>%
  filter(group4==1) %>%
  summarise (group = "group_4", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g4)
#GROUP 5 - ENROLLED STORE PARTNERS WHO CHOSE A SINGLE TDF
g5 <- md %>%
  filter(group5==1) %>%
  summarise (group = "group_5", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g5)
#GROUP 6 - ENROLLED NON-STORE PARTNERS WHO CHOSE A SINGLE TDF
g6 <- md %>%
  filter(group6==1) %>%
  summarise (group = "group_6", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g6)
#GROUP 7 - ENROLLED STORE PARTNERS WITH 401(k) balance >50%
g7 <- md %>%
  filter(group7==1) %>%
  summarise (group = "group_7", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g7)
#GROUP 8 - ENROLLED NON-STORE PARTNERS WITH 401(k) balance >50%
g8 <- md %>%
  filter(group8==1) %>%
  summarise (group = "group_8", n = n(), 
             mdAscore = round(sum(mdAsum)/sum(mdAresp),3),
             mdBscore = round(sum(mdBsum)/sum(mdBresp),3),
             mdCscore = round(sum(mdCsum)/sum(mdCresp),3),
             mdDscore = round(sum(mdDsum)/sum(mdDresp),3))
setDT(g8)

#rbindlist
l = list(g1,g2,g3,g4,g5,g6,g7,g8)
mdresults <- rbindlist(l, use.names=TRUE, fill=TRUE)
#setnames
setnames(mdresults,c("mdAscore","mdBscore","mdCscore","mdDscore"),
         c("A_Replace","B_Additional3","C_Current","D_Additional1"))

#write.csv
write.csv(mdresults,file="//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/401k Investment Options/2018-06-19_401k_MaxDiffresults.csv")




