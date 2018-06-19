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