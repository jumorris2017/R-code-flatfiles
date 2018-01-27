##CC by daypart and store
##identifying minimum timeframe
##ideal cell minimum = 70 CE responses

#load libraries
library(data.table)
library(dplyr)

#load data
cs2q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_2quarters.csv") #2 quarters
cs3q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_3quarters.csv") #3 quarters
cs4q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_4quarters.csv") #4 quarters
cs8q <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_8quarters.csv") #8 quarters

#calculate percent of stores with survey counts over 70, by daypart
cs2q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs2q[TOTAL_RSPNS<70, resp_over70 := 0]
cs3q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs3q[TOTAL_RSPNS<70, resp_over70 := 0]
cs4q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs4q[TOTAL_RSPNS<70, resp_over70 := 0]
cs8q[TOTAL_RSPNS>=70, resp_over70 := 1]; cs8q[TOTAL_RSPNS<70, resp_over70 := 0]

#percent of stores
#two quarters
temp2 <- cs2q %>%
  group_by(DAY_PART,resp_over70) %>%
  summarise (storeN = n()) %>%
  mutate(pct = round(storeN / sum(storeN),3)*100)
setDT(temp2)
setorder(temp2,resp_over70,DAY_PART)
#three quarters
temp3 <- cs3q %>%
  group_by(DAY_PART,resp_over70) %>%
  summarise (storeN = n()) %>%
  mutate(pct = round(storeN / sum(storeN),3)*100)
setDT(temp3)
setorder(temp3,resp_over70,DAY_PART)
#4 quarters
temp4 <- cs4q %>%
  group_by(DAY_PART,resp_over70) %>%
  summarise (storeN = n()) %>%
  mutate(pct = round(storeN / sum(storeN),3)*100)
setDT(temp4)
setorder(temp4,resp_over70,DAY_PART)
#8 quarters
temp8 <- cs8q %>%
  group_by(DAY_PART,resp_over70) %>%
  summarise (storeN = n()) %>%
  mutate(pct = round(storeN / sum(storeN),3)*100)
setDT(temp8)
setorder(temp8,resp_over70,DAY_PART)

#descriptives
temp2[resp_over70==1]
temp3[resp_over70==1]
temp4[resp_over70==1]
temp8[resp_over70==1]
#
mean(cs2q[,TOTAL_RSPNS])
mean(cs3q[,TOTAL_RSPNS])
mean(cs4q[,TOTAL_RSPNS])
mean(cs8q[,TOTAL_RSPNS])


