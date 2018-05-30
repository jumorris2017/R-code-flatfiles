

#load libraries
library(data.table)
library(broom)

#fiscal weeks 30-33

#set folder directory
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
be <- fread(paste0(data_dir,"/FW33_ERRORS.csv"))
bd <- fread(paste0(data_dir,"/FW33_DATA.csv"))
bj <- fread(paste0(data_dir,"/Backcast_CE-Partner_data_FW30-33.csv"))
bu <- fread(paste0(data_dir,"/UPLH_FW30-33_FY17.csv"))
bc <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), all = TRUE)}, list(be,bd,bj,bu))
#keep only applicable stores
bc <- na.omit(bc,cols="SALES_ERROR")
#filter outliers
bc <- bc %>% filter(SALES_ERROR < quantile(bc$SALES_ERROR, 0.975))
bc <- bc %>% filter(SALES_ERROR > quantile(bc$SALES_ERROR, 0.025))
setDT(bc)

#gen UPLH delta variable
bc[, UPLH_YOY_DELTA := UPLH-UPLH17]

#MAKE SALES ERROR INVERSE
bc[, SALES_ERROR := (SALES_ERROR)*(-1)]
bc[, TRANS_ERROR := (TRANS_ERROR)*(-1)]

summary(lm(SALES_ERROR ~ 
             CE_TB_STOREOPS + 
             CE_TB_SPEED + CE_TB_SPEED_YOY_DELTA +
             #CE_TB_ACCURACY +
             #CE_TB_BEVTASTE +
             CE_TB_CLEAN +
             SR_PCT + SR_PCT_YOY_DELTA +
             HS_CUST_PCT + HS_CUST_PCT_YOY_DELTA + 
             UPLH + 
             SM1YRSTABLE,
           data=bc))

summary(lm(SALES_ERROR ~ 
             CE_TB_STOREOPS + 
             SR_PCT + SR_PCT_YOY_DELTA +
             HS_CUST_PCT + HS_CUST_PCT_YOY_DELTA + 
             UPLH,
           data=bc))



#tertile
#quartile or ntile
bc <- bc %>% mutate(tertile = ntile(SALES_ERROR, 3))
setDT(bc)
#check out tertiles
bc %>% group_by(tertile) %>%
  summarise(min = min(SALES_ERROR),
            max = max(SALES_ERROR))

#make binary outcome for above norm high, above norm low
bc[tertile==1, high_tert := 0];bc[tertile==3, high_tert := 1]
#probit
prob1 <- glm(high_tert ~ 
               #CE_TB_STOREOPS + 
               SR_PCT + 
               SR_PCT_YOY_DELTA +
               HS_CUST_PCT + 
               HS_CUST_PCT_YOY_DELTA + 
               UPLH + 
               R52_OTW + 
               #R52_WEEKEND + 
               #RTM_BLENDED + 
               #RTM_BREWED + 
               R6W_BLENDED_DELTA +
               R6W_BREWED_COFFEE_ICED_DELTA + 
               R6W_ESPRESSO_BEVERAGES_ICED_DELTA +
               WEATHER_ADJUSTMENT + 
               MARRIED + 
               #HISPANIC + 
               #WHITE + 
               BLACK + 
               #RTM_COLD + 
               ATTACH_RATE +
               #BEV_PER_TRANS + 
               PM_PCT,
               #CONSTRAINED, 
             family = binomial(link = "probit"), 
             data = bc[high_tert==0|high_tert==1])
summary(prob1)
#write.csv(tidy(prob1), paste0(data_dir,"/coefs.csv"))


Regression(high_tert ~ 
             #CE_TB_STOREOPS + 
             SR_PCT + 
             SR_PCT_YOY_DELTA +
             HS_CUST_PCT + 
             HS_CUST_PCT_YOY_DELTA + 
             UPLH + 
             R52_OTW + 
             #R52_WEEKEND + 
             #RTM_BLENDED + 
             #RTM_BREWED + 
             R6W_BLENDED_DELTA +
             R6W_BREWED_COFFEE_ICED_DELTA + 
             R6W_ESPRESSO_BEVERAGES_ICED_DELTA +
             WEATHER_ADJUSTMENT + 
             MARRIED + 
             #HISPANIC + 
             #WHITE + 
             BLACK + 
             #RTM_COLD + 
             ATTACH_RATE +
             #BEV_PER_TRANS + 
             PM_PCT, data=bc[high_tert==0|high_tert==1],
           output = "Relative Importance Analysis")


bc[, sepct := SALES_ERROR*100]
hist(bc[, sepct], breaks=12, 
     main="FW33 Sales Forecast Errors", xlab="Percent Deviation from Forecast")

# library(flipRegression)
# Regression(yvar ~ xvar1 + xvar2 + xvar3, data=datasetname,
#            output = "Relative Importance Analysis")






#make binary outcome for above/below 0
bc[SALES_ERROR<0, high_0 := 0];bc[SALES_ERROR>0, high_0 := 1]
#probit
prob2 <- glm(high_0 ~ 
               CE_TB_STOREOPS + 
               SR_PCT + SR_PCT_YOY_DELTA +
               HS_CUST_PCT + HS_CUST_PCT_YOY_DELTA + 
               UPLH, 
             family = binomial(link = "probit"), 
             data = bc[high_0==0|high_0==1])
summary(prob2)

#make binary outcome for above/below 0.05%
bc[SALES_ERROR<0.05, high_05 := 0];bc[SALES_ERROR>0.05, high_05 := 1]
#probit
prob3 <- glm(high_05 ~ 
               CE_TB_STOREOPS + 
               #SR_PCT + 
               SR_PCT_YOY_DELTA +
               HS_CUST_PCT + HS_CUST_PCT_YOY_DELTA + 
               UPLH, 
             family = binomial(link = "probit"), 
             data = bc[high_05==0|high_05==1])
summary(prob3)

#mean error by categorical variables
bc[!is.na(DRIVE_THRU),mean(SALES_ERROR),by="DRIVE_THRU"]
bc[!is.na(URBANITY),mean(SALES_ERROR),by="URBANITY"]

cor.test(bc[,SALES_ERROR],bc[,R52_OTW])
#cor.test(bc[,SALES_ERROR],bc[,R52_MOP])
cor.test(bc[,SALES_ERROR],bc[,R52_WEEKEND])
cor.test(bc[,SALES_ERROR],bc[,RTM_BLENDED])
cor.test(bc[,SALES_ERROR],bc[,RTM_BREWED])
cor.test(bc[,SALES_ERROR],bc[,RTM_ESPRESSO])
cor.test(bc[,SALES_ERROR],bc[,RTM_FOOD])
cor.test(bc[,SALES_ERROR],bc[,RTM_BREWED])
cor.test(bc[,SALES_ERROR],bc[,WEATHER_ADJUSTMENT])
#cor.test(bc[,SALES_ERROR],bc[,BA_DEGREE])
cor.test(bc[,SALES_ERROR],bc[,MARRIED])
cor.test(bc[,SALES_ERROR],bc[,HISPANIC])
cor.test(bc[,SALES_ERROR],bc[,WHITE])
cor.test(bc[,SALES_ERROR],bc[,BLACK])
#cor.test(bc[,SALES_ERROR],bc[,MEAN_AGE])
#cor.test(bc[,SALES_ERROR],bc[,PER_CAP_INCOME])
#cor.test(bc[,SALES_ERROR],bc[,FEMALE])
#cor.test(bc[,SALES_ERROR],bc[,HH_SIZE])
#cor.test(bc[,SALES_ERROR],bc[,POP_DENSITY])
#cor.test(bc[,SALES_ERROR],bc[,DAYTIME_POP])
cor.test(bc[,SALES_ERROR],bc[,RTM_COLD])
cor.test(bc[,SALES_ERROR],bc[,ATTACH_RATE])
cor.test(bc[,SALES_ERROR],bc[,BEV_PER_TRANS])
cor.test(bc[,SALES_ERROR],bc[,PM_PCT])
cor.test(bc[,SALES_ERROR],bc[,CONSTRAINED])



R52_OTW + R52_WEEKEND + RTM_BLENDED + RTM_BREWED + RTM_ESPRESSO +
  RTM_FOOD + RTM_BREWED + WEATHER_ADJUSTMENT + MARRIED + 
  HISPANIC + WHITE + BLACK + RTM_COLD + ATTACH_RATE +
  BEV_PER_TRANS + PM_PCT + CONSTRAINED



















#set folder directory
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
hs <- fread(paste0(data_dir,"/HSCUST_FW30-33_FY17-18.csv"))










# #probit
# prob2 <- glm(high_tert ~ 
#                CE_TB_STOREOPS + 
#                CE_TB_SPEED + 
#                CE_TB_CLEAN +
#                SR_PCT + SR_PCT_YOY_DELTA +
#                HS_CUST_PCT + HS_CUST_PCT_YOY_DELTA + 
#                UPLH, 
#              family = binomial(link = "probit"), 
#              data = bc[high_tert==0|high_tert==1])
# summary(prob2)


#create delta
bc[, ObsvExp_Delta := ((Sum_SALES/Sum_ACTUAL_SALES)-1)*(-1)]
#bc <- bc[ObsvExp_Delta>=-1&ObsvExp_Delta<=1]

#corr
cor.test(bc[,ObsvExp_Delta],bc[,SM1YRSTABLE])
cor.test(bc[,ObsvExp_Delta],bc[,Q1_AVG_HRLY])
cor.test(bc[,ObsvExp_Delta],bc[,Q1_AVG_SM])
cor.test(bc[,ObsvExp_Delta],bc[,TURNOVER_12MO])
#scatterplots
plot(bc[,ObsvExp_Delta],bc[,SM1YRSTABLE])
plot(bc[,ObsvExp_Delta],bc[,Q1_AVG_HRLY])
plot(bc[,ObsvExp_Delta],bc[,Q1_AVG_SM])
plot(bc[,ObsvExp_Delta],bc[,TURNOVER_12MO])

#agg by CBSA
bcc <- bc[, list(Nstores = .N,
                 Sum_SALES = sum(Sum_SALES,na.rm=T),
                 Sum_ACTUAL_SALES = sum(Sum_ACTUAL_SALES,na.rm=T),
                 Q1_AVG_HRLY = round(mean(Q1_AVG_HRLY,na.rm=T),2),
                 Q1_AVG_SM = round(mean(Q1_AVG_SM,na.rm=T),2),
                 SM1YRSTABLE = round(mean(SM1YRSTABLE,na.rm=T),2),
                 TURNOVER_12MO = round(mean(TURNOVER_12MO,na.rm=T),2)),
          by=c("CBSA_CODE","CBSA_NAME")]

#create delta
bcc[, ObsvExp_Delta := ((Sum_SALES/Sum_ACTUAL_SALES)-1)*(-1)]
bcc <- bcc[ObsvExp_Delta>=-1&ObsvExp_Delta<=1]

#corr
cor.test(bcc[,ObsvExp_Delta],bcc[,SM1YRSTABLE])
cor.test(bcc[,ObsvExp_Delta],bcc[,Q1_AVG_HRLY])
cor.test(bcc[,ObsvExp_Delta],bcc[,Q1_AVG_SM])
cor.test(bcc[,ObsvExp_Delta],bcc[,TURNOVER_12MO])
#scatterplots
plot(bcc[,ObsvExp_Delta],bcc[,SM1YRSTABLE])
plot(bcc[,ObsvExp_Delta],bcc[,Q1_AVG_HRLY])
plot(bcc[,ObsvExp_Delta],bcc[,Q1_AVG_SM])
plot(bcc[,ObsvExp_Delta],bcc[,TURNOVER_12MO])



# cbsalist <- c("Miami","Houston","Dallas","Minneapolis",
#               "Washington DC","Philadelphia","New York")
cbsalist <- c(47900,33100,19100,33460,26420,37980,35620)

#restrict to areas of interest
bcls <- bc[CBSA_CODE %in% cbsalist]
#corr
cor.test(bcls[,ObsvExp_Delta],bcls[,SM1YRSTABLE])
cor.test(bcls[,ObsvExp_Delta],bcls[,Q1_AVG_HRLY])
cor.test(bcls[,ObsvExp_Delta],bcls[,Q1_AVG_SM])
cor.test(bcls[,ObsvExp_Delta],bcls[,TURNOVER_12MO])

# bc[bc$CBSA_NAME %like% "Washington", ] #47900
# bc[bc$CBSA_NAME %like% "Miami", ] #33100
# bc[bc$CBSA_NAME %like% "Dallas", ] #19100
# bc[bc$CBSA_NAME %like% "Minneapolis", ] #33460
# bc[bc$CBSA_NAME %like% "Houston", ] #26420
# bc[bc$CBSA_NAME %like% "Philadelphia", ] #37980
# bc[bc$CBSA_NAME %like% "New York", ] #35620

