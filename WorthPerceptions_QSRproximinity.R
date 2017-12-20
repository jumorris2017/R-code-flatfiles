##worth perceptions for Alberta

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)
library(Hmisc)

#load data
qsr <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worthperceptions_QSRproximinity.csv")
setnames(qsr,c("SBUX_STORENUM","QSR_Count","miles_closest"),c("store_num","qsrn","miles"))
qsr[, qsrn := as.numeric(qsrn)]
qcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worthperceptions_QSRproximinity_FY17Q4.csv")
names(qcc) <- tolower(names(qcc))
qcc <- qcc[, lapply(.SD, function(x) as.numeric(x))]
#keep only stores in both data.tables
qcc <- qcc[store_num %in% unique(qsr[,store_num])]
qsr <- qsr[store_num %in% unique(qcc[,store_num])]
#keep only variables we need for worth perceptions data
#qcc <- qcc[, c("store_num","wp_tb_score"), with=F]
#merge together
qsr <- merge(qsr,qcc,by="store_num",all=T)
qsr <- na.omit(qsr, cols=c("qsrn","miles"))

#correlations
rcorr(qsr[,miles],qsr[,wp_tb_score],type="pearson")
rcorr(qsr[,qsrn],qsr[,wp_tb_score],type="pearson")
pcor.test(qsr[,qsrn],qsr[,wp_tb_score],qsr[,miles],method="pearson")

# #split by qsr number
# prob = c(1/4,2/4,3/4,1)
# temp <- qsr %>% group_by(urbanity) %>% summarise( 
#   qsrn_25 = quantile(qsrn, probs = prob[1], na.rm = T), 
#   qsrn_50 = quantile(qsrn, probs = prob[2], na.rm = T), 
#   qsrn_75 = quantile(qsrn, probs = prob[3], na.rm = T), 
#   qsrn_100 = quantile(qsrn, probs = prob[4], na.rm = T)
# )
# qsr <- left_join(qsr, temp,by="urbanity")
# setDT(qsr)
# #recode based on quartiles
# qsr[qsrn < qsrn_25, qsrn_qtile := 1]
# qsr[qsrn >= qsrn_25 & qsrn < qsrn_50, qsrn_qtile := 2]
# qsr[qsrn >= qsrn_50 & qsrn < qsrn_75, qsrn_qtile := 3]
# qsr[qsrn >= qsrn_75, qsrn_qtile := 4]
# 
# #split by qsr miles
# prob = c(1/4,2/4,3/4,1)
# temp <- qsr %>% group_by(urbanity) %>% summarise( 
#   mi_25 = quantile(miles, probs = prob[1], na.rm = T), 
#   mi_50 = quantile(miles, probs = prob[2], na.rm = T), 
#   mi_75 = quantile(miles, probs = prob[3], na.rm = T), 
#   mi_100 = quantile(miles, probs = prob[4], na.rm = T)
# )
# qsr <- left_join(qsr, temp,by="urbanity")
# setDT(qsr)
# #recode based on quartiles
# qsr[miles < mi_25, mi_qtile := 1]
# qsr[miles >= mi_25 & miles < mi_50, mi_qtile := 2]
# qsr[miles >= mi_50 & miles < mi_75, mi_qtile := 3]
# qsr[miles >= mi_75, mi_qtile := 4]

#summarize
qsr2a <- qsr[, list(wp_resp_count = sum(wp_resp_count,na.rm=T),
                   wp_tb_count = sum(wp_tb_count,na.rm=T),
                   wp_tb_score = sum(wp_tb_count,na.rm=T)/sum(wp_resp_count,na.rm=T),
                   miles = mean(miles,na.rm=T)),
            by=c("qsrn_qtile","urbanity")]
qsr2a <- setorder(qsr2a,urbanity,qsrn_qtile)
#summarize
qsr2b <- qsr[, list(wp_resp_count = sum(wp_resp_count,na.rm=T),
                   wp_tb_count = sum(wp_tb_count,na.rm=T),
                   wp_tb_score = sum(wp_tb_count,na.rm=T)/sum(wp_resp_count,na.rm=T),
                   miles = mean(miles,na.rm=T)),
            by=c("mi_qtile","urbanity")]
qsr2b <- setorder(qsr2b,urbanity,mi_qtile)

#qsr subset
qsr3 <- qsr[urbanity=="U2"|urbanity=="U4"|urbanity=="U5"]

#rcorr(qsr3[,miles],qsr3[,wp_tb_score],type="pearson")
rcorr(qsr3[,qsrn],qsr3[,wp_tb_score],type="pearson")
pcor.test(qsr3[,qsrn],qsr3[,wp_tb_score],qsr3[,miles],method="pearson")

#split by qsr number
#qsr3 <- qsr[miles<0.1]
qsr3 <- qsr[miles<0.2]
prob = c(1/4,2/4,3/4,1)
temp <- qsr3 %>% group_by(urbanity) %>% summarise( 
  qsrn_25 = quantile(qsrn, probs = prob[1], na.rm = T), 
  qsrn_50 = quantile(qsrn, probs = prob[2], na.rm = T), 
  qsrn_75 = quantile(qsrn, probs = prob[3], na.rm = T), 
  qsrn_100 = quantile(qsrn, probs = prob[4], na.rm = T)
)
qsr3 <- left_join(qsr3, temp,by="urbanity")
setDT(qsr3)
#recode based on quartiles
qsr3[qsrn < qsrn_25, qsrn_qtile := 1]
qsr3[qsrn >= qsrn_25 & qsrn < qsrn_50, qsrn_qtile := 2]
qsr3[qsrn >= qsrn_50 & qsrn < qsrn_75, qsrn_qtile := 3]
qsr3[qsrn >= qsrn_75, qsrn_qtile := 4]
#summarize
qsr3 <- qsr3[, list(wp_resp_count = sum(wp_resp_count,na.rm=T),
                    wp_tb_count = sum(wp_tb_count,na.rm=T),
                    wp_tb_score = sum(wp_tb_count,na.rm=T)/sum(wp_resp_count,na.rm=T),
                    miles = mean(miles,na.rm=T)),
             by=c("qsrn_qtile","urbanity")]
qsr3 <- setorder(qsr3,urbanity,qsrn_qtile)


#split by qsr number - tertile
#qsr3 <- qsr[miles<0.1]
qsr3 <- qsr[miles<0.2]
prob = c(1/3,2/3,1)
temp <- qsr3 %>% group_by(urbanity) %>% summarise( 
  qsrn_33 = quantile(qsrn, probs = prob[1], na.rm = T), 
  qsrn_67 = quantile(qsrn, probs = prob[2], na.rm = T), 
  qsrn_100 = quantile(qsrn, probs = prob[3], na.rm = T)
)
qsr3 <- left_join(qsr3, temp,by="urbanity")
setDT(qsr3)
#recode based on quartiles
qsr3[qsrn < qsrn_33, qsrn_ttile := 1]
qsr3[qsrn >= qsrn_33 & qsrn < qsrn_67, qsrn_ttile := 2]
qsr3[qsrn >= qsrn_67, qsrn_ttile := 3]
#summarize
qsr3 <- qsr3[, list(wp_resp_count = sum(wp_resp_count,na.rm=T),
                    wp_tb_count = sum(wp_tb_count,na.rm=T),
                    wp_tb_score = sum(wp_tb_count,na.rm=T)/sum(wp_resp_count,na.rm=T),
                    miles = mean(miles,na.rm=T)),
             by=c("qsrn_ttile","urbanity")]
qsr3 <- setorder(qsr3,urbanity,qsrn_ttile)

##make comps quartile factor for grouping
qsr3[, qsrn_ttile := as.factor(qsrn_ttile)]
#set labels
lname <- "QSR Count Tertile"
llabels <- c("33rd", "67th", "100th") 
#plot of comps quartiles with average CC top box score for each
#set up unique elements
DT <- copy(qsr3[urbanity=="U2"])
maintitle <- "Worth Perceptions by QSR Count - Urban Core"
ylabel <- "WP Top Box Score"
xlabel <- "QSR Count Tertile"
xvar <- DT[,qsrn_ttile]
yvar <- DT[,wp_tb_score]
pdata <- DT
#plot
ggplot(data = pdata, aes(x = xvar, y = yvar*100)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() + 
  ggtitle(maintitle) + guides(fill=FALSE) +
  scale_y_continuous(limits=c(0,40)) +
  geom_text(size = 5, aes(label=paste0("WP = ",round(yvar,3)*100,"%"),y=0), stat= "identity", vjust = -1.75) +
  geom_text(size = 5, aes(label=c("QSRs <3","QSRs 4-6","QSRs 7+"),y=0), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = xlabel, y = ylabel) 






##12/15/17 updates for Mike
#load data
qsr <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worthperceptions_QSRproximinity.csv")
setnames(qsr,c("SBUX_STORENUM","QSR_Count","miles_closest"),c("store_num","qsrn","miles"))
qsr[, qsrn := as.numeric(qsrn)]
regi <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worthperceptions_QSRproximinity_regioncodes.csv")
setnames(regi,c("STORE_NUM","RGN_ORG_LVL_ID","RGN_ORG_LVL_DESCR"),c("store_num","regcd","regid"))
#keep only stores in both data.tables
regi <- regi[store_num %in% unique(qsr[,store_num])]

temp5 <- copy(qsr)
temp5 <- left_join(temp5,regi,by="store_num")
setDT(temp5)




temp5 <- temp5[, c("store_num","regid","urbanity")]
temp5[, store_num := 1]
temp5 <- temp5[, list(Store_Count = sum(store_num)), by=c("regid","urbanity")]
temp <- temp5[, list(Store_Count = sum(Store_Count)), by=c("urbanity")]
setnames(temp,"Store_Count","Urbanity_Store_Count")
temp5 <- left_join(temp5,temp,by="urbanity")
setDT(temp5)
temp5[, totalN := 8309]

temp5[, Percent_of_Urbanity := round(Store_Count/Urbanity_Store_Count,3)*100]
temp5[, Percent_of_TotalStores := round(Store_Count/totalN,3)*100]

temp5[, Index_UrbanToTotal_Pct := round((Percent_of_Urbanity/Percent_of_TotalStores)/100,3)]

#change infinite values to 0
temp5[mapply(is.infinite, temp5)] <- 0

temp5[, Index_Flag := 0]
temp5[Index_UrbanToTotal_Pct>=.8&Index_UrbanToTotal_Pct<=1.2, Index_Flag := 1]
temp5[,totalN := NULL]
temp5 <- setorder(temp5,urbanity,-Index_UrbanToTotal_Pct)
write.xlsx(temp5,file="C:/Users/jumorris/UrbanCore_StoreCount-by-Region.xlsx")





