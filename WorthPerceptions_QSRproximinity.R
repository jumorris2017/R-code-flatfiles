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
# #split by qsr number - without urbanity
# qsr4 <- qsr[miles<0.1]
# prob = c(1/4,2/4,3/4,1)
# temp <- qsr4 %>% summarise( 
#   qsrn_25 = quantile(qsrn, probs = prob[1], na.rm = T), 
#   qsrn_50 = quantile(qsrn, probs = prob[2], na.rm = T), 
#   qsrn_75 = quantile(qsrn, probs = prob[3], na.rm = T), 
#   qsrn_100 = quantile(qsrn, probs = prob[4], na.rm = T)
# )
# qsr4 <- cbind(qsr4, temp)
# setDT(qsr4)
# #recode based on quartiles
# qsr4[qsrn < qsrn_25, qsrn_qtile := 1]
# qsr4[qsrn >= qsrn_25 & qsrn < qsrn_50, qsrn_qtile := 2]
# qsr4[qsrn >= qsrn_50 & qsrn < qsrn_75, qsrn_qtile := 3]
# qsr4[qsrn >= qsrn_75, qsrn_qtile := 4]
# #summarize
# qsr4 <- qsr4[, list(wp_resp_count = sum(wp_resp_count,na.rm=T),
#                     wp_tb_count = sum(wp_tb_count,na.rm=T),
#                     wp_tb_score = sum(wp_tb_count,na.rm=T)/sum(wp_resp_count,na.rm=T),
#                     miles = mean(miles,na.rm=T)),
#              by=c("qsrn_qtile")]
# qsr4 <- setorder(qsr4,qsrn_qtile)

##make comps quartile factor for grouping
qsr3[, qsrn_qtile := as.factor(qsrn_qtile)]
#set labels
lname <- "QSR Count Quartile"
llabels <- c("25th", "50th", "75th", "100th") 
#plot of comps quartiles with average CC top box score for each
#set up unique elements
DT <- copy(qsr3[urbanity=="U2"])
maintitle <- "Worth Perceptions by QSR Count - Urban Core"
ylabel <- "WP Top Box Score"
xlabel <- "QSR Count Quartile"
xvar <- DT[,qsrn_qtile]
yvar <- DT[,wp_tb_score]
pdata <- DT
#plot
ggplot(data = pdata, aes(x = xvar, y = yvar*100)) +
  geom_bar(stat="identity", width = 0.7, fill="lightblue", colour="black") + theme_bw() + 
  ggtitle(maintitle) + guides(fill=FALSE) +
  scale_y_continuous(limits=c(0,40)) +
  geom_text(size = 5, aes(label=paste0("WP = ",round(yvar,3)*100,"%"),y=0), stat= "identity", vjust = -1.75) +
  geom_text(size = 5, aes(label=c("QSRs <3","QSRs 4-5","QSRs 6-7","QSRs 8+"),y=0), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = xlabel, y = ylabel) 





