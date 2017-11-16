##Recodes Partner Life open-ended response codes into nets (binary)
##Author: Julie Morris

##read in tenure data
oedb <- read.csv("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/Open End Codes/partner_life_verbatims_and_codes_julie.csv", fileEncoding="UTF-8-BOM")
setDT(oedb)
oedb <- na.omit(oedb, cols="Store_Role")

#define verbatim codes
#
q1_6a <- c(1,2,3,4,6,7,9,21)
q1_6b <- c(10)
q1_6c <- c(11,12,13,16,20)
q1_6d <- c(17,18,19,24,27)
q1_6e <- c(5,25)
q1_6f <- c(8)
q1_6g <- c(28)
q1_6h <- c(14,15,22,23,26,95,29,99)
#
q2_3a <- c(1,2,6,10,12,3,5)
q2_3b <- c(4,17,11,15)
q2_3c <- c(8,9,14,20,21,22,23)
q2_3d <- c(19)
q2_3e <- c(16,18,95,99)
#
q3_3a <- c(1,2,3,4,5,6,7,8)
q3_3b <- c(9,10,11,12,13)
q3_3c <- c(14,15,16,17)
q3_3d <- c(18,19,20)
q3_3e <- c(21,22,50)
q3_3f <- c(29,30,31,32,33,34)
q3_3g <- c(97,98,99)
#
q3_4a <- c(7,43,44,45,46,47,48,49)
q3_4b <- c(1,2,18)
q3_4c <- c(5,3,6)
q3_4d <- c(12,24,25,26,27)
q3_4e <- c(8,9,11,14)
q3_4f <- c(15,16,19,20,21,22)
q3_4g <- c(29,30,31,32)
q3_4h <- c(10,17,23,28,33,34,35,36,38,39,40,41,42,50,51,52,53,54,55,56,57,94,95,99)
q3_4i <- c(98)
#
q5_6a <- c(1,3,4,6,7,8,9)
q5_6b <- c(20,21,22,23,24)
q5_6c <- c(29,30,31,32,33)
q5_6d <- c(35,34,38,39,42,44,45,47)
q5_6e <- c(10,11,12,13,14,15,16,18,19,57)
q5_6f <- c(49,50,51,52)
q5_6g <- c(53,54,55,56,59,60)
q5_6h <- c(61,48,62)
q5_6i <- c(17,25,26,27,28,36,41,43,46,58,95,99)
#
q5_9a <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
q5_9b <- c(36,37,38,39,40)
q5_9c <- c(43,41,42,44,45,46,47,48,49)
q5_9d <- c(50,51,52,53,54)
q5_9e <- c(58,55,56,60,69,59,65,67,68,57,61,62,63,64,66,70,71)
q5_9f <- c(72,73,74,75,76,77,78,79,80,81,82,83,84,85)
q5_9g <- c(86,87,88,89,90)
q5_9h <- c(91,92,93,94,95,96,97,98,99)

#id variables
idvars <- names(oedb[,1:8])

#select vars and melt
#dt <- oedb[, -names(oedb %>% select(contains("NET"))), with=F]
dt <- melt(oedb,id=idvars)

# #create net variables
# for (i in seq_along(q2_3vars)) {
#   dt[variable==i & value %in% q2_3a, Q2_3_NET_a := 1]
# }
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6a, Q1_6_NET_finstab := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6a, Q1_6_NET_finstab := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6a, Q1_6_NET_finstab := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6a, Q1_6_NET_finstab := 1]
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6b, Q1_6_NET_consist := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6b, Q1_6_NET_consist := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6b, Q1_6_NET_consist := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6b, Q1_6_NET_consist := 1]
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6c, Q1_6_NET_career := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6c, Q1_6_NET_career := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6c, Q1_6_NET_career := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6c, Q1_6_NET_career := 1]
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6d, Q1_6_NET_workenv := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6d, Q1_6_NET_workenv := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6d, Q1_6_NET_workenv := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6d, Q1_6_NET_workenv := 1]
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6e, Q1_6_NET_benefits := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6e, Q1_6_NET_benefits := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6e, Q1_6_NET_benefits := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6e, Q1_6_NET_benefits := 1]
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6f, Q1_6_NET_flex := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6f, Q1_6_NET_flex := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6f, Q1_6_NET_flex := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6f, Q1_6_NET_flex := 1]
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6g, Q1_6_NET_brand := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6g, Q1_6_NET_brand := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6g, Q1_6_NET_brand := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6g, Q1_6_NET_brand := 1]
#
dt[variable=="Q1_6_OE1Code" & value %in% q1_6h, Q1_6_NET_other := 1]
dt[variable=="Q1_6_OE2Code" & value %in% q1_6h, Q1_6_NET_other := 1]
dt[variable=="Q1_6_OE3Code" & value %in% q1_6h, Q1_6_NET_other := 1]
dt[variable=="Q1_6_OE4Code" & value %in% q1_6h, Q1_6_NET_other := 1]

#
dt[variable=="Q2_3_OE1Code" & value %in% q2_3a, Q2_3_NET_othresp := 1]
dt[variable=="Q2_3_OE2Code" & value %in% q2_3a, Q2_3_NET_othresp := 1]
dt[variable=="Q2_3_OE3Code" & value %in% q2_3a, Q2_3_NET_othresp := 1]
dt[variable=="Q2_3_OE4Code" & value %in% q2_3a, Q2_3_NET_othresp := 1]
dt[variable=="Q2_3_OE5Code" & value %in% q2_3a, Q2_3_NET_othresp := 1]
dt[variable=="Q2_3_OE6Code" & value %in% q2_3a, Q2_3_NET_othresp := 1]
#
dt[variable=="Q2_3_OE1Code" & value %in% q2_3b, Q2_3_NET_healthybal := 1]
dt[variable=="Q2_3_OE2Code" & value %in% q2_3b, Q2_3_NET_healthybal := 1]
dt[variable=="Q2_3_OE3Code" & value %in% q2_3b, Q2_3_NET_healthybal := 1]
dt[variable=="Q2_3_OE4Code" & value %in% q2_3b, Q2_3_NET_healthybal := 1]
dt[variable=="Q2_3_OE5Code" & value %in% q2_3b, Q2_3_NET_healthybal := 1]
dt[variable=="Q2_3_OE6Code" & value %in% q2_3b, Q2_3_NET_healthybal := 1]
#
dt[variable=="Q2_3_OE1Code" & value %in% q2_3c, Q2_3_NET_planahead := 1]
dt[variable=="Q2_3_OE2Code" & value %in% q2_3c, Q2_3_NET_planahead := 1]
dt[variable=="Q2_3_OE3Code" & value %in% q2_3c, Q2_3_NET_planahead := 1]
dt[variable=="Q2_3_OE4Code" & value %in% q2_3c, Q2_3_NET_planahead := 1]
dt[variable=="Q2_3_OE5Code" & value %in% q2_3c, Q2_3_NET_planahead := 1]
dt[variable=="Q2_3_OE6Code" & value %in% q2_3c, Q2_3_NET_planahead := 1]
#
dt[variable=="Q2_3_OE1Code" & value %in% q2_3d, Q2_3_NET_finstab := 1]
dt[variable=="Q2_3_OE2Code" & value %in% q2_3d, Q2_3_NET_finstab := 1]
dt[variable=="Q2_3_OE3Code" & value %in% q2_3d, Q2_3_NET_finstab := 1]
dt[variable=="Q2_3_OE4Code" & value %in% q2_3d, Q2_3_NET_finstab := 1]
dt[variable=="Q2_3_OE5Code" & value %in% q2_3d, Q2_3_NET_finstab := 1]
dt[variable=="Q2_3_OE6Code" & value %in% q2_3d, Q2_3_NET_finstab := 1]
#
dt[variable=="Q2_3_OE1Code" & value %in% q2_3e, Q2_3_NET_other := 1]
dt[variable=="Q2_3_OE2Code" & value %in% q2_3e, Q2_3_NET_other := 1]
dt[variable=="Q2_3_OE3Code" & value %in% q2_3e, Q2_3_NET_other := 1]
dt[variable=="Q2_3_OE4Code" & value %in% q2_3e, Q2_3_NET_other := 1]
dt[variable=="Q2_3_OE5Code" & value %in% q2_3e, Q2_3_NET_other := 1]
dt[variable=="Q2_3_OE6Code" & value %in% q2_3e, Q2_3_NET_other := 1]

#
dt[variable=="Q3_3_1Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_2Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_3Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_4Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_5Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_6Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_7Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_8Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_9Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
dt[variable=="Q3_3_10Code" & value %in% q3_3a, Q3_3_NET_brand := 1]
#
dt[variable=="Q3_3_1Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_2Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_3Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_4Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_5Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_6Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_7Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_8Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_9Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
dt[variable=="Q3_3_10Code" & value %in% q3_3b, Q3_3_NET_corpresp := 1]
#
dt[variable=="Q3_3_1Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_2Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_3Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_4Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_5Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_6Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_7Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_8Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_9Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
dt[variable=="Q3_3_10Code" & value %in% q3_3c, Q3_3_NET_compassion := 1]
#
dt[variable=="Q3_3_1Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_2Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_3Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_4Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_5Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_6Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_7Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_8Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_9Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
dt[variable=="Q3_3_10Code" & value %in% q3_3d, Q3_3_NET_inclusion := 1]
#
dt[variable=="Q3_3_1Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_2Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_3Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_4Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_5Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_6Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_7Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_8Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_9Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
dt[variable=="Q3_3_10Code" & value %in% q3_3e, Q3_3_NET_pplenv := 1]
#
dt[variable=="Q3_3_1Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_2Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_3Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_4Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_5Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_6Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_7Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_8Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_9Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
dt[variable=="Q3_3_10Code" & value %in% q3_3f, Q3_3_NET_benefits := 1]
#
dt[variable=="Q3_3_1Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_2Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_3Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_4Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_5Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_6Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_7Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_8Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_9Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]
dt[variable=="Q3_3_10Code" & value %in% q3_3g, Q3_3_NET_personalexp := 1]

#
dt[variable=="Q3_4_1Code" & value %in% q3_4a, Q3_4_NET_products := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4a, Q3_4_NET_products := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4a, Q3_4_NET_products := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4a, Q3_4_NET_products := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4a, Q3_4_NET_products := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4a, Q3_4_NET_products := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4a, Q3_4_NET_products := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4b, Q3_4_NET_pay := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4b, Q3_4_NET_pay := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4b, Q3_4_NET_pay := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4b, Q3_4_NET_pay := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4b, Q3_4_NET_pay := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4b, Q3_4_NET_pay := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4b, Q3_4_NET_pay := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4c, Q3_4_NET_labor := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4c, Q3_4_NET_labor := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4c, Q3_4_NET_labor := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4c, Q3_4_NET_labor := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4c, Q3_4_NET_labor := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4c, Q3_4_NET_labor := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4c, Q3_4_NET_labor := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4d, Q3_4_NET_support := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4d, Q3_4_NET_support := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4d, Q3_4_NET_support := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4d, Q3_4_NET_support := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4d, Q3_4_NET_support := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4d, Q3_4_NET_support := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4d, Q3_4_NET_support := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4e, Q3_4_NET_welfare := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4e, Q3_4_NET_welfare := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4e, Q3_4_NET_welfare := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4e, Q3_4_NET_welfare := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4e, Q3_4_NET_welfare := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4e, Q3_4_NET_welfare := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4e, Q3_4_NET_welfare := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4f, Q3_4_NET_mgmtpolicy := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4f, Q3_4_NET_mgmtpolicy := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4f, Q3_4_NET_mgmtpolicy := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4f, Q3_4_NET_mgmtpolicy := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4f, Q3_4_NET_mgmtpolicy := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4f, Q3_4_NET_mgmtpolicy := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4f, Q3_4_NET_mgmtpolicy := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4g, Q3_4_NET_negattitudes := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4g, Q3_4_NET_negattitudes := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4g, Q3_4_NET_negattitudes := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4g, Q3_4_NET_negattitudes := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4g, Q3_4_NET_negattitudes := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4g, Q3_4_NET_negattitudes := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4g, Q3_4_NET_negattitudes := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4h, Q3_4_NET_other := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4h, Q3_4_NET_other := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4h, Q3_4_NET_other := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4h, Q3_4_NET_other := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4h, Q3_4_NET_other := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4h, Q3_4_NET_other := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4h, Q3_4_NET_other := 1]
#
dt[variable=="Q3_4_1Code" & value %in% q3_4i, Q3_4_NET_nothing := 1]
dt[variable=="Q3_4_2Code" & value %in% q3_4i, Q3_4_NET_nothing := 1]
dt[variable=="Q3_4_3Code" & value %in% q3_4i, Q3_4_NET_nothing := 1]
dt[variable=="Q3_4_4Code" & value %in% q3_4i, Q3_4_NET_nothing := 1]
dt[variable=="Q3_4_5Code" & value %in% q3_4i, Q3_4_NET_nothing := 1]
dt[variable=="Q3_4_6Code" & value %in% q3_4i, Q3_4_NET_nothing := 1]
dt[variable=="Q3_4_7Code" & value %in% q3_4i, Q3_4_NET_nothing := 1]

#
dt[variable=="Q5_6_1Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6a, Q5_6_NET_frifam := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6b, Q5_6_NET_community := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6b, Q5_6_NET_community := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6c, Q5_6_NET_fooddrink := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6d, Q5_6_NET_relax := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6e, Q5_6_NET_creative := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6f, Q5_6_NET_outdoors := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6g, Q5_6_NET_success := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6g, Q5_6_NET_success := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6h, Q5_6_NET_homeimp := 1]
#
dt[variable=="Q5_6_1Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_2Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_3Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_4Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_5Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_6Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_7Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_8Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_9Code" & value %in% q5_6i, Q5_6_NET_other := 1]
dt[variable=="Q5_6_10Code" & value %in% q5_6i, Q5_6_NET_other := 1]

#
dt[variable=="Q5_9_1Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9a, Q5_9_NET_work := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9a, Q5_9_NET_work := 1]
#
dt[variable=="Q5_9_1Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9b, Q5_9_NET_family := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9b, Q5_9_NET_family := 1]
#
dt[variable=="Q5_9_1Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9c, Q5_9_NET_health := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9c, Q5_9_NET_health := 1]
#
dt[variable=="Q5_9_1Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9d, Q5_9_NET_financial := 1]
#
dt[variable=="Q5_9_1Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9e, Q5_9_NET_wklifebal := 1]
#
dt[variable=="Q5_9_1Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9f, Q5_9_NET_negemotions := 1]
#
dt[variable=="Q5_9_1Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9g, Q5_9_NET_healthy := 1]
#
dt[variable=="Q5_9_1Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_2Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_3Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_4Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_5Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_6Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_7Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_8Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_9Code" & value %in% q5_9h, Q5_9_NET_other := 1]
dt[variable=="Q5_9_10Code" & value %in% q5_9h, Q5_9_NET_other := 1]

#drop "variable" and "value" indicators, and lapply to sum by variable and ID
temp <- dt[, -c("variable","value"), with=FALSE]
temp <- temp[, lapply(.SD, sum, na.rm=TRUE), by=idvars]
#replace any values (except RID) greater than 1 with 1
temp[, names(temp)[-(1:8)] := lapply(.SD, function(x) {ifelse(x > 1, 1, x)}), .SDcols = 9:ncol(temp)]

#write to .csv
write.csv(temp,file="//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/Open End Codes/partner_life_verbatims_coded.csv")


###ANALYSIS


##read in open_end data
oe <- fread("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/Open End Codes/partner_life_verbatims_coded.csv")
#only keep 3 store roles
oe[,V1 := NULL]
#10=barista;11=shift supervisor;14=store manager
oe <- oe[!which(is.na(oe[,Store_Role]))]

#id variables
idvars <- names(oe[,1:8])

##weight vars
vars <- names(oe) %>% .[grepl("NET", .)]
test <- oe[, (vars) := lapply(.SD, function(x) x * oe[,Total_weights]), .SDcols = vars]

##analysis

##set directory
data_dir <- ("C:/Users/jumorris/Desktop/temp_plots/interim_csvs/")

#by store role
#temp <- oe[,names(oe %>% select(contains("NET"))),with=FALSE]
temp1 <- test[, lapply(.SD,mean,na.rm=TRUE), by="Store_Role", .SDcols=names(oe %>% select(contains("NET")))]
#temp3 <- cbind(c("value"),temp)
temp1 <- melt(temp1,id="Store_Role")
temp1[, que := substr(variable, 1, 4)]
#temp2[,V1 := NULL]
temp1[, value := round(value,3)]
#order
temp1 <- setorder(temp1,Store_Role,que,-value)
write.csv(temp1,file=paste0(data_dir,"oe_storerole_weighted.csv"))

#by tenure rollup
temp2 <- test[, lapply(.SD,mean,na.rm=TRUE), by="Tenure_Rollup", .SDcols=names(oe %>% select(contains("NET")))]
temp2 <- melt(temp2,id="Tenure_Rollup")
temp2[, que := substr(variable, 1, 4)]
temp2[, value := round(value,3)]
#order
temp2 <- setorder(temp2,Tenure_Rollup,que,-value)
write.csv(temp2,file=paste0(data_dir,"oe_tenure_weighted.csv"))

#by age
temp3 <- test[, lapply(.SD,mean,na.rm=TRUE), by="Age_Rollup", .SDcols=names(oe %>% select(contains("NET")))]
temp3 <- melt(temp3,id="Age_Rollup")
temp3[, que := substr(variable, 1, 4)]
temp3[, value := round(value,3)]
#order
temp3 <- setorder(temp3,Age_Rollup,que,-value)
write.csv(temp3,file=paste0(data_dir,"oe_age_weighted.csv"))

#by education level
temp4 <- test[, lapply(.SD,mean,na.rm=TRUE), by="Q8_7_Education_Level", .SDcols=names(oe %>% select(contains("NET")))]
temp4 <- melt(temp4,id="Q8_7_Education_Level")
temp4[, que := substr(variable, 1, 4)]
temp4[, value := round(value,3)]
#order
temp4 <- setorder(temp4,Q8_7_Education_Level,que,-value)
write.csv(temp4,file=paste0(data_dir,"oe_education_weighted.csv"))

#by student loans
temp5 <- test[, lapply(.SD,mean,na.rm=TRUE), by="Q8_6_Student_Loans", .SDcols=names(oe %>% select(contains("NET")))]
temp5 <- melt(temp5,id="Q8_6_Student_Loans")
temp5[, que := substr(variable, 1, 4)]
temp5[, value := round(value,3)]
#order
temp5 <- setorder(temp5,Q8_6_Student_Loans,que,-value)
write.csv(temp5,file=paste0(data_dir,"oe_studentloans_weighted.csv"))




