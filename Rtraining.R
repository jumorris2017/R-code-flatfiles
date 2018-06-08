###SESSION 1: Hello, R! Data exploration!

#install packages: only need to *install* a package once, but then must load it with each new R session
install.packages("data.table")
#load your package:
library(data.table)

###SESSION 1: Functions! Aggregation! 

#clear your workspace
rm(list=ls())

#read in your data from last time
dt <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Rtraining.csv")

#time to expore your data!
#see your data
head(dt) #shows first 6 rows (6 = default)
head(dt, 10) #shows first 10 rows (can set this to any number!)
#if you're used to looking at your data in a spreadsheet format, you can!
View(dt) #opens up data.table in viewing window
#summarize & understand you data
summary(dt) #statistical summary of your data
class(dt) #class of your data
ls(dt) #lists the elements of your data
sapply(dt,class) #classes of the elements of your data

#what is a data.table?
#R views a data.table as a matrix of rows and columns, identified using matrix notation
#i.e., DT[rows,columns]

#What will the following show you?
dt[1, ]
dt[ ,1]
dt[1,1]

#data exploration: univariate

#let's explore one variable: BAvgHrs!
#why do these give you the same summary values?
summary(dt[,BAvgHrs])

#visually explore the distribution of BAvgHrs
hist(dt[,BAvgHrs])
#well, that's ugly! 
hist(dt[,BAvgHrs], main="Histogram of Average Hours", 
     xlab="BAvgHrs", 
     ylab="Hours",
     breaks=15)

#let's analyze our cars by those that are "high" versus "low" fuel efficiency
#how many cars in our data get less than (or equal to) 20 BAvgHrs?
nrow(dt[BAvgHrs<=20,])
#how many cars in our data get over 20 BAvgHrs?
nrow(dt[BAvgHrs>20,])

#let's create a new binary variable differentiaing these cars
dt[BAvgHrs<=20, high_BAvgHrs := 0]
dt[BAvgHrs>20, high_BAvgHrs := 1]

#let's check our work
#get counts by one variable
dt[, .N, by="high_BAvgHrs"]
#get counts by two variables
dt[, .N, by=c("high_BAvgHrs","BAvgHrs")]
#get proportions by one variable
dt[, .N/nrow(dt), by="high_BAvgHrs"]
#get proportions by two variables
dt[, .N/nrow(dt), by=c("high_BAvgHrs","BAvgHrs")]

#let's make a data.table with 3 variables we want to correlate
#notice what happens in the "Global Environment" when you do this
dt_cor <- dt[, .(BAvgHrs, ASM, Cosd2018)]
#let's rename two of them
setnames(dt_cor,c("ASM","Cosd2018"),c("ASMnewname","Cosd2018newname"))
#corelation matrix
cor(dt_cor)

#e.g., "mean"
#use R's mean function to calculate the mean of BAvgHrs
mean(dt[,BAvgHrs],na.rm=T)
dt[is.na(dt[,ASM]), ASM := 1]
#write you (full) data to a .csv file
write.csv(dt, file="O:/CoOp/CoOp194_PROReportng&OM/Julie/Rtraining_v2.csv")