###SESSION 1: Hello, R! Data exploration!

#download data
data(mtcars)
#assign as data.table
setDT(mtcars, keep.rownames = T) 

#expore your data
View(mtcars) #opens up data.table in viewing window
head(mtcars) #shows first 6 rows (6 = default)
head(mtcars, 10) #shows first 10 rows (can set this to any number!)
summary(mtcars) #statistical summary of your data
class(mtcars) #class of your data
ls(mtcars) #lists the elements of your data
sapply(mtcars,class) #classes of the elements of your data

#what is a data.table?
#R views a data.table as a matrix of rows and columns, identified using matrix notation
#i.e., DT[rows,columns]

#What will the following show you?
mtcars[1, ]
mtcars[ ,1]
mtcars[1,1]

#data exploration: univariate

#let's explore one variable: MPG!
#why do these give you the same summary values?
summary(mtcars[,2])
summary(mtcars[,mpg])

#visually explore the distribution of MPG
hist(mtcars[,mpg])
#well, that's ugly! 
hist(mtcars[,mpg], main="Histogram of Miles per Gallon", 
     xlab="MPG", 
     ylab="Number of Cars",
     breaks=10)

#let's analyze our cars by those that are "high" versus "low" fuel efficiency
#how many cars in our data get less than (or equal to) 20 mpg?
nrow(mtcars[mpg<=20])
#how many cars in our data get over 20 mpg?
nrow(mtcars[mpg>20])

#let's create a new binary variable differentiaing these cars
mtcars[mpg<=20, high_mpg := 0]
mtcars[mpg>20, high_mpg := 1]

#let's check our work
#get counts by one variable
mtcars[, .N, by="high_mpg"]
#get counts by two variables
mtcars[, .N, by=c("high_mpg","mpg")]
#get proportions by one variable
mtcars[, .N/nrow(mtcars), by="high_mpg"]
#get proportions by two variables
mtcars[, .N/nrow(mtcars), by=c("high_mpg","mpg")]

#data exploration: bivariate
#how are mpgs and horsepower related?
plot(mtcars[,hp],mtcars[,mpg])
#well that's ugly!
plot(mtcars[,hp],mtcars[,mpg], main="Scatterplot of Horsepower and MPGs",
     xlab="Horsepower",
     ylab="Miles per Gallon")
#let's add a linear regression line to get a quick visual check on the relationship
abline(lm(mpg ~ hp, data=mtcars))

#is there a linear relationship..?
linear_model1 <- lm(mpg ~ hp, data=mtcars)
summary(linear_model1)

#what's the average hp of low mpg cars?
mean(mtcars[high_mpg==0, hp])
#and high?
mean(mtcars[high_mpg==1, hp])
#can we do this in one step instead of two?
mtcars[, mean(hp), by="high_mpg"]
#are these statistically different?
t.test(mtcars[high_mpg==0, hp], mtcars[high_mpg==1, hp])

#let's create a new variable of the average mpg per hp
mtcars[, mpg_per_hp := mpg/hp]
summary(mtcars[,mpg_per_hp])
head(mtcars[,mpg_per_hp])
head(mtcars)
#oof! long numbers... let's round them
mtcars[, mpg_per_hp := round(mpg/hp,3)]

#sometimes you want a smaller data.table. let's restrict mtcars to only 2 variables
mtcars_small <- mtcars[, .(rn, wt)]
#let's square the weight variable
mtcars_small[, wt := wt^2]
#now let's rename it
setnames(mtcars_small,"wt","wtsquared")

#let's merge our data.table with the weight-squared variable into the full mtcars data
mtcars <- merge(mtcars, mtcars_small, by="rn")

#merge function: additional arguments
# merge(mtcars, mtcars_small, by=c("rn","id_var_2","id_var_3"), all=TRUE) # more than 1 identifier variable; outer join
# merge(mtcars, mtcars_small, by="rn", all.x=TRUE) #left join
# merge(mtcars, mtcars_small, by="rn", all.y=TRUE) #right join

#let's make a data.table with 3 variables we want to correlate
mtcars_cor <- mtcars[, .(mpg, cyl, hp)]
#let's rename two of them
setnames(mtcars_cor,c("cyl","hp"),c("cylinders","horsepower"))
#corelation matrix
cor(mtcars_cor)

#write you (full) data to a .csv file
write.csv(mtcars, file="C:/Users/jumorris/mtcars_data.csv")


###SESSION 2: Functions! Aggregation! 

#read in your data from last time
mydata <- fread("C:/Users/jumorris/mtcars_data.csv")
#look at your data
head(mydata)
#there's a row number variable in there! let's get rid of it
mydata[, V1 := NULL]

#let's talk about functions
#we have already used lots of functions
#e.g., "mean"
#use R's mean function to calculate the mean of mpg
mean(mtcars[,mpg])

#let's write our own mean function
julies_mean <- function(x) {
  sum(x)/length(x)
}
#let's construct a vector to test it
julies_vector <- c(1,3,5,7,9)
#view the vector
julies_vector
#calculate the sum of the elements
sum(julies_vector)
#get the number of elements
length(julies_vector)
#calculate the mean "by hand"
sum(julies_vector) / length(julies_vector)
#now, calculate the mean using our function
julies_mean(julies_vector)
#now, calculate the mean using R's mean function
mean(julies_vector)
#use our mean function to calculate the mean of mpg
julies_mean(mtcars[,mpg])

#let's write another function
add_two_things <- function(x,y) {
  x + y
}
#let's test it
add_two_things(1,2)
add_two_things(115,275)

#and another!
add_then_divide <- function(x,y,z) {
  (x + y) / z
}
#let's test it
add_then_divide(5,15,2)

#stand deviation
sd(mtcars[,mpg])
#how is R calculating sd?
#let's look at the function!
sd
#let's try it long-hand
sqrt(var(mtcars[,mpg]))

#aggregation function
#what if we wanted a data.table summarizing other variables, by high/low mpg group?
#first, let's look at our column names
colnames(mtcars)
#we want the mean of all variables from "mpg" to "carb" by high/low mpg group, so let's grab those column names:
colnames(mtcars)[2:12]
#let's get those means by high/low mpg group
mtcars_agg <- mtcars[, lapply(.SD, mean, na.rm=T), 
                     by="high_mpg", 
                     .SDcols=colnames(mtcars)[2:12]]
# #can also write them out:
# mtcars_agg <- mtcars[, lapply(.SD, mean, na.rm=T), 
#                      by="high_mpg", 
#                      .SDcols=c("mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb")]
# #same thing, but using a vector of the column names
# #make of vector of the column names:
# colvector <- colnames(mtcars)[2:12]
# #look at your vector
# colvector
# mtcars_agg <- mtcars[, lapply(.SD, mean, na.rm=T),
#                      by="high_mpg",
#                      .SDcols=colvector]
# #same thing, but specifying a function:
# mtcars_agg <- mtcars[, lapply(.SD, function(x) mean(x, na.rm=T)), 
#                      by="high_mpg", 
#                      .SDcols=colnames(mtcars)[2:12]]
# #same thing, but rounding our results:
# mtcars_agg <- mtcars[, lapply(.SD, function(x) round(mean(x, na.rm=T),3)), 
#                      by="high_mpg", 
#                      .SDcols=colnames(mtcars)[2:12]]
# #same thing, but with more than one grouping variable
# mtcars_agg <- mtcars[, lapply(.SD, mean, na.rm=T), 
#                      by=c("high_mpg","grouping_var_2"), 
#                      .SDcols=colnames(mtcars)[2:12]]
#what if we wanted to build out a specific aggregated data.table (i.e., not just *all* means)?
mtcars_agg2 <- mtcars[, list(carN = .N,
                             avg_mpg = mean(mpg, na.rm=T),
                             max_wt = max(wt, na.rm=T),
                             min_gear = min(gear, na.rm=T),
                             sum_of_gear_column = sum(gear, na.rm=T)),
                      by=c("high_mpg")]

#why do I keep specifying "na.rm = T" (TRUE)?
#let's test it: let's change on value in the gear column to NA
mtcars <- mtcars[rn=="AMC Javelin", gear := NA]
mtcars_agg2 <- mtcars[, list(carN = .N,
                             avg_mpg = mean(mpg),
                             max_wt = max(wt),
                             min_gear = min(gear),
                             sum_of_gear_column = sum(gear)),
                      by=c("high_mpg")]


