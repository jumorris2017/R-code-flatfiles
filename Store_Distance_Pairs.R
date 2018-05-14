##Calculating Store Distance to other Stores
##Findings pairs of close stores
##For Lisa/Megan request 1/9/18

#load libraries
library(data.table)
library(geosphere)
library(stringr)
library(splitstackshape)

#load data
dd <- fread("O:/CoOp/CoOp194_PROReportng&OM/Megan/Distances/ComplexLatLong.csv")
setcolorder(dd,c("STORE_NUM","LONGITUDE","LATITUDE"))
#remove stores 101 duplicates
dd <- dd[!duplicated(dd),]

#create distance matrix
dm <- distm(dd[,2:3])
dm <- as.data.table(dm)

#add stores column
stores <- dd[,STORE_NUM]

#cbind
ddm <- cbind(stores,dm)

#colnames
cnames <- c("stores",paste0("st",dd[,STORE_NUM]))

#setnames
setnames(ddm,cnames)

#write distance matrix
#write.csv(ddm,file="O:/CoOp/CoOp194_PROReportng&OM/Megan/Distances/distancematrix.csv")
#ddm <- fread("O:/CoOp/CoOp194_PROReportng&OM/Megan/Distances/distancematrix.csv")
ddm[, "V1" := NULL]; ddm[, "stores" := NULL]
ddm <- ddm[, lapply(.SD, function(x) round(x,0))]
stores <- colnames(ddm)

#delete anything larger than 100,000
ddm <- ddm[, lapply(.SD, function(x) ifelse(x>=100000,NA,x))]
#add padding of 0's for correct sorting
ddm <- ddm[, lapply(.SD, function(x) str_pad(x,5,pad="0"))]

#paste store number to distance
temp <- as.data.table(matrix(nrow=9393,ncol=9393))
for (i in 1:9393) {
  col <- colnames(ddm)[i]
  vec <- ddm[[col]]
  temp[, i] <- paste(vec,stores,sep="-")
}

###"ddm" is a matrix of the distances###
#create a ranking the distances, and deleting them if they're over rank 26
sortmat <- as.data.table(matrix(nrow=9393,ncol=9393))
for (i in 1:9393){
  col <- colnames(temp)[i]
  vec <- temp[[col]]
  vec <- sort(vec)
  vec[27:length(vec)] <- NA
  sortmat[,i] <- vec
}

##keep rows 1-26
sortmat2 <- sortmat[1:26,] 
#transpose data.table
sortmat2 <- t(sortmat2)
sortmat2 <- as.data.table(sortmat2)

#create column names
colnames2 <- as.vector(c("store_num",paste0("st",str_pad(c(1:25),2,pad="0"))))
#setnames
setnames(sortmat2,colnames2)

#split distance from store numbers
sortmat3 <- as.data.table(matrix(nrow=9393,ncol=26))
for (i in seq_along(colnames(sortmat2))) {
  sortmat3[,i] <- cSplit(sortmat2, i, "-")[,26]
}
colnames3 <- as.vector(c("store_dist",paste0("st",str_pad(c(1:25),2,pad="0"),"dist")))
#setnames
setnames(sortmat3,colnames3)
#delete store distance to self
sortmat3[, store_dist := NULL]

sortmat4 <- as.data.table(matrix(nrow=9393,ncol=26))
for (i in seq_along(colnames(sortmat2))) {
  sortmat4[,i] <- cSplit(sortmat2, i, "-")[,27]
}
colnames4 <- as.vector(c("store_num",paste0("st",str_pad(c(1:25),2,pad="0"),"num")))
#setnames
setnames(sortmat4,colnames4)

#cbind stores to distances
final <- cbind(sortmat4,sortmat3)

#order columns
colordervec <- c("store_num",final[, sort(names(final)[2:51])])
setcolorder(final,colordervec)

#write final product
write.csv(final,file="O:/CoOp/CoOp194_PROReportng&OM/Megan/Distances/listof25closeststores.csv")

















# #create a matrix with the distance-store cells ranked from closest to furthest away, keeping ony top 26
# sortmat <- as.data.table(matrix(nrow=9393,ncol=9393))
# for (i in 6000:9393){
#   col <- colnames(temp)[i]
#   vec <- temp[[col]]
#   vec <- sort(vec)
#   vec[27:length(vec)] <- NA
#   sortmat[,i] <- vec
# }
# #get rid of all rankings greater than 26 (25 + 1 for self)
# rankmat <- rankmat[, lapply(.SD, function(x) ifelse(x>26,NA,x))]

# ##sort columns separately
# temp <- apply(temp,1,sort)
# 
# #give top25 storename column headers
# top25 <- as.data.table(top25)
# #cbind
# top25 <- cbind(stores,top25)
# #setnames
# setnames(top25,cnames)
# 
###"top25" is a ranking of the 25 closest stores###

# #pull in the store number for top 25 stores
# top25stores <- top25[, lapply(.SD, function(x) ifelse(x>1&x<=26,top25[,stores],NA)), .SDcols=colnames(top25)[2:ncol(top25)]]

# ###"top25stores" is a matrix of the store numbers of the 25 closest stores###
# 
# # ##create a for loop that deletes all the distances for stores ranking >26
# # top25distances <- as.data.table(matrix(nrow=9393,ncol=9393))
# # #loop through cells. if within top 26 ranking, return the distance
# # #need the j+1 because the other data.tables have the "stores" column
# # for (i in 1:nrow(top25distances)) {
# #   for (j in 1:ncol(top25distances)) {
# #     if (!is.na(top25[i,j+1])) {
# #       top25distances[i,j] <- ddm[i,j+1]
# #     } else top25distances[i,j] <- NA
# #   }
# # }
# 
# ###"top25distances" is a matrix of the distances of the 25 closest stores###
# 
# ##cbind the matrices together (without store number columns)
# top25nostorenum <- top25[, stores := NULL]; ddmnostorenum <- ddm[, stores := NULL]
# top25bind <- cbind(top25nostorenum,top25stores,ddmnostorenum)
# top25bind <- top25bind[, lapply(.SD, function(x) as.character(x))]
# 
# ##paste together the distance and store number in one cell
# finalddm <- as.data.table(matrix(nrow=9393,ncol=9393))
# finalddm <- finalddm[, lapply(.SD, function(x) as.character(x))]
# for (i in 1:9393) {
#   j <- i+9393
#   k <- i+9393+9393
#   finalddm[, i] <- paste(top25bind[,i,with=FALSE],top25bind[,j,with=FALSE],top25bind[,k,with=FALSE],sep="-")
# }
# 
# ##sort columns separately
# test <- apply(finalddm,1,sort)
# 
# ##keep only rows 2-26
# test2 <- test[2:26,] 
# 
# ##parse out the distance from the store number
# 
# ##transpose the matrix so it's long format (cols=store number, distance 1, store 1, distance n, store n)
# 
# 
# 
# 
# 
# 
