
rm(list = ls())
setwd("E:\\UNIVERSITA\\GUIDOLIN\\Progetto")

library(sqldf)
library(arules)

source("src\\dataset.R")




#### 1. GLOBALE (category_name) ####

mg1 <- data[,c(3,9,7)]
mg1$member_id <- as.factor(mg1$member_id)
mg1[,2] <- as.factor(mg1[,2])
mg1 <- na.omit(mg1)
nlevels(mg1$member_id) 
nlevels(mg1[,2])

arul1 <- split(x = mg1[,"category_name"], f = mg1$member_id)
arul1 <- lapply(arul1, unique)
arul1 <- as(arul1, "transactions") 
regole1 <- apriori(arul1, parameter = list(support = 0.005, confidence = 0.1))
out1 <- inspect(sort(regole1, by = "confidence"))
out1 <- out1[-c(4,17,23:35),-c(6,8)]
out1 <- out1[1:15,]
out1[,c(4,5,6)] <- round(out1[,c(4,5,6)],4)

n1 <- nrow(out1)
p1 <- ncol(out1)
lat1 <- c()
for (i in 1:n1) {
  lat1 <- c(lat1, as.character(out1[i,1]))
  for (j in 2:p1) {
    lat1 <- c(lat1, "&", as.character(out1[i,j]))
  }
  lat1 <- c(lat1, "\\\\\n")
}
lat1 <- paste(lat1, collapse="")
cat(lat1)





#### 2. CLUSTER TECH (group_name - SEI GRUPPI MAGGIORI) ####

mg2 <- data[,c(3,6,9)]
var <- c("NashJS", "The Iron Yard - Nashville", "Nashville .NET User Group",
         "PyNash", "Nashville UX", "Code for Nashville")
mg2 <- mg2[mg2$category_name=="Tech",]
mg2 <- mg2[mg2$group_name %in% var,]
mg2$member_id <- as.factor(mg2$member_id)
mg2$group_name <- as.factor(mg2$group_name)
mg2 <- na.omit(mg2)
nlevels(mg2$member_id) 
nlevels(mg2[,2]) 

arul2 <- split(x = mg2[,"group_name"], f = mg2$member_id)
arul2 <- lapply(arul2, unique)
arul2 <- as(arul2, "transactions") 
regole2 <- apriori(arul2, parameter = list(support = 0.01, confidence = 0.4))
out2 <- inspect(sort(regole2, by = "support"))
out2 <- out2[,-c(6,8)]
out2 <- out2[1:15,]
out2[,c(4,5,6)] <- round(out2[,c(4,5,6)],4)

n2 <- nrow(out2)
p2 <- ncol(out2)
lat2 <- c()
for (i in 1:n2) {
  lat2 <- c(lat2, as.character(out2[i,1]))
  for (j in 2:p2) {
    lat2 <- c(lat2, "&", as.character(out2[i,j]))
  }
  lat2 <- c(lat2, "\\\\\n")
}
lat2 <- paste(lat2, collapse="")
cat(lat2)




#### 3. CLUSTER DUE (category_name per insiemistica) ####

name <- c("s + b + o", "s + b", "s + o", "b + o", "s", "b", "o")
ass <- c(152, 444, 466, 160, 2687, 3588, 3073)
s <- sum(ass)
freq <- ass/s
freq
a <- as.data.frame(cbind(name,round(as.numeric(freq),3)))

















