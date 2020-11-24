#### MODELLI ####

rm(list = ls())
setwd("C:\\Users\\agnol\\Desktop\\UNIVERSITA\\GUIDOLIN\\Progetto")

library(sqldf)  
library(igraph)
library(amen)

source("src/functions.R")
source("src/dataset.R")
source("modelli_variabili_rete.R")


# Creazione sociomatrice in base alla soglia
eventi_data <- filtro_e(data, 65)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)
y <- as.matrix(eventi_sociomatrice)
diag(y) <- rep(NA, NCOL(y))

# Variabili per modello ame
var.ame <- variabili_ame(eventi_sociomatrice)

# Variabili per altri modelli
var.mod <- variabili_modelli(sociomatrice = eventi_sociomatrice,
                             same_group = var.ame$same_group,
                             same_cat = var.ame$same_cat,
                             Xn = var.ame$Xn)
df <- as.data.frame(cbind(var.mod$y_v,var.mod$dframe))
colnames(df) <- c("y", colnames(var.mod$dframe))
df <- na.omit(df)




# SRRM con R = 0
srrm0 <- ame(y, Xdyad = var.ame$Xd, Xrow = var.ame$Xm[,-1], Xcol = var.ame$Xm[,-1],
            symmetric = TRUE, nvar = TRUE)
pred.srrm0 <- pmax(srrm0$YPM, 0)
(mse.srrm0 <- mean((y - pred.srrm0)^2, na.rm = TRUE))



# SRRM con R = 1
srrm <- ame(y, Xdyad = var.ame$Xd, Xrow = var.ame$Xm[,-1], Xcol = var.ame$Xm[,-1],
            symmetric = TRUE, nvar = TRUE, R = 1)
pred.srrm <- pmax(srrm$YPM, 0)
(mse.srrm <- mean((y - pred.srrm)^2, na.rm = TRUE))



# Modello lineare
lm <- lm(var.mod$y_v ~ ., data = var.mod$dframe)
pred.v.lm <- c(predict(lm))
names(pred.v.lm) <- NULL
pred.lm <- matrice_previsioni(pred.v.lm, NCOL(y))
pred.lm <- pmax(pred.lm, 0)
rm(pred.v.lm)
(mse.lm <- mean((y - pred.lm)^2, na.rm = TRUE))



# MARS
library(polspline)
mars <- polymars(df$y, df[,-1])
(j.ottima <- mars$fitting$size[mars$fitting$GCV==min(mars$fitting$GCV)])
plot(mars$fitting$size[mars$fitting$"0/1"==1], mars$fitting$GCV[mars$fitting$"0/1"==1],
     xlab="num funzioni base", ylab="gcv")
points(mars$fitting$size[mars$fitting$"0/1"==0], mars$fitting$GCV[mars$fitting$"0/1"==0],
       col=4)

pred.v.mars <- c(predict(mars, df[,-1]))
names(pred.v.mars) <- NULL
pred.mars <- matrice_previsioni(pred.v.mars, NCOL(y))
pred.mars <- pmax(pred.mars, 0)
rm(pred.v.mars)
(mse.mars <- mean((y - pred.mars)^2, na.rm = TRUE))



# Rete neurale
library(nnet)
decay <- 10^(seq(-3,-1, length=10))
nodi <- 1:15
mse.ciclo <- matrix(0, length(decay), length(nodi))
set.seed(444)
for(d in 1:length(decay)){
  for(n in 1:15){
    nnc <- nnet(y~., data = df, size = nodi[n], decay = decay[d], 
                maxit = 500, linout = TRUE, MaxNWts = 5000)
    pred.v.nn <- c(predict(nnc, df[,-1]))
    names(pred.v.nn) <- NULL
    pred.nn <- matrice_previsioni(pred.v.nn, NCOL(y))
    pred.nn <- pmax(pred.nn, 0)
    mse.ciclo[d,n] <- mean((y - pred.nn)^2, na.rm = TRUE)
    print(d,n)
  }
}
mse.ciclo
min(mse.ciclo)

set.sedd(456)
nn <- nnet(y~., data = df, size = 15, decay = decay[8],
           maxit = 2000, linout = TRUE, MaxNWts = 2000)

pred.v.nn <- c(predict(nn, df[,-1]))
names(pred.v.nn) <- NULL
pred.nn <- matrice_previsioni(pred.v.nn, NCOL(y))
pred.nn <- pmax(pred.nn, 0)
(mse.nn <- mean((y - pred.nn)^2, na.rm = TRUE))



# Random Forest
library(randomForest)
err.rfc <- c(4,4)
set.seed(555)
for(i in c(1:7)){
  rfc <- randomForest(x = df[,-1], y = df[,1], nodesize = 1, 
                      ntree = 500, mtry = i)
  err.rfc <- rbind(err.rfc, c(i, rfc$mse[500]))
  cat(i,"")
}
err.rfc <- err.rfc[-1,]
err.rfc

set.seed(667)
rf <- randomForest(y~., data=df, nodesize=1, ntree=500, mtry=3, importance=TRUE)

pred.v.rf <- c(predict(rf, df[,-1]))
names(pred.v.rf) <- NULL
pred.rf <- matrice_previsioni(pred.v.rf, NCOL(y))
pred.rf <- pmax(pred.rf, 0)
(mse.rf <- mean((y - pred.rf)^2, na.rm = TRUE))



# Gradient Boosting
library(gbm)

shri <- 10^(seq(-3,-1, length=10))
int <- 1:7
mse.ciclo <- matrix(0, length(shri), length(int))
set.seed(444)
for(s in 1:length(shri)){
  for(i in 1:7){
    gb <- gbm(y ~ ., data = df, distribution = "gaussian", n.trees = 5000, 
              interaction.depth = i, shrinkage = shri[s])
    pred.v.gb <- c(predict(gb, df[,-1]))
    names(pred.v.gb) <- NULL
    pred.gb <- matrice_previsioni(pred.v.gb, NCOL(y))
    pred.gb <- pmax(pred.gb, 0)
    mse.ciclo[s,i] <- mean((y - pred.gb)^2, na.rm = TRUE)
    print(s,i)
  }
}
mse.ciclo
min(mse.ciclo)

set.seed(679)
gb <- gbm(y ~ ., data = df, distribution = "gaussian", n.trees = 5000, 
          interaction.depth = 7, shrinkage = shri[10])

pred.v.gb <- c(predict(gb, df[,-1]))
names(pred.v.gb) <- NULL
pred.gb <- matrice_previsioni(pred.v.gb, NCOL(y))
pred.gb <- pmax(pred.gb, 0)
(mse.gb <- mean((y - pred.gb)^2, na.rm = TRUE))




# Visualizzare affiancati gli rmse
(cbind(mse.srrm0, mse.srrm, mse.lm, mse.mars, mse.nn, mse.rf, mse.gb))


# Visualizzare affiancate le previsioni delle prime oss per colonna
y.oss <- c(y)
p.srrm0 <- round(c(pred.srrm0))
p.srrm <- round(c(pred.srrm))
p.lm <- round(c(pred.lm))
p.mars <- round(c(pred.mars))
p.nn <- round(c(pred.nn))
p.rf <- round(c(pred.rf))
p.gb <- round(c(pred.gb))
a <- as.data.frame(cbind(y.oss, p.srrm0, p.srrm, p.lm, p.mars, p.nn,
                         p.rf, p.gb))
a[720:740,]



















