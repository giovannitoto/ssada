#### PREVISIONE PRESENZA AGLI EVENTI ####

rm(list = ls())
setwd("E:\\UNIVERSITA\\GUIDOLIN\\Progetto")

source("src/functions.R")
source("dataset_previsione.R")




#### 1. TUTTI GLI EVENTI ####

# Sistemazione dataset
dati <- data[, -c(3,8)]
rm(data)
set.seed(1)
acaso <- sample(1:NROW(dati), NROW(dati)*0.75)
sss <- dati[acaso,]
vvv <- dati[-acaso,]
sss.s <- sss
sss.s[,-c(1,2,5,6,7)] <- scale(sss[,-c(1,2,5,6,7)])
vvv.s <- vvv
vvv.s[,-c(1,2,5,6,7)] <- scale(vvv[,-c(1,2,5,6,7)])
x <- model.matrix(~., data = sss.s[,-1])
x.vvv <- model.matrix(~., data = vvv.s[,-1])


# Brevi analisi esplorative
table(sss$weekend)
plot(table(sss$mese))
hist(sss$membri_gr, nclass = 50)
hist(sss$attivi_gr, nclass = 50)
plot(sort(table(sss$categoria), decreasing = TRUE))
hist(sss$y, nclass = 50)


# Modello lineare:
lm <- lm(y ~ ., data = sss)
p.lm <- pmax(predict(lm, newdata = vvv), 0)
(mae.lm <- mean(abs(p.lm - vvv$y)))


# Lasso
library(glmnet)
set.seed(2)
cb1 <- sample(1:NROW(sss), NROW(sss)*0.67)
cb2 <- setdiff(1:NROW(sss), cb1)
lm_lasso_grid <- glmnet(x = x[cb1,-1], y = sss.s[cb1,1], alpha = 1)
plot(lm_lasso_grid, label = T)
plot(lm_lasso_grid, xvar = "lambda", label = T)
p.cb2_lasso <- predict(lm_lasso_grid, newx = x[cb2,-1])
err_lasso <- apply(abs(sss$y[cb2] - p.cb2_lasso), 2, mean)
plot(lm_lasso_grid$lambda, err_lasso, xlab = expression(lambda), ylab = "errore",
     xlim = c(0,0.15), ylim = c(4.57,4.63))
(lambda.ottimo <- lm_lasso_grid$lambda[err_lasso == min(err_lasso)])
lm_lasso <- glmnet(x = x[,-1], y = sss$y, alpha = 1, lambda = lambda.ottimo)
p.lasso <- pmax(predict(lm_lasso, newx = x.vvv[,-1]), 0)
(mae.lasso <- mean(abs(p.lasso - vvv$y)))


# MARS
library(polspline)
mars <- polymars(sss$y, sss[,-1])
(j.ottima <- mars$fitting$size[mars$fitting$GCV==min(mars$fitting$GCV)])
p.mars <- pmax(predict(mars, vvv[,-1]), 0)
(mae.mars <- mean(abs(p.mars - vvv$y)))


# Albero di regressione
library(tree)
set.seed(3)
cb1 <- sample(1:NROW(sss), NROW(sss)*0.67)
cb2 <- setdiff(1:NROW(sss), cb1)
tree_grow <- tree(y ~ ., data = sss[cb1,],
                  control = tree.control(nobs = length(cb1), minsize = 2, mindev = 0.001))
tree_cut <- prune.tree(tree_grow, newdata=sss[cb2,])
plot(tree_cut)
(j.ottima <- tree_cut$size[tree_cut$dev == min(tree_cut$dev)])
abline(v = j.ottima, col = 2, lty = 2)
tree <- prune.tree(tree_grow, best = j.ottima)
plot(tree)
p.tree <- pmax(predict(tree, newdata=vvv), 0)
(mae.tree <- mean(abs(p.tree - vvv$y)))


# Rete neurale
library(nnet)
set.seed(4)
cb1 <- sample(1:NROW(sss), NROW(sss)*0.67)
cb2 <- setdiff(1:NROW(sss), cb1)
decay <- 10^(seq(-3,-1, length = 4))
nodi <- 1:5
mae.ciclo <- matrix(0, length(decay), length(nodi))
set.seed(5)
for(d in 1:length(decay)){
  for(n in 1:length(nodi)){
    nnc <- nnet(y ~ ., data = sss[cb1,],
                size = nodi[n], decay = decay[d], maxit = 1000, linout = TRUE, 
                MaxNWts = 5000)
    p.nnc <- pmax(predict(nnc, newdata = sss[cb2,]), 0)
    mae.ciclo[d,n] <- mean(abs(p.nnc - sss$y[cb2]))
    cat("d:",d, "- n:", n, " ")
  }
}
mae.ciclo
min(mae.ciclo)
set.seed(6)
nn <- nnet(y~., data = sss, size = nodi[5], decay = decay[3],
           maxit = 2000, linout = TRUE, MaxNWts = 2000)
p.nn <- pmax(predict(nn, newdata = vvv), 0)
(mae.nn <- mean(abs(p.nn - vvv$y)))


# Gradient Boosting
library(gbm)
set.seed(7)
cb1 <- sample(1:NROW(sss), NROW(sss)*0.67)
cb2 <- setdiff(1:NROW(sss), cb1)
shri <- 10^(seq(-3, -1, length = 4))
int <- c(1:4)
mae.ciclo_gb <- matrix(0, length(shri), length(int))
set.seed(444)
for(s in 1:length(shri)){
  for(i in int){
    gbc <- gbm(y ~ ., data = sss[cb1,], distribution = "gaussian", n.trees = 2000, 
               interaction.depth = i, shrinkage = shri[s])
    p.gbc <- pmax(predict(gbc, newdata = sss[cb2,]), 0)
    mae.ciclo_gb[s,i] <- mean(abs(sss$y[cb2] - p.gbc), na.rm = TRUE)
    cat(s, " - ", i)
  }
}
mae.ciclo_gb
min(mae.ciclo_gb)
set.seed(8)
gb <- gbm(y ~ ., data = sss, distribution = "gaussian", n.trees = 5000, 
          interaction.depth = int[4], shrinkage = shri[4])
p.gb <- pmax(predict(gb, newdata = vvv), 0)
(mae.gb <- mean(abs(vvv$y - p.gb), na.rm = TRUE))
summary(gb)


# Random Forest
library(randomForest)
set.seed(9)
cb1 <- sample(1:NROW(sss), NROW(sss)*0.67)
cb2 <- setdiff(1:NROW(sss), cb1)
mtry <- c(1:4)
mae.rfc <- rep(NA, length(mtry))
set.seed(10)
for(i in mtry){
  rfc <- randomForest(x = sss[cb1,-1], y = sss[cb1,1], nodesize = 1, 
                      ntree = 400, mtry = mtry[i])
  p.rfc <- pmax(predict(rfc, newdata = sss[cb2,]), 0)
  mae.rfc[i] <- mean(abs(sss$y[cb2] - p.rfc), na.rm = TRUE)
  cat(i," ")
}
mae.rfc
min(mae.rfc)
set.seed(11)
rf <- randomForest(y ~ ., data = sss, nodesize = 1, ntree = 500, mtry = mtry[3], 
                   importance = TRUE)
p.rf <- pmax(predict(rf, newdata = vvv), 0)
(mae.rf <- mean(abs(vvv$y - p.rf), na.rm = TRUE))


# Tabella riassuntiva:
resume <- matrix(NA, nrow = 7, ncol = 2)
resume <- as.data.frame(resume)
colnames(resume) <- c("Modello", "MAE")
resume$Modello <- c("modello lineare", "lm lasso", "MARS", "albero di regressione", 
                    "rete neurale", "gradient boosting", "random forest")
resume$MAE <- c(mae.lm, mae.lasso, mae.mars, mae.tree, mae.nn, mae.gb, mae.rf)
resume





#### 2. EVENTI DELLA CATEGORIA "TECH" ####

# Sistemazione dataset
dati1 <- dati[dati$categoria == "Tech",]
dati1 <- dati1[,-2]
set.seed(12)
acaso <- sample(1:NROW(dati1), NROW(dati1)*0.75)
sss1 <- dati1[acaso,]
vvv1 <- dati1[-acaso,]
sss.s1 <- sss1
sss.s1[,-c(1,4,5,6)] <- scale(sss1[,-c(1,4,5,6)])
vvv.s1 <- vvv1
vvv.s1[,-c(1,4,5,6)] <- scale(vvv1[,-c(1,4,5,6)])
x1 <- model.matrix(~., data = sss.s1[,-1])
x.vvv1 <- model.matrix(~., data = vvv.s1[,-1])


# Brevi analisi esplorative
table(sss1$weekend)
plot(table(sss1$mese))
hist(sss1$membri_gr, nclass = 50)
hist(sss1$attivi_gr, nclass = 50)
hist(sss1$y, nclass = 50)


# Modello lineare:
lm1 <- lm(y ~ ., data = sss1)
p.lm1 <- pmax(predict(lm1, newdata = vvv1), 0)
(mae.lm1 <- mean(abs(p.lm1 - vvv1$y)))


# Lasso
library(glmnet)
set.seed(13)
cb1 <- sample(1:NROW(sss1), NROW(sss1)*0.67)
cb2 <- setdiff(1:NROW(sss1), cb1)
lm_lasso_grid1 <- glmnet(x = x1[cb1,-1], y = sss.s1[cb1,1], alpha = 1)
plot(lm_lasso_grid1, label = T)
plot(lm_lasso_grid1, xvar = "lambda", label = T)
p.cb2_lasso1 <- predict(lm_lasso_grid1, newx = x1[cb2,-1])
err_lasso1 <- apply(abs(sss1$y[cb2] - p.cb2_lasso1), 2, mean)
plot(lm_lasso_grid1$lambda, err_lasso1, xlab = expression(lambda), ylab = "errore",
     xlim = c(0,0.1), ylim = c(9.14,9.17))
(lambda.ottimo1 <- lm_lasso_grid1$lambda[err_lasso1 == min(err_lasso1)])
lm_lasso1 <- glmnet(x = x1[,-1], y = sss1$y, alpha = 1, lambda = lambda.ottimo1)
p.lasso1 <- pmax(predict(lm_lasso1, newx = x.vvv1[,-1]), 0)
(mae.lasso1 <- mean(abs(p.lasso1 - vvv1$y)))


# MARS
library(polspline)
mars1 <- polymars(sss1$y, sss1[,-1])
(j.ottima1 <- mars1$fitting$size[mars1$fitting$GCV==min(mars1$fitting$GCV)])
p.mars1 <- pmax(predict(mars1, vvv1[,-1]), 0)
(mae.mars1 <- mean(abs(p.mars1 - vvv1$y)))


# Albero di regressione
library(tree)
set.seed(14)
cb1 <- sample(1:NROW(sss1), NROW(sss1)*0.67)
cb2 <- setdiff(1:NROW(sss1), cb1)
tree_grow1 <- tree(y ~ ., data = sss1[cb1,],
                  control = tree.control(nobs = length(cb1), minsize = 2, mindev = 0.001))
tree_cut1 <- prune.tree(tree_grow1, newdata=sss1[cb2,])
plot(tree_cut1)
(j.ottima1 <- tree_cut1$size[tree_cut1$dev == min(tree_cut1$dev)])
abline(v = j.ottima1, col = 2, lty = 2)
tree1 <- prune.tree(tree_grow1, best = j.ottima1)
plot(tree1)
p.tree1 <- pmax(predict(tree1, newdata=vvv1), 0)
(mae.tree1 <- mean(abs(p.tree1 - vvv1$y)))


# Rete neurale
library(nnet)
set.seed(15)
cb1 <- sample(1:NROW(sss1), NROW(sss1)*0.67)
cb2 <- setdiff(1:NROW(sss1), cb1)
decay1 <- 10^(seq(-3,-1, length = 10))
nodi1 <- 1:10
mae.ciclo1 <- matrix(0, length(decay1), length(nodi1))
set.seed(16)
for(d in 1:length(decay1)){
  for(n in 1:length(nodi1)){
    nnc <- nnet(y ~ ., data = sss1[cb1,],
                size = nodi1[n], decay = decay1[d], maxit = 1000, linout = TRUE, 
                MaxNWts = 5000)
    p.nnc <- pmax(predict(nnc, newdata = sss1[cb2,]), 0)
    mae.ciclo1[d,n] <- mean(abs(p.nnc - sss1$y[cb2]))
    cat("d:",d, "- n:", n, " ")
  }
}
mae.ciclo1
min(mae.ciclo1)
set.seed(17)
nn1 <- nnet(y ~ ., data = sss1, size = nodi1[8], decay = decay1[7],
           maxit = 2000, linout = TRUE, MaxNWts = 2000)
p.nn1 <- pmax(predict(nn1, newdata = vvv1), 0)
(mae.nn1 <- mean(abs(p.nn1 - vvv1$y)))


# Gradient Boosting
library(gbm)
set.seed(18)
cb1 <- sample(1:NROW(sss1), NROW(sss1)*0.67)
cb2 <- setdiff(1:NROW(sss1), cb1)
shri1 <- 10^(seq(-3, -1, length = 6))
int1 <- c(1:5)
mae.ciclo_gb1 <- matrix(0, length(shri1), length(int1))
set.seed(19)
for(s in 1:length(shri1)){
  for(i in int1){
    gbc <- gbm(y ~ ., data = sss1[cb1,], distribution = "gaussian", n.trees = 2000, 
               interaction.depth = i, shrinkage = shri1[s])
    p.gbc <- pmax(predict(gbc, newdata = sss1[cb2,]), 0)
    mae.ciclo_gb1[s,i] <- mean(abs(sss1$y[cb2] - p.gbc), na.rm = TRUE)
    cat(s, " - ", i)
  }
}
mae.ciclo_gb1
min(mae.ciclo_gb1)
set.seed(20)
gb1 <- gbm(y ~ ., data = sss1, distribution = "gaussian", n.trees = 5000, 
          interaction.depth = int1[4], shrinkage = shri1[3])
p.gb1 <- pmax(predict(gb1, newdata = vvv1), 0)
(mae.gb1 <- mean(abs(vvv1$y - p.gb1), na.rm = TRUE))
summary(gb1)


# Random Forest
library(randomForest)
set.seed(21)
cb1 <- sample(1:NROW(sss1), NROW(sss1)*0.67)
cb2 <- setdiff(1:NROW(sss1), cb1)
mtry1 <- c(1:4)
mae.rfc1 <- rep(NA, length(mtry1))
set.seed(22)
for(i in mtry1){
  rfc <- randomForest(x = sss1[cb1,-1], y = sss1[cb1,1], nodesize = 1, 
                      ntree = 400, mtry = mtry1[i])
  p.rfc <- pmax(predict(rfc, newdata = sss1[cb2,]), 0)
  mae.rfc1[i] <- mean(abs(sss1$y[cb2] - p.rfc), na.rm = TRUE)
  cat(i," ")
}
mae.rfc1
min(mae.rfc1)
set.seed(23)
rf1 <- randomForest(y ~ ., data = sss1, nodesize = 1, ntree = 500, mtry = mtry1[3], 
                   importance = TRUE)
p.rf1 <- pmax(predict(rf1, newdata = vvv1), 0)
(mae.rf1 <- mean(abs(vvv1$y - p.rf1), na.rm = TRUE))


# Tabella riassuntiva:
resume1 <- matrix(NA, nrow = 7, ncol = 2)
resume1 <- as.data.frame(resume1)
colnames(resume1) <- c("Modello", "MAE")
resume1$Modello <- c("modello lineare", "lm lasso", "MARS", "albero di regressione", 
                    "rete neurale", "gradient boosting", "random forest")
resume1$MAE <- c(mae.lm1, mae.lasso1, mae.mars1, mae.tree1, mae.nn1, mae.gb1, mae.rf1)
resume1




#### 3. EVENTI DEL CLUSTER DUE ####

# Sistemazione dataset
dati2 <- dati[dati$categoria %in% c("Career & Business", "Socializing", "Outdoors & Adventure"),]
set.seed(24)
acaso <- sample(1:NROW(dati2), NROW(dati2)*0.75)
sss2 <- dati2[acaso,]
vvv2 <- dati2[-acaso,]
sss.s2 <- sss2
sss.s2[,-c(1,2,5,6,7)] <- scale(sss2[,-c(1,2,5,6,7)])
vvv.s2 <- vvv2
vvv.s2[,-c(1,2,5,6,7)] <- scale(vvv2[,-c(1,2,5,6,7)])
x2 <- model.matrix(~., data = sss.s2[,-1])
x.vvv2 <- model.matrix(~., data = vvv.s2[,-1])


# Brevi analisi esplorative
table(sss2$weekend)
plot(table(sss2$mese))
hist(sss2$membri_gr, nclass = 50)
hist(sss2$attivi_gr, nclass = 50)
plot(sort(table(sss2$categoria), decreasing = TRUE))
hist(sss2$y, nclass = 50)


# Modello lineare:
lm2 <- lm(y ~ ., data = sss2)
p.lm2 <- pmax(predict(lm2, newdata = vvv2), 0)
(mae.lm2 <- mean(abs(p.lm2 - vvv2$y)))


# Lasso
library(glmnet)
set.seed(25)
cb1 <- sample(1:NROW(sss2), NROW(sss2)*0.67)
cb2 <- setdiff(1:NROW(sss2), cb1)
lm_lasso_grid2 <- glmnet(x = x2[cb1,-1], y = sss.s2[cb1,1], alpha = 1)
plot(lm_lasso_grid2, label = T)
plot(lm_lasso_grid2, xvar = "lambda", label = T)
p.cb2_lasso2 <- predict(lm_lasso_grid2, newx = x2[cb2,-1])
err_lasso2 <- apply(abs(sss2$y[cb2] - p.cb2_lasso2), 2, mean)
plot(lm_lasso_grid2$lambda, err_lasso2, xlab = expression(lambda), ylab = "errore",
     xlim = c(0.05,0.3), ylim = c(4.54,4.60))
(lambda.ottimo2 <- lm_lasso_grid2$lambda[err_lasso2 == min(err_lasso2)])
lm_lasso2 <- glmnet(x = x2[,-1], y = sss2$y, alpha = 1, lambda = lambda.ottimo2)
p.lasso2 <- pmax(predict(lm_lasso2, newx = x.vvv2[,-1]), 0)
(mae.lasso2 <- mean(abs(p.lasso2 - vvv2$y)))


# MARS
library(polspline)
mars2 <- polymars(sss2$y, sss2[,-1])
(j.ottima2 <- mars2$fitting$size[mars2$fitting$GCV==min(mars2$fitting$GCV)])
p.mars2 <- pmax(predict(mars2, vvv2[,-1]), 0)
(mae.mars2 <- mean(abs(p.mars2 - vvv2$y)))


# Albero di regressione
library(tree)
set.seed(26)
cb1 <- sample(1:NROW(sss2), NROW(sss2)*0.67)
cb2 <- setdiff(1:NROW(sss2), cb1)
tree_grow2 <- tree(y ~ ., data = sss2[cb1,],
                  control = tree.control(nobs = length(cb1), minsize = 2, mindev = 0.001))
tree_cut2 <- prune.tree(tree_grow2, newdata=sss2[cb2,])
plot(tree_cut2)
(j.ottima2 <- tree_cut2$size[tree_cut2$dev == min(tree_cut2$dev)])
j.ottima2 <- 47
abline(v = j.ottima2, col = 2, lty = 2)
tree2 <- prune.tree(tree_grow2, best = j.ottima2)
plot(tree2)
p.tree2 <- pmax(predict(tree2, newdata=vvv2), 0)
(mae.tree2 <- mean(abs(p.tree2 - vvv2$y)))


# Rete neurale
library(nnet)
set.seed(27)
cb1 <- sample(1:NROW(sss2), NROW(sss2)*0.67)
cb2 <- setdiff(1:NROW(sss2), cb1)
decay2 <- 10^(seq(-3,-1, length = 10))
nodi2 <- 1:8
mae.ciclo2 <- matrix(0, length(decay2), length(nodi2))
set.seed(28)
for(d in 1:length(decay2)){
  for(n in 1:length(nodi2)){
    nnc <- nnet(y ~ ., data = sss2[cb1,],
                size = nodi2[n], decay = decay2[d], maxit = 1000, linout = TRUE, 
                MaxNWts = 5000)
    p.nnc <- pmax(predict(nnc, newdata = sss2[cb2,]), 0)
    mae.ciclo2[d,n] <- mean(abs(p.nnc - sss2$y[cb2]))
    cat("d:",d, "- n:", n, " ")
  }
}
mae.ciclo2
min(mae.ciclo2)
set.seed(29)
nn2 <- nnet(y ~ ., data = sss2, size = nodi2[7], decay = decay2[9],
           maxit = 2000, linout = TRUE, MaxNWts = 2000)
p.nn2 <- pmax(predict(nn2, newdata = vvv2), 0)
(mae.nn2 <- mean(abs(p.nn2 - vvv2$y)))


# Gradient Boosting
library(gbm)
set.seed(30)
cb1 <- sample(1:NROW(sss2), NROW(sss2)*0.67)
cb2 <- setdiff(1:NROW(sss2), cb1)
shri2 <- 10^(seq(-3, -1, length = 6))
int2 <- c(1:5)
mae.ciclo_gb2 <- matrix(0, length(shri2), length(int2))
set.seed(31)
for(s in 1:length(shri2)){
  for(i in int2){
    gbc <- gbm(y ~ ., data = sss2[cb1,], distribution = "gaussian", n.trees = 2000, 
               interaction.depth = i, shrinkage = shri2[s])
    p.gbc <- pmax(predict(gbc, newdata = sss2[cb2,]), 0)
    mae.ciclo_gb2[s,i] <- mean(abs(sss2$y[cb2] - p.gbc), na.rm = TRUE)
    cat(s, " - ", i)
  }
}
mae.ciclo_gb2
min(mae.ciclo_gb2)
set.seed(32)
gb2 <- gbm(y ~ ., data = sss2, distribution = "gaussian", n.trees = 5000, 
          interaction.depth = int2[4], shrinkage = shri2[6])
p.gb2 <- pmax(predict(gb2, newdata = vvv2), 0)
(mae.gb2 <- mean(abs(vvv2$y - p.gb2), na.rm = TRUE))
summary(gb2)


# Random Forest
library(randomForest)
set.seed(33)
cb1 <- sample(1:NROW(sss2), NROW(sss2)*0.67)
cb2 <- setdiff(1:NROW(sss2), cb1)
mtry2 <- c(1:4)
mae.rfc2 <- rep(NA, length(mtry2))
set.seed(34)
for(i in mtry2){
  rfc <- randomForest(x = sss2[cb1,-1], y = sss2[cb1,1], nodesize = 1, 
                      ntree = 400, mtry = mtry2[i])
  p.rfc <- pmax(predict(rfc, newdata = sss2[cb2,]), 0)
  mae.rfc2[i] <- mean(abs(sss2$y[cb2] - p.rfc), na.rm = TRUE)
  cat(i," ")
}
mae.rfc2
min(mae.rfc2)
set.seed(35)
rf2 <- randomForest(y ~ ., data = sss2, nodesize = 1, ntree = 500, mtry = mtry2[3], 
                   importance = TRUE)
p.rf2 <- pmax(predict(rf2, newdata = vvv2), 0)
(mae.rf2 <- mean(abs(vvv2$y - p.rf2), na.rm = TRUE))


# Tabella riassuntiva:
resume2 <- matrix(NA, nrow = 7, ncol = 2)
resume2 <- as.data.frame(resume2)
colnames(resume2) <- c("Modello", "MAE")
resume2$Modello <- c("modello lineare", "lm lasso", "MARS", "albero di regressione", 
                    "rete neurale", "gradient boosting", "random forest")
resume2$MAE <- c(mae.lm2, mae.lasso2, mae.mars2, mae.tree2, mae.nn2, mae.gb2, mae.rf2)
resume2


#### RESUME ####
resume
resume1
resume2
