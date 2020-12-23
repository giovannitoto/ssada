#### MODELLI AME ####

rm(list = ls())
#setwd("E:\\UNIVERSITA\\GUIDOLIN\\Progetto")

library(sqldf)  
library(igraph)
library(amen)

source("src/functions.R")
source("src/modelli_variabili_rete.R")



#### GLOBALE ####

# Creazione sociomatrice in base alla soglia
source("src/dataset.R")
eventi_data <- filtro_e(data, 50)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)
y <- as.matrix(eventi_sociomatrice)
diag(y) <- rep(NA, NCOL(y))

# Variabili per modello ame
var.ame <- variabili_ame(eventi_sociomatrice, eventi_data)

srrm0 <- ame(y, Xdyad = var.ame$Xd, Xrow = var.ame$Xm[,-1], Xcol = var.ame$Xm[,-1],
            symmetric = TRUE, nvar = TRUE, R = 0, burn = 1000, nscan = 3000,
            intercept = TRUE)
summary(srrm0)
p.srrm0 <- round(pmax(srrm0$YPM, 0),0)
(mae.srrm0<- mean(abs(y - p.srrm0), na.rm = TRUE))

srrm <- ame(y, Xdyad = var.ame$Xd, Xrow = var.ame$Xm[,-1], Xcol = var.ame$Xm[,-1],
            symmetric = TRUE, nvar = TRUE, R = 1, burn = 1000, nscan = 3000,
            intercept = TRUE)
summary(srrm)
p.srrm <- round(pmax(srrm$YPM, 0),0)
(mae.srrm <- mean(abs(y - p.srrm), na.rm = TRUE))



#### CLUSTER TECH ####

# Creazione sociomatrice in base alla soglia
source("src/dataset.R")
data <- data[data$category_name == "Tech",]
eventi_data1 <- filtro_e(data, 40)$data
eventi_sociomatrice1 <- sociomatrice_eventi(eventi_data1)
y1 <- as.matrix(eventi_sociomatrice1)
diag(y1) <- rep(NA, NCOL(y1))

# Variabili per modello ame
var.ame1 <- variabili_ame_tech(eventi_sociomatrice1, eventi_data1)

srrm.10 <- ame(y1, Xdyad = var.ame1$Xd, Xrow = var.ame1$Xm[,-1], Xcol = var.ame1$Xm[,-1],
              symmetric = TRUE, nvar = TRUE, R = 0, burn = 1000, nscan = 3000,
              intercept = TRUE)
summary(srrm.10)
p.srrm.10 <- round(pmax(srrm.10$YPM, 0),0)
(mae.srrm.10 <- mean(abs(y1 - p.srrm.20), na.rm = TRUE))

srrm.1 <- ame(y1, Xdyad = var.ame1$Xd, Xrow = var.ame1$Xm[,-1], Xcol = var.ame1$Xm[,-1],
            symmetric = TRUE, nvar = TRUE, R = 1, burn = 1000, nscan = 3000,
            intercept = TRUE)
summary(srrm.1)
plot(srrm.1)
p.srrm.1 <- round(pmax(srrm.1$YPM, 0),0)
(mae.srrm.1 <- mean(abs(y1 - p.srrm.1), na.rm = TRUE))

srrm.12 <- ame(y1, Xdyad = var.ame1$Xd, Xrow = var.ame1$Xm[,-1], Xcol = var.ame1$Xm[,-1],
              symmetric = TRUE, nvar = TRUE, R = 2, burn = 1000, nscan = 3000,
              intercept = TRUE)
summary(srrm.12)
plot(srrm.12)
p.srrm.12 <- round(pmax(srrm.12$YPM, 0),0)
(mae.srrm.12 <- mean(abs(y1 - p.srrm.12), na.rm = TRUE))




#### CLUSTER SOCIALITA' ####
source("src/dataset.R")
var2 <- c("Socializing", "Career & Business", "Outdoors & Adventure")
data <- data[data$category_name %in% var2,]
eventi_data2 <- filtro_e(data, 30)$data
eventi_sociomatrice2 <- sociomatrice_eventi(eventi_data2)
y2 <- as.matrix(eventi_sociomatrice2)
diag(y2) <- rep(NA, NCOL(y2))

var.ame2 <- variabili_ame(eventi_sociomatrice2, eventi_data2)

srrm.20 <- ame(y2, Xdyad = var.ame2$Xd, Xrow = var.ame2$Xm[,-1], Xcol = var.ame2$Xm[,-1],
              symmetric = TRUE, nvar = TRUE, R = 0, burn = 1000, nscan = 3000,
              intercept = TRUE)
summary(srrm.20)
p.srrm.20 <- round(pmax(srrm.20$YPM, 0),0)
(mae.srrm.20 <- mean(abs(y2 - p.srrm.20), na.rm = TRUE))

srrm.21 <- ame(y2, Xdyad = var.ame2$Xd, Xrow = var.ame2$Xm[,-1], Xcol = var.ame2$Xm[,-1],
              symmetric = TRUE, nvar = TRUE, R = 1, burn = 1000, nscan = 3000,
              intercept = TRUE)
summary(srrm.21)
p.srrm.21 <- round(pmax(srrm.21$YPM, 0),0)
(mae.srrm.21 <- mean(abs(y2 - p.srrm.21), na.rm = TRUE))

srrm.2 <- ame(y2, Xdyad = var.ame2$Xd, Xrow = var.ame2$Xm[,-1], Xcol = var.ame2$Xm[,-1],
             symmetric = TRUE, nvar = TRUE, R = 2, burn = 1000, nscan = 3000,
             intercept = TRUE)
summary(srrm.2)
p.srrm.2 <- round(pmax(srrm.2$YPM, 0),0)
(mae.srrm.2 <- mean(abs(y2 - p.srrm.2), na.rm = TRUE))
