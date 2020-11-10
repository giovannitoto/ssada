#setwd("C:/Users/Giovanni/Desktop/ssada")
setwd("H:/ssada")
# ---------------------------------------------------------------------------- #
library(sqldf)       # SQL
library(igraph)      # graph
# ---------------------------------------------------------------------------- #

# Importo funzioni
source("src/functions.R")

# Salvo in memoria il dataset da analizzare
source("src/dataset.R")

# ---------------------------------------------------------------------------- #

# Filtro il dataset in modo da considerare solo gli utenti con un numero minimo
# di partecipazioni ad un gruppo

# Conto il numeri di eventi a cui ogni utente ha partecipato per ogni gruppo
gruppo_per_membro <- sqldf("SELECT member_id, group_id, COUNT(*) AS 'Eventi'
                           FROM data GROUP BY member_id, group_id")
hist(gruppo_per_membro$Eventi, nclass=100, xlim=c(0,20),
     main=paste("Istogramma di",NROW(gruppo_per_membro),"coppie (evento,membro)"))
table(gruppo_per_membro$Eventi)[1:20]
prop.table(table(gruppo_per_membro$Eventi)[1:20])

mge_data <- filtro_mge(data, 50)$data
mge_sociomatrice <- sociomatrice_membri(mge_data)

mge.net <- graph_from_adjacency_matrix(as.matrix(mge_sociomatrice),
                                       mode="undirected", weighted=T, diag=F)
plot(mge.net)
hist(degree(mge.net), nclass=30)
hist(betweenness(mge.net), nclass=100)

# ---------------------------------------------------------------------------- #

Y <- as.matrix(mge_sociomatrice)

lattice::levelplot(Y)
lattice::levelplot(log(Y+0.1)) # leggermente meglio
lattice::levelplot(sqrt(Y))

Y.log <- log(Y+0.1)
lattice::levelplot(Y)


# ---------------------------------------------------------------------------- #

# Modello ANOVA
Row.Id1 <- matrix(rownames(Y), nrow(Y), ncol(Y))
Col.Id1 <- t(Row.Id1)
Row.Id2 <- matrix(rownames(Y.log), nrow(Y.log), ncol(Y.log))
Col.Id2 <- t(Row.Id2)

mod.anova1 <- lm( c(Y) ~ c(Row.Id1) + c(Col.Id1), na.action = na.exclude )
mod.anova2 <- lm( c(Y.log) ~ c(Row.Id2) + c(Col.Id2), na.action = na.exclude ) 

anova(mod.anova1)
anova(mod.anova2) 
summary(mod.anova1) # R2 = 
summary(mod.anova2) # R2 = 

# ---------------------------------------------------------------------------- #

# Confronto tra utenti in termini di media di riga
# nota: media di colonna e riga coincidono poiche' Y e' simmetrica
rmean <- rowMeans(Y, na.rm=T)   # medie di riga
mu.hat <- mean(Y, na.rm=T)      # media totale
a.hat <- rmean - mu.hat         # scostamento di riga dalla media

# Effetti di riga
a.hat[1:5]
head( sort(a.hat, decreasing=T) )

# ---------------------------------------------------------------------------- #

# Social Relation Model
fit_SRM <- ame(Y)
summary(fit_SRM)

mu.hat             # ANOVA - media generale  
mean(fit_SRM$BETA) #   SRM - media generale  

fit_SRM$APM[1:8]   # stima effetti di riga
fit_SRM$BPM[1:8]   # stima effetti di colonna

gofstats(Y)
# sd.rowmean sd.colmean   dyad.dep  triad.dep 
# 


# ---------------------------------------------------------------------------- #

# Social Relation Regression Model 

# Preparo le covariate di nodo (nodal covariates)
membersCC =c("member_id"="character", "name"="character", "hometown"="character",
             "city"="character", "state"="character",
             "lat"="character", "lon"="character")
members = read.csv(file="data/meta-members.csv", header=T, stringsAsFactors=F,
                   strip.white=T, colClasses=membersCC)
colnames(members)[2] = "member_name"
row.names(members) <- members$member_id

members$lat <- as.numeric(members$lat)
members$lon <- as.numeric(members$lon)
Xn <- as.matrix(members[rownames(Y), c("lat","lon")])
# Preparo le covariate diadiche (dyadic covariates)
# Xd <- GRUPPI IN COMUNE


# Una volta preparate le var. d'interesse, stimo il modello:
# - Xd : var. diadiche
# - Xr : var. di riga
# - Xc : var. di colonna
fit_srrm <- ame(Y, Xr=Xn, Xc=Xn)
summary(fit_srrm)

fit_ame1 <- ame(Y, Xr=Xn, Xc=Xn, R=1)
summary(fit_ame1)

fit_ame2 <- ame(Y, Xr=Xn, Xc=Xn, R=2)
summary(fit_ame2)

# ---------------------------------------------------------------------------- #