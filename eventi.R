# setwd("C:/Users/Giovanni/Desktop/ssada")
# setwd("H:/ssada")
# ---------------------------------------------------------------------------- #
library(sqldf)       # SQL
library(igraph)      # graph
# ---------------------------------------------------------------------------- #

# Importo funzioni
source("src/functions.R")

# Salvo in memoria il dataset da analizzare
source("src/dataset.R")

# ---------------------------------------------------------------------------- #

# Seleziona gli eventi a cui hanno hanno partecipato almeno 'soglia' partecipanti
eventi_data <- filtro_e(data, 70)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)

e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                   mode="undirected", weighted=T, diag=F)
plot(e.net)
hist(degree(e.net), nclass=30)
hist(betweenness(e.net), nclass=100)


# ---------------------------------------------------------------------------- #


# MODELLO SRRM CON FATTORI LATENTI UNIDIMENSIONALI (R = 1)

library(amen)


# Creo la sociomatrice

eventi_data <- filtro_e(data, 70)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)

e.net.srm <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                         mode="undirected", weighted=T, diag=F)
hist(degree(e.net), nclass=30)
hist(betweenness(e.net), nclass=100)

y <- as.matrix(eventi_sociomatrice)


# Seleziono le variabili di nodo (categoria e numerosità del gruppo)

groupsCC =c("group_id"="character", "group_name"="character",
            "num_members"="integer", "category_id"="character",
            "category_name"="character", "organizer_id"="character",
            "group_urlname"="character")
groups = read.csv(file="data/meta-groups.csv", header=T, stringsAsFactors=F,
                  strip.white=T, colClasses=groupsCC)

eventsCC = c("event_id"="character", "group_id"="character",
             "name"="character", "time"="character")
events = read.csv(file="data/meta-events.csv", header=T, stringsAsFactors=F,
                  strip.white=T, colClasses=eventsCC)
colnames(events)[3] = "event_name"

events_groups = merge(events, groups, by="group_id", all.x=TRUE)
events_groups$num_members <- as.numeric(events_groups$num_members)
str(events_groups)

rownames(events_groups) <- events_groups$event_id

y <- as.matrix(eventi_sociomatrice)
Xn <- events_groups[rownames(y), c("group_id","category_id","num_members")]
Xn <- as.data.frame(Xn)
Xn$category_id <- as.factor(Xn$category_id)
Xn$group_id <- as.numeric(Xn$group_id)
Xn$num_members <- as.numeric(Xn$num_members)
Xm <- model.matrix(~Xn$category_id+Xn$num_members)
head(Xm)


# Variabili diadiche

stessogruppo <- matrice_stessogruppo(eventi_data)
stessacategoria <- matrice_stessacategoria(eventi_data)

Xd <- array(c(as.matrix(stessogruppo),as.matrix(stessacategoria)),
            c(NROW(stessogruppo),NCOL(stessogruppo),2))
str(Xd)


# Modello

r1 <- ame(y, Xdyad = Xd, Xrow = Xm[,-1], Xcol = Xm[,-1], R = 1, symmetric = TRUE)


# ---------------------------------------------------------------------------- #


# RAPPRESENTAZIONE GRAFICA RETE

library(network)
library(ggplot2)
library(GGally)
library(sna)
library(intergraph)
dev.off()


eventi <- filtro_e(data[!((data$category_name=="Pets & Animals") | (data$category_name=="Singles")),], 40)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi)
e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                     mode="undirected", weighted=T, diag=F)

# Per selezionare categoria
groupsCC =c("group_id"="character", "group_name"="character",
            "num_members"="integer", "category_id"="character",
            "category_name"="character", "organizer_id"="character",
            "group_urlname"="character")
groups = read.csv(file="data/meta-groups.csv", header=T, stringsAsFactors=F,
                  strip.white=T, colClasses=groupsCC)
eventsCC = c("event_id"="character", "group_id"="character",
             "name"="character", "time"="character")
events = read.csv(file="data/meta-events.csv", header=T, stringsAsFactors=F,
                  strip.white=T, colClasses=eventsCC)
colnames(events)[3] = "event_name"
events_groups = merge(events, groups, by="group_id", all.x=TRUE)
events_groups$num_members <- as.numeric(events_groups$num_members)
rownames(events_groups) <- events_groups$event_id

category_list <- events_groups[rownames(eventi_sociomatrice), c("category_name")]
category_list <- as.factor(category_list)
levels(category_list)
table(category_list)
category_list <- as.numeric(category_list) + 1

ggnet2(net = e.net, 
       mode = "fruchtermanreingold", # placement method (default)
       color = category_list, # colore nodi
       size = 2.5, # grandezza dei nodi
       max_size = 5, # grandezza massima nodi
       shape = 19, # forma nodi
       layout.exp = 0, # regola grandezza immagine (0 è grande)
       alpha = 1, # trasparenza nodi (tra 0 e 1)
       label = NULL, # etichetta sui nodi
       label.color = "blue", # colore etichetta nodi
       label.size = 4.5, # grandezza etichetta nodi
       edge.color = "blue", # colore nodi
       edge.size = 0.3, # spessore archi
       edge.alpha = 0.4, # trasparenza archi (tra 0 e 1)
       edge.lty = 1, # tipo di linea archi
       edge.label = NULL, # etichette archi
       legend.size = 8, # dimensione legenda
       legend.position = "right" # posizione legenda
)



