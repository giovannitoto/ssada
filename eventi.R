# ---------------------------------------------------------------------------- #

library(sqldf)       # SQL
library(igraph)      # graph
library(amen)        # ame
library(network)     # ggnet2
library(ggplot2)     # ggnet2
library(GGally)      # ggnet2
library(sna)         # ggnet2
library(intergraph)  # ggnet2

# ---------------------------------------------------------------------------- #

# Importo funzioni
source("src/functions.R")

# Salvo in memoria il dataset da analizzare
source("src/dataset.R")

# ---------------------------------------------------------------------------- #

# Importo metadati di gruppi e eventi
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
rm(list=c("groupsCC","groups","eventsCC","events"))

# ---------------------------------------------------------------------------- #

# MODELLO SRRM CON FATTORI LATENTI UNIDIMENSIONALI (R = 1)


# Seleziona gli eventi a cui hanno hanno partecipato almeno 'soglia' partecipanti
eventi_data <- filtro_e(data, 70)$data

# Creo la sociomatrice
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)

# Creo grafo a partire dalla sociomatrice
e.net.srm <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                         mode="undirected", weighted=T, diag=F)
plot(e.net)
hist(degree(e.net), nclass=30)
hist(betweenness(e.net), nclass=100)

# Variabile risposta
y <- as.matrix(eventi_sociomatrice)

# Variabili di nodo
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


# Modelli AME
r1 <- ame(y, Xdyad = Xd, Xrow = Xm[,-1], Xcol = Xm[,-1], R = 1, symmetric = T)
r2 <- ame(y, Xdyad = Xd, Xrow = Xm[,-1], Xcol = Xm[,-1], R = 2, symmetric = T)

# ---------------------------------------------------------------------------- #

# RAPPRESENTAZIONE GRAFICA RETE
# Vedi 'eventi_loop.R' per una versione migliorata

eventi <- filtro_e(data, 40)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi)
e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                     mode="undirected", weighted=T, diag=F)

# Identifico categoria di ogni evento presente nella rete
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
dev.off()

# ---------------------------------------------------------------------------- #