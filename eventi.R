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

library(network)
library(ggplot2)
library(GGally)
library(sna)
library(intergraph)
dev.off()
ggnet2(net = net, 
       mode = "fruchtermanreingold", # placement method (default)
       color = "red", # colore nodi
       size = "degree", # grandezza dei nodi
       max_size = 7, # grandezza massima nodi
       shape = 19, # forma nodi
       layout.exp = 0, # regola grandezza immagine (0 è grande)
       alpha = 0.65, # trasparenza nodi (tra 0 e 1)
       label = NULL, # etichetta sui nodi
       label.color = "blue", # colore etichetta nodi
       label.size = 4.5, # grandezza etichetta nodi
       edge.color = "blue", # colore nodi
       edge.size = 0.3, # spessore archi
       edge.alpha = 0.9, # trasparenza archi (tra 0 e 1)
       edge.lty = 1, # tipo di linea archi
       edge.label = NULL, # etichette archi
       legend.size = 8, # dimensione legenda
       legend.position = "right" # posizione legenda
)

# ---------------------------------------------------------------------------- #

# Modello SRRM
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
Xn <- events_groups[rownames(y), c("category_id")]

head(Xn)

library(amen)
srrm <- ame(y, Xr = Xn, Xc = Xn)







