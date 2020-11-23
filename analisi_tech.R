# ANALISI DELLA CATEGORIA 'Tech'

# ---------------------------------------------------------------------------- #

library(sqldf)        # SQL
library(igraph)       # graph
library(network)      # ggnet2
library(ggplot2)      # ggnet2
library(GGally)       # ggnet2
library(sna)          # ggnet2
library(intergraph)   # ggnet2
library(RColorBrewer) # colors

# ---------------------------------------------------------------------------- #

# Importo funzioni
source("src/functions.R")

# Salvo in memoria il dataset da analizzare
source("src/dataset.R")

# ---------------------------------------------------------------------------- #

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

rm(list=c("groupsCC","groups","eventsCC","events"))

# ---------------------------------------------------------------------------- #

# Definisco colori
qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors,
                            rownames(qual_col_pals)))
rm(qual_col_pals)
# Anteprima dei primi n colori in col_vector, tramite un grafico a torta
n <- 40; pie(rep(1,n), col=col_vector[1:n])

# ---------------------------------------------------------------------------- #

# Filtro dataset e seleziono solo category=Tech
data <- data[data$category_name=="Tech",]

# Analisi esplorative al variare della 'soglia'
soglia_list <- c(70, 60, 50, 40, 30, 20)
for (SOGLIA in soglia_list) {
  ev <- filtro_e(data, SOGLIA)$data
  cat(paste("Soglia:",SOGLIA,"=>",length(unique(ev$group_name))),"gruppi\n\n")
}
rm(list=c("SOGLIA","ev"))

# ---------------------------------------------------------------------------- #

# Creo diverse sociamatrici al variare della 'soglia' e le salvo in un array
# Esporto in images/eventi_loop i grafi delle sociomatrici
ID <- "tech"  # per distinguere i risultati
soglia_list <- c(70, 60, 50)
eventi_sociomatrice_list <- list()
for (SOGLIA in soglia_list) {
  # Fisso nome del file
  name <- paste(ID, "_eventi_sociomatrice_",SOGLIA, sep="")
  
  # Creo sociomatrice e grafo
  eventi <- filtro_e(data, SOGLIA)$data
  eventi_sociomatrice <- sociomatrice_eventi(eventi)
  
  eventi_sociomatrice_list[[name]] <- eventi_sociomatrice
  
  # Identifico categorie e assegno colori
  group_list <- events_groups[rownames(eventi_sociomatrice), c("group_name")]
  group_list <- as.factor(group_list)
  tot_events <- length(group_list)
  
  # Modifico nome dei livelli, aggiungendo la numerosita' (num. eventi)
  for (j in 1:length(levels(group_list))) {
    nj <- sum(group_list==levels(group_list)[j])
    levels(group_list)[j] <- paste(levels(group_list)[j]," (",nj,")", sep="")
  }
  
  # Creo grafo  
  e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                       mode="undirected", weighted=T, diag=F)
  
  # Apro file .png
  ratio <- 1.5
  png(paste("images/eventi_loop/grafo_", name, ".png", sep=""),
      width=ratio*950,height=ratio*670)
  # Creo grafico
  COL <- col_vector[1:length(levels(group_list))]
  names(COL) <- levels(group_list)
  
  myplot <- ggnet2(net = e.net, 
                   mode = "fruchtermanreingold", # placement method (default)
                   size = "degree", # grandezza dei nodi
                   size.cut = TRUE,  # gruppi di grandezza dei nodi (quartili)
                   #max_size = 5, # grandezza massima nodi
                   shape = 19, # forma nodi
                   layout.exp = 0, # regola grandezza immagine (0 e' grande)
                   alpha = 1, # trasparenza nodi (tra 0 e 1)
                   label = NULL, # etichetta sui nodi
                   label.color = "blue", # colore etichetta nodi
                   label.size = 4.5, # grandezza etichetta nodi
                   edge.color = "#CBD5E8", # colore nodi (col_vector[40])
                   edge.size = 0.3, # spessore archi
                   edge.alpha = 0.8, # trasparenza archi (tra 0 e 1)
                   edge.lty = 1, # tipo di linea archi
                   edge.label = NULL, # etichette archi
                   legend.size = 15, # dimensione legenda
                   legend.position = "right", # posizione legenda
                   
                   color = group_list, # colore nodi
                   color.legend = paste("group (", tot_events, ")", sep=""),
                   palette = COL
  )
  # Salvo grafico nel file .png
  print(myplot)
  # Chiudo file .png
  dev.off()
}

# ----------------------------------------------------------------------------- #