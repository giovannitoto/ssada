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
n <- 20; pie(rep(1,n), col=col_vector[1:n])


# ---------------------------------------------------------------------------- #

# Creo diverse sociamatrici al variare della 'soglia' e le salvo in un array
# Esporto in images/eventi_loop i grafi delle sociomatrici
soglia_list <- c(70, 60, 50, 40, 30)
eventi_sociomatrice_list <- list()
for (SOGLIA in soglia_list) {
  # Fisso nome del file
  name <- paste("eventi_sociomatrice_",SOGLIA, sep="")
  
  # Creo sociomatrice e grafo
  eventi <- filtro_e(data, SOGLIA)$data
  eventi_sociomatrice <- sociomatrice_eventi(eventi)
  
  eventi_sociomatrice_list[[name]] <- eventi_sociomatrice
  
  # Creo grafo  
  e.net <- network(as.matrix(eventi_sociomatrice), directed=F)
  
  # Identifico categorie e assegno colori
  category_list <- events_groups[rownames(eventi_sociomatrice), c("category_name")]
  category_list <- as.factor(category_list)
  
  # Apro file .png
  ratio <- 1.5
  png(paste("images/eventi_loop/TTTgrafo_", name, ".png", sep=""),
      width=ratio*950,height=ratio*670)
  # Creo grafico
  COL <- col_vector[1:length(levels(category_list))]
  names(COL) <- levels(category_list)
  
  myplot <- ggnet2(net = e.net, 
                   mode = "fruchtermanreingold", # placement method (default)
                   size = 3.5, # grandezza dei nodi
                   max_size = 5, # grandezza massima nodi
                   shape = 19, # forma nodi
                   layout.exp = 0, # regola grandezza immagine (0 è grande)
                   alpha = 1, # trasparenza nodi (tra 0 e 1)
                   label = NULL, # etichetta sui nodi
                   label.color = "blue", # colore etichetta nodi
                   label.size = 4.5, # grandezza etichetta nodi
                   edge.color = "#CBD5E8", # colore nodi (col_vector[40])
                   edge.size = 0.3, # spessore archi
                   edge.alpha = 0.4, # trasparenza archi (tra 0 e 1)
                   edge.lty = 1, # tipo di linea archi
                   edge.label = NULL, # etichette archi
                   legend.size = 20, # dimensione legenda
                   legend.position = "right", # posizione legenda
                   
                   color = category_list, # colore nodi
                   color.legend = "categoria",
                   palette = COL
                  )
  # Salvo grafico nel file .png
  print(myplot)
  # Chiudo file .png
  dev.off()
}

# ---------------------------------------------------------------------------- #