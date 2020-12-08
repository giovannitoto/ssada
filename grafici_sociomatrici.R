# GRAFICI DELLE SOCIOMATRICI - GGPLOT

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

densita <- function(sociomatrice) {
  n <- nrow(sociomatrice)
  sum(sociomatrice>0) / (n*(n-1))
}

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

# SENZA VINCOLI: 70-30 => 50

# Seleziono sociomatrice su cui applicare il procedimento
NAME <- "_30"
eventi_data <- filtro_e(data, 30)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)

# length(unique(eventi_data$category_name))      # categorie considerate
# length(unique(eventi_data$group_name))         # gruppi considerati
# length(unique(eventi_data$member_id))          # membri distinti considerati
# part.tot <- nrow(eventi_data); part.tot        # partecipazioni totali
# part.tot/length(unique(eventi_data$member_id)) # partecipazioni medie

# Creo grafo  
# e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
#                                      mode="undirected", weighted=T, diag=F)
# n.nodi <- length(V(e.net)); n.nodi         # numero nodi
# n.archi <- length(E(e.net)); n.archi       # numero archi
# archi.max <- n.nodi*(n.nodi-1); archi.max  # numero max di archi
# edge_density(e.net)                        # densita'
# sum(eventi_sociomatrice) / (2*n.archi)     # valore medio degli archi (y_{ij}>0)
# mean(igraph::degree(e.net))                # grado medio
# median(igraph::degree(e.net))              # grado mediano
# mean(igraph::betweenness(e.net))           # betweenness medio
# median(igraph::betweenness(e.net))         # betweenness mediano
# diameter(e.net)                            # diametro

# Definisco impostazioni del grafico
ratio <- 1.5
COL <- c("#7A7A7A","#FB9A99","#E31A1C","#1B9E77","#80FF80",
         "#F0027F","#E6AB02","#D95F02","#BF80BF","#1F78B4",
         "#000000")
#pie(rep(1,length(COL)), col=COL[1:length(COL)])
names(COL) <- class.c <- c("Career & Business",
                           "Dancing",
                           "Games",
                           "Movies & Film",
                           "Outdoors & Adventure",
                           "Pets & Animals",
                           "Religion & Beliefs",
                           "Singles",
                           "Socializing",
                           "Tech",
                           "Other")

# Calcolo numero eventi per le categorie principali
# category_list <- events_groups[rownames(eventi_sociomatrice), c("category_name")]
# category_list <- as.factor(category_list)
# 
# new_levels <- levels(category_list)
# for (c in 1:length(levels(category_list))) {
#   if (!(levels(category_list)[c] %in% class.c)) {
#     new_levels[c] <- "Other"
#   }
# }
# levels(category_list) <- new_levels
# for (c in 1:length(levels(category_list))) {
#   cat(paste(levels(category_list)[c], ":",
#             sum(category_list==levels(category_list)[c]),"\n"))
# }

# Elimino nodi con troppo pochi archi
sort(rowSums(eventi_sociomatrice))[1:8]
nodi_da_rimuovere <- c("224633651")
for (nn in nodi_da_rimuovere) {
  eventi_sociomatrice <- eventi_sociomatrice[!(rownames(eventi_sociomatrice) %in% nodi_da_rimuovere), ]
  eventi_sociomatrice <- eventi_sociomatrice[, !(colnames(eventi_sociomatrice) %in% nodi_da_rimuovere)]
}

# Identifico categorie e assegno colori
category_list <- events_groups[rownames(eventi_sociomatrice), c("category_name")]
category_list <- as.factor(category_list)

new_levels <- levels(category_list)
for (c in 1:length(levels(category_list))) {
  if (!(levels(category_list)[c] %in% class.c)) {
    new_levels[c] <- "Other"
  }
}
levels(category_list) <- new_levels
for (c in 1:length(levels(category_list))) {
  cat(paste(levels(category_list)[c], ":",
            sum(category_list==levels(category_list)[c]),"\n"))
}

# Creo grafo ridotto
e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                     mode="undirected", weighted=T, diag=F)

# Apro file .png
png(paste("img_latex/grafo_", NAME, ".png", sep=""),width=ratio*950,height=ratio*670)
myplot <- ggnet2(net = e.net, 
                 mode = "fruchtermanreingold", # placement method (default)
                 size = "degree",              # grandezza dei nodi
                 size.cut = TRUE,              # gruppi di grandezza dei nodi (quartili)
                 #max_size = 5,                # grandezza massima nodi
                 shape = 19,                   # forma nodi
                 layout.exp = 0,               # regola grandezza immagine (0 e' grande)
                 alpha = 1,                    # trasparenza nodi (tra 0 e 1)
                 label = NULL,                 # etichetta sui nodi
                 label.color = "blue",         # colore etichetta nodi
                 label.size = 4.5,             # grandezza etichetta nodi
                 edge.color = "#CBD5E8",       # colore nodi
                 edge.size = 0.3,              # spessore archi
                 edge.alpha = 0.8,             # trasparenza archi (tra 0 e 1)
                 edge.lty = 1,                 # tipo di linea archi
                 edge.label = NULL,            # etichette archi
                 legend.size = 15,             # dimensione legenda
                 legend.position = "right",    # posizione legenda
                 
                 color = category_list,        # colore nodi
                 color.legend = paste("Nodi non visualizzati:",length(nodi_da_rimuovere),"\n\nCategory:"),
                 palette = COL
)
# Salvo grafico nel file .png
print(myplot)
# Chiudo file .png
dev.off()

# ---------------------------------------------------------------------------- #

# TECH: 70-20 => 40

# Seleziono sociomatrice su cui applicare il procedimento
NAME <- "_00"
eventi_data <- filtro_e(data[data$category_name=="Tech",], 70)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)

length(unique(eventi_data$category_name))      # categorie considerate
length(unique(eventi_data$group_name))         # gruppi considerati
length(unique(eventi_data$member_id))          # membri distinti considerati
part.tot <- nrow(eventi_data); part.tot        # partecipazioni totali
part.tot/length(unique(eventi_data$member_id)) # partecipazioni medie

# Creo grafo  
e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                     mode="undirected", weighted=T, diag=F)
n.nodi <- length(V(e.net)); n.nodi         # numero nodi
n.archi <- length(E(e.net)); n.archi       # numero archi
archi.max <- n.nodi*(n.nodi-1); archi.max  # numero max di archi
edge_density(e.net)                        # densita'
sum(eventi_sociomatrice) / (2*n.archi)     # valore medio degli archi (y_{ij}>0)
mean(igraph::degree(e.net))                # grado medio
median(igraph::degree(e.net))              # grado mediano
mean(igraph::betweenness(e.net))           # betweenness medio
median(igraph::betweenness(e.net))         # betweenness mediano
diameter(e.net)                            # diametro

# Definisco impostazioni del grafico
ratio <- 1.5
COL <- c("#E31A1C","#1B9E77","#F0027F","#FB9A99","#1F78B4","#E6AB02","#4DAF4A",
         "#6A3D9A","#FFFF33","#80FF80","#999999","#A6761D","#D95F02","#BF80BF",
         "#000000")
pie(rep(1,length(COL)), col=COL[1:length(COL)])
names(COL) <- class.c <- c("Agile Nashville User Group",
                           "Code for Nashville",
                           "Data Science Nashville",
                           "Franklin Developer Lunch & Learn",
                           "NashJS",
                           "Nashville .NET User Group",
                           "Nashville Blockchain Meetup",
                           "Nashville DevOps Meetup",
                           "Nashville Mobile Developers",
                           "Nashville Modern Excel & Power BI User Group",
                           "Nashville Product Meetup",
                           "Nashville UX",
                           "PyNash",
                           "The Nashville Microsoft Azure Users Group",
                           "Other")

# Calcolo numero eventi per le categorie principali
category_list <- events_groups[rownames(eventi_sociomatrice), c("group_name")]
category_list <- as.factor(category_list)

new_levels <- levels(category_list)
for (c in 1:length(levels(category_list))) {
  if (!(levels(category_list)[c] %in% class.c)) {
    new_levels[c] <- "Other"
  }
}
levels(category_list) <- new_levels
for (c in 1:length(levels(category_list))) {
  cat(paste(levels(category_list)[c], ":",
            sum(category_list==levels(category_list)[c]),"\n"))
}

# Apro file .png
png(paste("img_latex/grafo_tech_", NAME, ".png", sep=""),width=ratio*950,height=ratio*670)
myplot <- ggnet2(net = e.net, 
                 mode = "fruchtermanreingold", # placement method (default)
                 size = "degree",              # grandezza dei nodi
                 size.cut = TRUE,              # gruppi di grandezza dei nodi (quartili)
                 #max_size = 5,                # grandezza massima nodi
                 shape = 19,                   # forma nodi
                 layout.exp = 0,               # regola grandezza immagine (0 e' grande)
                 alpha = 1,                    # trasparenza nodi (tra 0 e 1)
                 label = NULL,                 # etichetta sui nodi
                 label.color = "blue",         # colore etichetta nodi
                 label.size = 4.5,             # grandezza etichetta nodi
                 edge.color = "#CBD5E8",       # colore nodi
                 edge.size = 0.3,              # spessore archi
                 edge.alpha = 0.8,             # trasparenza archi (tra 0 e 1)
                 edge.lty = 1,                 # tipo di linea archi
                 edge.label = NULL,            # etichette archi
                 legend.size = 15,             # dimensione legenda
                 legend.position = "right",    # posizione legenda
                 
                 color = category_list,        # colore nodi
                 color.legend = "Group:",
                 palette = COL
)
# Salvo grafico nel file .png
print(myplot)
# Chiudo file .png
dev.off()

# ---------------------------------------------------------------------------- #

# Visualizzare gruppi e loro numerosita' al variare della soglia:
# utile per tracciare l'evoluzione del numero di eventi
eventi_data <- filtro_e(data[data$category_name == "Tech",], 50)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)
category_list <- events_groups[rownames(eventi_sociomatrice), c("group_name")]
category_list <- as.factor(category_list)
for (c in 1:length(levels(category_list))) {
  cat(paste(levels(category_list)[c], ":",
            sum(category_list==levels(category_list)[c]),"\n"))
}

# ---------------------------------------------------------------------------- #

# GRUPPO2: 70-20 => 40

# Seleziono sociomatrice su cui applicare il procedimento
NAME <- "_00"
cat_list <- c("Socializing", "Outdoors & Adventure", "Career & Business")
eventi_data <- filtro_e(data[data$category_name %in% cat_list,], 0)$data
eventi_sociomatrice <- sociomatrice_eventi(eventi_data)

# length(unique(eventi_data$category_name))      # categorie considerate
# length(unique(eventi_data$group_name))         # gruppi considerati
# length(unique(eventi_data$member_id))          # membri distinti considerati
# part.tot <- nrow(eventi_data); part.tot        # partecipazioni totali
# part.tot/length(unique(eventi_data$member_id)) # partecipazioni medie

# Creo grafo  
# e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
#                                      mode="undirected", weighted=T, diag=F)
# n.nodi <- length(V(e.net)); n.nodi         # numero nodi
# n.archi <- length(E(e.net)); n.archi       # numero archi
# archi.max <- n.nodi*(n.nodi-1); archi.max  # numero max di archi
# edge_density(e.net)                        # densita'
# sum(eventi_sociomatrice) / (2*n.archi)     # valore medio degli archi (y_{ij}>0)
# mean(igraph::degree(e.net))                # grado medio
# median(igraph::degree(e.net))              # grado mediano
# mean(igraph::betweenness(e.net))           # betweenness medio
# median(igraph::betweenness(e.net))         # betweenness mediano
# diameter(e.net)                            # diametro

# Definisco impostazioni del grafico
ratio <- 1.5
COL <- c("#E31A1C","#1B9E77","#F0027F","#1F78B4","#E6AB02","#4DAF4A",
         "#6A3D9A","#FFFF33","#80FF80","#A6761D","#D95F02","#BF80BF",
         "#000000")
pie(rep(1,length(COL)), col=COL[1:length(COL)])
names(COL) <- class.c <- c("20's & 30's Women looking for girlfriends",
                           "20s in Nashville",
                           "Business Girls Rock! Nashville Entrepreneurship Meetup",
                           "Music City Young Professionals",
                           "Nashville Flight Training",
                           "Nashville Hiking Meetup",
                           "Nashville Online Entrepreneurs",
                           "Nashville SEO & Internet Marketing, Over 1,600 Members!",
                           "Nashville Young Professionals Meetup",
                           "Transplant Nashville",
                           "Women 'n' Wine of Williamson County",
                           "WOMEN \"Word of Mouth Entrepreneurial Networkers\"",
                           "Other")

# Calcolo numero eventi per le categorie principali
# category_list <- events_groups[rownames(eventi_sociomatrice), c("group_name")]
# category_list <- as.factor(category_list)
# 
# new_levels <- levels(category_list)
# for (c in 1:length(levels(category_list))) {
#   if (!(levels(category_list)[c] %in% class.c)) {
#     new_levels[c] <- "Other"
#   }
# }
# levels(category_list) <- new_levels
# for (c in 1:length(levels(category_list))) {
#   cat(paste(levels(category_list)[c], ":",
#             sum(category_list==levels(category_list)[c]),"\n"))
# }

# Elimino nodi con troppo pochi archi
# sort(rowSums(eventi_sociomatrice))[1:8]
# nodi_da_rimuovere <- c()
# for (nn in nodi_da_rimuovere) {
#   eventi_sociomatrice <- eventi_sociomatrice[!(rownames(eventi_sociomatrice) %in% nodi_da_rimuovere), ]
#   eventi_sociomatrice <- eventi_sociomatrice[, !(colnames(eventi_sociomatrice) %in% nodi_da_rimuovere)]
# }

# Identifico categorie e assegno colori
category_list <- events_groups[rownames(eventi_sociomatrice), c("group_name")]
category_list <- as.factor(category_list)

new_levels <- levels(category_list)
for (c in 1:length(levels(category_list))) {
  if (!(levels(category_list)[c] %in% class.c)) {
    new_levels[c] <- "Other"
  }
}
levels(category_list) <- new_levels
for (c in 1:length(levels(category_list))) {
  cat(paste(levels(category_list)[c], ":",
            sum(category_list==levels(category_list)[c]),"\n"))
}

# Associo forme diverse ai tre gruppi considerati
shape_list <- events_groups[rownames(eventi_sociomatrice), c("category_name")]
shape_list <- as.factor(shape_list)
# shape_list[shape_list=="Socializing"] <- 
# shape_list[shape_list=="Outdoors & Adventure"] <- 
# shape_list[shape_list=="Career & Business"] <- 

# Creo grafo ridotto
e.net <- graph_from_adjacency_matrix(as.matrix(eventi_sociomatrice), 
                                     mode="undirected", weighted=T, diag=F)

# Apro file .png
png(paste("img_latex/grafo_socialita_", NAME, ".png", sep=""),width=ratio*950,height=ratio*670)
myplot <- ggnet2(net = e.net, 
                 mode = "fruchtermanreingold", # placement method (default)
                 size = "degree",              # grandezza dei nodi
                 size.cut = TRUE,              # gruppi di grandezza dei nodi (quartili)
                 #max_size = 5,                # grandezza massima nodi
                 layout.exp = 0,               # regola grandezza immagine (0 e' grande)
                 alpha = 1,                    # trasparenza nodi (tra 0 e 1)
                 label = NULL,                 # etichetta sui nodi
                 label.color = "blue",         # colore etichetta nodi
                 label.size = 4.5,             # grandezza etichetta nodi
                 edge.color = "#CBD5E8",       # colore nodi
                 edge.size = 0.3,              # spessore archi
                 edge.alpha = 0.8,             # trasparenza archi (tra 0 e 1)
                 edge.lty = 1,                 # tipo di linea archi
                 edge.label = NULL,            # etichette archi
                 legend.size = 15,             # dimensione legenda
                 legend.position = "right",    # posizione legenda
                 
                 shape.legend = "Category:",
                 shape = shape_list,           # forma nodi
                 
                 color = category_list,        # colore nodi
                 color.legend = "Group:",
                 palette = COL
)
# Salvo grafico nel file .png
print(myplot)
# Chiudo file .png
dev.off()



