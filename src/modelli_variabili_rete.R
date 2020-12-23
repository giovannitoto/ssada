#### FUNZIONI UTILI ####

variabili_ame <- function(sociomatrice, eventi_data)
{ # Crea le matrici di variabili di nodo e diadiche per un modello ame
  source("src/functions.R")
  
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
  
  y <- as.matrix(sociomatrice)
  
  Xn <- events_groups[rownames(y), c("category_id","num_members")]
  Xn <- as.data.frame(Xn)
  Xn$category_id <- as.factor(Xn$category_id)
  Xn$num_members <- as.numeric(Xn$num_members)
  Xm <- model.matrix(~Xn$category_id+Xn$num_members)
  
  stessogruppo <- matrice_stessogruppo(eventi_data)
  stessacategoria <- matrice_stessacategoria(eventi_data)
  Xd <- array(c(as.matrix(stessogruppo),as.matrix(stessacategoria)),
              c(NROW(stessogruppo),NCOL(stessogruppo),2))
  
  return(list(Xm = Xm, Xd = Xd, same_group = stessogruppo, 
              same_cat = stessacategoria, Xn = Xn))
}


variabili_ame_tech <- function(sociomatrice, eventi_data)
{ # Crea le matrici di variabili di nodo e diadiche per un modello ame
  source("src/functions.R")
  
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
  
  y <- as.matrix(sociomatrice)
  
  Xn <- events_groups[rownames(y), c("group_name","num_members")]
  Xn <- as.data.frame(Xn)
  Xn$group_name <- as.factor(Xn$group_name)
  Xn$num_members <- as.numeric(Xn$num_members)
  Xn[,1] <- factor(Xn[,1], levels=c(levels(Xn[,1]), "altro"))
  Xn[,1][Xn[,1] %in% attr(table(Xn[,1])[table(Xn[,1]) <= 10],
                          "dimnames")[[1]]] = "altro"
  Xn[,1] <- factor(Xn[,1])
  Xm <- model.matrix(~Xn$group_name+Xn$num_members)
  
  stessogruppo <- matrice_stessogruppo(eventi_data)
  Xd <- array(c(as.matrix(stessogruppo)),
              c(NROW(stessogruppo),NCOL(stessogruppo)))
  
  return(list(Xm = Xm, Xd = Xd, same_group = stessogruppo, Xn = Xn))
}
