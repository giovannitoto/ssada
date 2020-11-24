#### FUNZIONI UTILI ####

variabili_ame <- function(sociomatrice)
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
  
  Xn <- events_groups[rownames(y), c("group_id","category_id","num_members")]
  Xn <- as.data.frame(Xn)
  Xn$category_id <- as.factor(Xn$category_id)
  Xn$group_id <- as.numeric(Xn$group_id)
  Xn$num_members <- as.numeric(Xn$num_members)
  Xm <- model.matrix(~Xn$category_id+Xn$num_members)
  
  stessogruppo <- matrice_stessogruppo(eventi_data)
  stessacategoria <- matrice_stessacategoria(eventi_data)
  Xd <- array(c(as.matrix(stessogruppo),as.matrix(stessacategoria)),
              c(NROW(stessogruppo),NCOL(stessogruppo),2))
  
  return(list(Xm = Xm, Xd = Xd, same_group = stessogruppo, 
              same_cat = stessacategoria, Xn = Xn))
}



variabili_modelli <- function(sociomatrice, same_group, same_cat, Xn)
{ # Ritorna variabili ame vettorizzate e rispettiva model.matrix
  y <- as.matrix(sociomatrice)
  xd_gr_v <- c(as.matrix(same_group))
  xd_cat_v <- c(as.matrix(same_cat))
  a_v <- rep(rowMeans(y), NCOL(y))
  diag(y) <- rep(NA, NCOL(y))
  y_v <- c(y)
  num_r_v <- rep(as.vector(Xn$num_members), NCOL(y))
  cat_r_v <- rep(as.vector(Xn$category_id), NCOL(y))
  num_c_v <- cat_c_v <- c()
  for(i in 1:NCOL(y)){
    num_c_v <- c(num_c_v, rep(Xn$num_members[i], NCOL(y)))
    cat_c_v <- c(cat_c_v, rep(Xn$category_id[i], NCOL(y)))
  }
  dati_vett <- as.data.frame(cbind(num_r_v, num_c_v, cat_r_v, cat_c_v,
                                   xd_gr_v, xd_cat_v, a_v))
  dati_vett[,1] <- as.numeric(dati_vett[,1])
  dati_vett[,2] <- as.numeric(dati_vett[,2])
  dati_vett[,7] <- as.numeric(dati_vett[,7])
  mm <- model.matrix(~., data = dati_vett)
  
  return(list(dframe = dati_vett, modelmat = mm, y_v = y_v))
}




matrice_previsioni <- function(pred.v, ncol_y)
{ # Ritorna una matrice con i valori predetti per ogni cella della sociomatrice.
  # per ottenere una pred.v adatta (può cambiare a seconda del modello) bisogna
  # prima scrivere:
  #     pred.v <- c(predict(mod))  oppure fitted(mod)
  #     names(pred.v) <- NULL
  pred <- matrix(NA, ncol_y, ncol_y)
  pred.v1 <- rep(NA, ncol_y)
  for (j in 1:ncol_y){
    pred.v1 <- pred.v[1 : (ncol_y-1)]
    pred.v <- pred.v[-(1 : (ncol_y-1))]
    for (i in 1:ncol_y){
      if (i < j){
        pred[i,j] <- pred.v1[i]
      } else if (i > j){
        pred[i,j] <- pred.v1[i-1]
      }
    }
  }
  return(pred)
}





















