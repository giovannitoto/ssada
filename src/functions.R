# ---------------------------------------------------------------------------- #
# NO LIBRARY
# ---------------------------------------------------------------------------- #

filtro_me = function(data, soglia) {
  # seleziona gli utenti che hanno hanno partecipato almeno a 'soglia' eventi
  # e' irrilevante il gruppo di appartenenza degli eventi
  dd <- sqldf("SELECT member_id, COUNT(*) AS 'Eventi'
             FROM data GROUP BY member_id")
  id_list <- unique(dd[dd$Eventi>=soglia, ]$member_id)
  new_data <- data[data$member_id %in% id_list, ]
  
  cat(paste("Numero di utenti considerati:        ", length(id_list), "\n"))
  cat(paste("Numero di partecipazioni considerate:", NROW(new_data), "\n"))
  
  list(id_list=id_list, data=new_data)
}

filtro_mge = function(data, soglia) {
  # seleziona gli utenti che hanno hanno partecipato almeno a 'soglia' eventi
  # di un gruppo
  dd <- sqldf("SELECT member_id, group_id, COUNT(*) AS 'Eventi'
                          FROM data GROUP BY member_id, group_id")
  id_list <- unique(dd[dd$Eventi>=soglia, ]$member_id)
  new_data <- data[data$member_id %in% id_list, ]
  
  cat(paste("Numero di utenti considerati:        ", length(id_list), "\n"))
  cat(paste("Numero di partecipazioni considerate:", NROW(new_data), "\n"))
  
  list(id_list=id_list, data=new_data)
}

sociomatrice_membri <- function(data) {
  #  nodi : 'member_id'
  # archi : numero 'event_id' in cui 'member_id' partecipano insieme
  event_list <- unique(data$event_id)
  members <- sort(unique(data$member_id))
  n <- length(members)
  sociomatrice <- matrix(0, n, n)
  for (ev in event_list) {
    id_list <- unique(data[data$event_id==ev,]$member_id)
    if (length(id_list)>1) {
      coppie <- combn(id_list, 2)
      for (c in 1:NCOL(coppie)) {
        i <- match(coppie[1,c], members)
        j <- match(coppie[2,c], members)
        sociomatrice[i,j] <- sociomatrice[i,j] + 1
        sociomatrice[j,i] <- sociomatrice[j,i] + 1
      }
    }
    data <- data[!(data$event_id==ev),]
  }
  sociomatrice <- as.data.frame(sociomatrice)
  colnames(sociomatrice) <- rownames(sociomatrice) <- members
  sociomatrice
}

# ---------------------------------------------------------------------------- #

filtro_e = function(data, soglia) {
  # seleziona gli eventi a cui hanno hanno partecipato almeno 'soglia' partecipanti
  dd <- sqldf("SELECT event_id, COUNT(*) AS 'Membri'
             FROM data GROUP BY event_id")
  id_list <- unique(dd[dd$Membri>=soglia, ]$event_id)
  new_data <- data[data$event_id %in% id_list, ]
  
  cat(paste("Numero di eventi considerati:        ", length(id_list), "\n"))
  
  list(id_list=id_list, data=new_data)
}

sociomatrice_eventi <- function(data) {
  # funzione che crea la sociomatrice (eventi come nodi)
  member_list <- unique(data$member_id)
  events <- sort(unique(data$event_id))
  n <- length(events)
  sociomatrice <- matrix(0, n, n)
  for (me in member_list) {
    id_list <- unique(data[data$member_id==me,]$event_id)
    if (length(id_list)>1) {
      coppie <- combn(id_list, 2)
      for (c in 1:NCOL(coppie)) {
        i <- match(coppie[1,c], events)
        j <- match(coppie[2,c], events)
        sociomatrice[i,j] <- sociomatrice[i,j] + 1
        sociomatrice[j,i] <- sociomatrice[j,i] + 1
      }
    }
    data <- data[!(data$member_id==me),]
  }
  sociomatrice <- as.data.frame(sociomatrice)
  colnames(sociomatrice) <- rownames(sociomatrice) <- events
  sociomatrice
}

# ---------------------------------------------------------------------------- #