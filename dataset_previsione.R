# ---------------------------------------------------------------------------- #

setwd("D:\\UNIVERSITA\\GUIDOLIN\\Progetto")

library(sqldf)       
library(lubridate)   
library(chron)

# ---------------------------------------------------------------------------- #

# Importo funzioni
source("src/functions.R")

# Salvo in memoria il dataset da analizzare
source("src/dataset.R")

# ---------------------------------------------------------------------------- #

data$anno <- format(as.Date(data$time), "%Y")
data$mese <- format(as.Date(data$time), "%m")
data$giorno <- format(as.Date(data$time), "%d")
data$settimana <- weekdays(as.Date(data$time), abbr=TRUE)
data$weekend <- is.weekend(as.Date(data$time))

# ---------------------------------------------------------------------------- #

# Filtro il dataset in modo da considerare solo i gli eventi con un numero 
# minimo di partecipanti
mge_data <- filtro_mge(data, 30)$data

# Conto il numeri di eventi a cui ogni utente ha partecipato per ogni gruppo
gruppo_per_membro <- sqldf("SELECT member_id, group_id, COUNT(*) AS 'Eventi'
                           FROM mge_data GROUP BY member_id, group_id")
attivi_gruppo <- sqldf("SELECT group_id, COUNT(*) AS 'attivi'
                        FROM data GROUP BY group_id")

data <- merge(data, attivi_gruppo, by="group_id", all.x=TRUE)

# ---------------------------------------------------------------------------- #

partecipanti_evento <- sqldf("SELECT event_id, COUNT(*) AS 'y'
                              FROM data GROUP BY event_id")

data <- merge(data, partecipanti_evento, by="event_id", all.x=TRUE)

# ---------------------------------------------------------------------------- #


var_list <- c(1,24,9,6,7,23,18,19,21,22)
data <- data[,var_list]

rm(attivi_gruppo, gruppo_per_membro, mge_data, partecipanti_evento)

data <- sqldf("SELECT * FROM data GROUP BY event_id")
rownames(data) <- data[,1]
data <- data[,-1]

colnames(data) <- c("y", "categoria", "gruppo", "membri_gr", "attivi_gr", 
                    "anno", "mese", "weekday", "weekend")
data$y <- as.numeric(data$y)
data$categoria <- as.factor(data$categoria)
data$gruppo <- as.factor(data$gruppo)
data$membri_gr <- as.numeric(data$membri_gr)
data$attivi_gr <- as.numeric(data$attivi_gr)
data$anno <- as.factor(data$anno)
data$mese <- as.factor(data$mese)
data$weekday <- as.factor(data$weekday)
data$weekend <- as.factor(data$weekend)






