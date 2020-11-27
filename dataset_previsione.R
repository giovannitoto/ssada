# ---------------------------------------------------------------------------- #

library(sqldf)       # SQL
library(lubridate)   # date
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

# Filtro il dataset in modo da considerare solo gli utenti con un numero minimo
# di partecipazioni ad un gruppo
mge_data <- filtro_mge(data, 50)$data
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

var_list <- c()

data <- data[, var_list]

data <- sqldf("SELECT * FROM data GROUP BY event_id")









