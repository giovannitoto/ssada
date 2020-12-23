# ISTOGRAMMA NUMEROSITA' EVENTI

# ---------------------------------------------------------------------------- #

library(sqldf)        # SQL
library(tidyverse)
library(gridExtra)

# ---------------------------------------------------------------------------- #

# Importo funzioni
source("src/functions.R")

# Salvo in memoria il dataset da analizzare
source("src/dataset.R")

# ---------------------------------------------------------------------------- #

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

dd <- sqldf("SELECT event_id, COUNT(*) AS 'partecipanti'
             FROM data GROUP BY event_id")
dd <- merge(dd, events_groups[,c("event_id","category_name")], by="event_id")
dd$category_name <- as.factor(dd$category_name)
colnames(dd)[3] <- "category"

class.c <- c("Career & Business",
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

COL <- c("#7A7A7A","#FB9A99","#E31A1C","#1B9E77","#80FF80",
         "#F0027F","#E6AB02","#D95F02","#BF80BF","#1F78B4",
         "#000000")

new_levels <- levels(dd$category)
for (c in 1:length(levels(dd$category))) {
  if (!(levels(dd$category)[c] %in% class.c)) {
    new_levels[c] <- "Other"
  }
}
levels(dd$category) <- new_levels

p1 <- ggplot(dd, aes(x=partecipanti)) +
    geom_histogram( binwidth=3, fill="#7bf87b", color="#e9ecef", alpha=1) +
    labs(x="partecipanti", y = "conteggio") +
    coord_cartesian(xlim = c(0, 60)) +
    scale_x_continuous(breaks=seq(0,150,20))

soglia <- c(30,40,50,60,70,30,40,50,60,70)
soglia_y <- c(rep(80,5),rep(77,5))
eventi_soglia1 <- c(625,303,164,85,48)
eventi_soglia2 <- as.character(round(c(625,303,164,85,48) / 19031 * 100,2))
eventi_soglia2 <- paste(eventi_soglia2,"%",sep="")
eventi_soglia <- c(eventi_soglia1,eventi_soglia2)

ss <- as.data.frame(cbind(soglia+1, soglia_y, eventi_soglia))
colnames(ss) <- c("soglia","soglia_y","eventi_soglia")
ss$soglia <- as.numeric(as.character(ss$soglia))
ss$soglia_y <- as.numeric(as.character(ss$soglia_y))

p2 <- ggplot(dd[dd$partecipanti>=30,], aes(x=partecipanti)) +
    geom_histogram( binwidth=2, fill="#7bf87b", color="#e9ecef", alpha=1) +
    labs(x="partecipanti", y = "conteggio") +
    coord_cartesian(xlim = c(30, 141), ylim=c(0,80)) +
    scale_x_continuous(breaks=c(30,40,50,60,70,146)) +
    scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
    geom_vline(xintercept=soglia,linetype="dashed") +
    geom_text(ss, mapping=aes(x=soglia, y=soglia_y,
                              label=eventi_soglia),
              angle=0, hjust=0, size=3)

p3 <- ggplot(dd, aes(partecipanti, fill=category)) +
      geom_histogram( binwidth=3, color="#e9ecef", alpha=1) +
      labs(x="partecipanti", y = "conteggio") +
      coord_cartesian(xlim = c(0, 141)) +
      scale_x_continuous(breaks=seq(0,150,20)) +
      scale_fill_manual(breaks=class.c,values=COL)

p4 <- ggplot(dd[dd$partecipanti>=30,], aes(x=partecipanti, fill=category)) +
      geom_histogram( binwidth=2, color="#e9ecef", alpha=1) +
      labs(x="partecipanti", y = "conteggio") +
      coord_cartesian(xlim = c(30, 141)) +
      scale_x_continuous(breaks=c(30,40,50,60,70,100,141)) +
      scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
      scale_fill_manual(breaks=class.c,values=COL)

grid.arrange(p1, p2, ncol=2, widths=c(4,7))
