# ---------------------------------------------------------------------------- #
# NO LIBRARY
# ---------------------------------------------------------------------------- #

# Importo dataset
dataCC = c("X"="NULL", "event_id"="character",
           "member_id"="character", "group_id"="character")
data = read.csv(file="data/rsvps.csv", header=T, stringsAsFactors=F,
                strip.white=T, colClasses=dataCC)

eventsCC = c("event_id"="character", "group_id"="NULL",
             "name"="character", "time"="character")
events = read.csv(file="data/meta-events.csv", header=T, stringsAsFactors=F,
                  strip.white=T, colClasses=eventsCC)
colnames(events)[2] = "event_name"

groupsCC =c("group_id"="character", "group_name"="character",
            "num_members"="integer", "category_id"="character",
            "category_name"="character", "organizer_id"="character",
            "group_urlname"="character")
groups = read.csv(file="data/meta-groups.csv", header=T, stringsAsFactors=F,
                  strip.white=T, colClasses=groupsCC)

membersCC =c("member_id"="character", "name"="character", "hometown"="character",
             "city"="character", "state"="character",
             "lat"="character", "lon"="character")
members = read.csv(file="data/meta-members.csv", header=T, stringsAsFactors=F,
                   strip.white=T, colClasses=membersCC)
colnames(members)[2] = "member_name"

rm(list=c("dataCC","eventsCC","groupsCC","membersCC"))

# ---------------------------------------------------------------------------- #

# Unisco i quattro dataset
data = merge(data, events, by="event_id", all.x=TRUE)
data = merge(data, groups, by="group_id", all.x=TRUE)
data = merge(data, members, by="member_id", all.x=TRUE)
data = data[, c(3,2,1,4:17)]

rm(list=c("events","groups","members"))

# ---------------------------------------------------------------------------- #

# Controllo altri dataset
# group_edges = read.csv("data/group-edges.csv", sep=",", header=T)[,2:4]
# member_edges = read.csv("data/member-edges.csv", sep=",", header=T)[,2:4]
# member_to_group_edges = read.csv("data/member-to-group-edges.csv", sep=",", header=T)
# 
# rm(list=c("group_edges","member_edges","member_to_group_edges","group_edges"))

# ---------------------------------------------------------------------------- #
