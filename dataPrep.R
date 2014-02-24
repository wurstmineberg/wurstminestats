# Datapreparations

library(RCurl)
library(jsonlite)
library(ggplot2)
library(scales)     # For datetime scales on plots
library(gridExtra)  # For annotations outside of plot ## TODO
library(plyr)       # To join() dataframes

## Get people.json for player id and join dates
people <- fromJSON("http://wurstmineberg.de/assets/serverstatus/people.json")
## Get player stats from wurstmineberg API
playerstats <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")
## Get achievements
achievements <- fromJSON("http://api.wurstmineberg.de/server/playerstats/achievement.json")
## Get item stats
items <- fromJSON("http://api.wurstmineberg.de/server/playerstats/item.json")


# A little cleanup
names(playerstats) <- sub("stat.","",names(playerstats))
playerstats[playerstats == "NULL"] <- "0"

names(achievements) <- sub("achievement.","",names(achievements))
achievements <- achievements[names(achievements) != "exploreAllBiomes"]
achievements[achievements == "NULL"] <- "0"

names(items) <- sub("stat.","",names(items))
items[items == "NULL"] <- "0"

# Extract player names to separate variable
playerTemp <- names(playerstats[,1])

## Add category to people$status for easier matching
people$status[is.na(people$status)] <- "later"

## Getting rid of the nested list stuff
# This took me so long, please don't even ask me about it.
for(i in (1:(ncol(playerstats)))) {
  playerstats[i] <- unlist(playerstats[i], use.names=F)
}; rm(i);

# Do the same for the achievement dataset
for(i in (1:(ncol(achievements)))) {
  achievements[i] <- unlist(achievements[i], use.names=F)
}; rm(i);

# Do the same for the items dataset
for(i in (1:(ncol(items)))) {
  items[i] <- unlist(items[i], use.names=F)
}; rm(i);


# Getting rid of NAs and assuming 0
playerstats[playerstats == NA] <- 0

# Numericizzle
playerstats <- as.data.frame(mapply(as.numeric,playerstats))
achievements <- as.data.frame(mapply(as.numeric,achievements))
items <- as.data.frame(mapply(as.numeric,items))

## Sorting according to people.json
playerstats$player <- playerTemp
achievements$player <- playerTemp
items$player <- playerTemp
rm(playerTemp)

# Crucial part where we enhance the original list by matching with people.json
playerstats <- playerstats[match(people$minecraft[people$status != "former"], playerstats$player),]
achievements <- achievements[match(people$minecraft[people$status != "former"], achievements$player),]
items <- items[match(people$minecraft[people$status != "former"], items$player),]

## Get joinDate from people.json, excluding former members
playerstats$joinDate <- people$join_date[people$status != "former"]

## Convert player names to people.json-IDs
playerstats$player <- people$id[people$status != "former"]
achievements$player <- people$id[people$status != "former"]
items$player <- people$id[people$status != "former"]

# Convert to factors with appropriate levels
playerstats$player <- factor(playerstats$player, levels=playerstats$player)
achievements$player <- factor(playerstats$player, levels=playerstats$player)
items$player <- factor(playerstats$player, levels=playerstats$player)

## Getting rid of NAs and assuming 0 (again. Don't ask.)
playerstats[is.na(playerstats)] <- 0

# Just to get a numeric ID to have an easy index and player numID. Yes, rownames, I know.
playerstats$numID <- (1:(nrow(playerstats)))

## Give people status values because lol
playerstats$joinStatus <- as.factor(people$status[people$status != "former"])

# In case of missing join date, apply NA / Technically not necessary anymore
playerstats$joinDate[playerstats$joinDate == 0] <- NA

# Convert joinDate to POSIXct because time
playerstats$joinDate <- as.POSIXct(playerstats$joinDate, 
                                   origin="1970-01-01")

## Convert play time to real time hours as separate column
playerstats$playOneHour <- (playerstats$playOneMinute/20/60/60)

## Define server birth and server age times

# player specific server age (days since their whitelisting)
playerstats$serverAge <- round(as.numeric(difftime(Sys.time(),
                          playerstats$joinDate[playerstats$numID], 
                          units ="days")))

# player specific server birth (days they've been whitelisted after server creation)
playerstats$serverBirth <-round(as.numeric(difftime(playerstats$joinDate[playerstats$numID],
                          playerstats$joinDate[1], 
                          units ="days")))
# current server age total
wurstminebergAge <- round(as.numeric(difftime(Sys.time(),
        playerstats$joinDate[1], 
        units ="auto")))

# Get total distance column by summing up all *OneCm rows per player
playerstats$distanceTraveled <- 0
for(i in 1:nrow(playerstats)){
  playerstats$distanceTraveled[i] <- sum(playerstats[i, grep("OneCm", colnames(playerstats))])
}; rm(i);

## Resort columns to get interesting stuff first. Manually, like a fucking animal.
generalColumns <- c("player", "numID", "joinDate", "joinStatus", "serverAge", "serverBirth", "leaveGame",
                    "deaths", "playerKills","damageDealt","damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")
itemColumns <- c("drop", "fishCaught", "treasureFished", "junkFished")
distanceColumns <- c("distanceTraveled","walkOneCm", "climbOneCm", "minecartOneCm", "horseOneCm", 
                     "boatOneCm", "pigOneCm", "fallOneCm", "swimOneCm", "diveOneCm", "flyOneCm")

playerstats <- playerstats[c(generalColumns,itemColumns,distanceColumns)]
rm(generalColumns,itemColumns,distanceColumns)

## Get a vector of the age gaps starting from player[1]
inviteGaps <- c(0,round(as.numeric(difftime(playerstats$joinDate[2:nrow(playerstats)], playerstats$joinDate[1:(nrow(playerstats)-1)], units="days"))))
mean(inviteGaps)

## Join playerstats and achievements data. This feels very epic.
playerstats <- join(playerstats,achievements)

# Delete achievements dataset
rm(achievements)

# Reorganizzle rownames just in case ¯\_(ツ)_/¯
rownames(playerstats) <- playerstats$player

## Write dataset to file for ze easy access
write.csv(playerstats, "data/playerstats.csv")
