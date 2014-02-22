# Datapreparations

library(RCurl)
library(jsonlite)
library(ggplot2)
library(scales)     # For datetime scales on plots
library(gridExtra)  # For annotations outside of plot ## TODO

# Get people.json for player id and join dates
people <- fromJSON("http://wurstmineberg.de/assets/serverstatus/people.json")
## Get player stats from wurstmineberg API
playerstats <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")

# A little cleanup
names(playerstats) <- sub("stat.","",names(playerstats))
playerstats[playerstats == "NULL"] <- "0"

# Extract player names to separate variable
playerTemp <- names(playerstats[,1])

## Add category to people$status for easier matching
people$status[is.na(people$status)] <- "later"

## Getting rid of the nested list stuff
# This took me so long, please don't even ask me about it.
for(i in (1:(ncol(playerstats)))) {
  playerstats[i] <- unlist(playerstats[i], use.names=F)
}; rm(i);

# Getting rid of NAs and assuming 0
playerstats[playerstats == NA] <- 0

# Numericizzle
playerstats <- as.data.frame(mapply(as.numeric,playerstats))

## Sorting according to people.json
playerstats$player <- playerTemp; rm(playerTemp);

# Crucial part where we enhance the original list by matching with people.json
playerstats <- playerstats[match(people$minecraft[people$status != "former"], playerstats$player),]

## Get joinDate from people.json, excluding former members
playerstats$joinDate <- people$join_date[people$status != "former"]

# Get rid of unnecessary rownames column (we'll get an index later)
rownames(playerstats) <- NULL

## Convert player names to people.json-IDs
playerstats$player <- people$id[people$status != "former"]

# Convert to factors with appropriate levels
playerstats$player <- factor(playerstats$player, 
                             levels=playerstats$player)

## Getting rid of NAs and assuming 0 (again. Don't ask.)
playerstats[is.na(playerstats)] <- 0

# Just to get a numeric ID to have an easy index and player number
playerstats$number <- (1:(nrow(playerstats)))

## Give people status values because lol
playerstats$joinStatus <- as.factor(people$status[people$status != "former"])

# In case of missing join date, apply NA
playerstats$joinDate[playerstats$joinDate == 0] <- NA

# Convert joinDate to POSIXct because time
playerstats$joinDate <- as.POSIXct(playerstats$joinDate, 
                                   origin="1970-01-01")

## Convert play time to real time hours
playerstats$playOneHour <- (playerstats$playOneMinute/20/60/60)

## Define server birth and server age times

# player specific server age
playerstats$serverAge <- round(as.numeric(difftime(Sys.time(),
                          playerstats$joinDate[playerstats$number], 
                          units ="days")))
# player specific server birth
playerstats$serverBirth <-round(as.numeric(difftime(playerstats$joinDate[playerstats$number],
                          playerstats$joinDate[1], 
                          units ="days")))
# current server age total
wurstminebergAge <- round(as.numeric(difftime(Sys.time(),
        playerstats$joinDate[1], 
        units ="auto")))

# Get total distance column
playerstats$distanceTraveled <- 0
for(i in 1:nrow(playerstats)){
  playerstats$distanceTraveled[i] <- sum(playerstats[i,grep("OneCm", colnames(playerstats))])
}; rm(i);

## Resort columns to get name and joinDate first. Manually, like a fucking animal.
generalColumns <- c("player", "number", "joinDate", "joinStatus", "serverAge", "serverBirth", "leaveGame",
                    "deaths", "playerKills","damageDealt","damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")
itemColumns <- c("drop", "fishCaught", "treasureFished", "junkFished")
distanceColumns <- c("distanceTraveled","walkOneCm", "climbOneCm", "minecartOneCm", "horseOneCm", 
                     "boatOneCm", "pigOneCm", "fallOneCm", "swimOneCm", "diveOneCm", "flyOneCm")

playerstats <- playerstats[c(generalColumns,itemColumns,distanceColumns)]
## Get a vector of the age gaps starting from player[1]
inviteGaps <- c(0,round(as.numeric(difftime(playerstats$joinDate[2:26], playerstats$joinDate[1:25], units="days"))))
mean(inviteGaps)

## Write dataset to file for ze easy access
write.csv(playerstats, "playerstats.csv")
