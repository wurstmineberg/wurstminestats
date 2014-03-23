## Datapreparations

# Loading all necessary libraries
library(RCurl)
library(jsonlite)
library(ggplot2)
library(scales)       # For datetime scales on plots
library(grid)         # for unit() in ggplot theme() functions
#library(gridExtra)   # For annotations outside of plot ## TODO
library(plyr)         # To join() dataframes
library(RColorBrewer) # Because colours
library(httr)         # For direct web access stuff, apparently
library(gdata)        # For some reorder() stuff. Factor levels are hell, people.

source("functions.R")

# Get a close enough timestamp for the data age
# Reimport via as.POSIXct(x,origin="1970-01-01") should be sufficient
now <- format(Sys.time(), "%s")

## Get people.json for player id and join dates
people <- fromJSON("http://wurstmineberg.de/assets/serverstatus/people.json")
## Get player stats from wurstmineberg API
playerstats <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")
## Get achievements
achievements <- fromJSON("http://api.wurstmineberg.de/server/playerstats/achievement.json")
## Get item stats
items <- fromJSON("http://api.wurstmineberg.de/server/playerstats/item.json")
## Get entity stats 
entities <- fromJSON("http://api.wurstmineberg.de/server/playerstats/entity.json")
## Get achievement descriptions from website and append IDs as extra column
achievementStrings <- fromJSON("http://wurstmineberg.de/static/json/achievements.json")
achievementStrings$id <- names(achievementStrings[,1])
## Get strings.json for some… strings.
strings <- fromJSON("http://wurstmineberg.de/static/json/strings.json")
## Get latest deaths log
latestdeaths <- fromJSON("http://api.wurstmineberg.de/server/deaths/latest.json")

#### This is where imported datasets get cleaned up so we can actually use them ####

## Reformat people.json
people <- as.data.frame(people[1])
names(people) <- sub("people.","",names(people))
# Add category to people$status for easier matching
people$status[is.na(people$status)] <- "later"

activePeople <- data.frame(id=rep(0, length(people$minecraft[people$status != "former"])), 
                           mc=rep(0, length(people$minecraft[people$status != "former"])))
activePeople$mc <- people$minecraft[people$status != "former"]
activePeople$name <- people$name[people$status != "former"]
activePeople$id <- people$id[people$status != "former"]

for(i in 1:length(activePeople$name)){
  if(is.na(activePeople$name[i])){
    activePeople$name[i] <- activePeople$id[i]
  }
}; rm(i)

# Reformat stat datasets
playerstats   <- prettyShitUp(playerstats)
achievements  <- prettyShitUp(achievements)
entities      <- prettyShitUp(entities)
items         <- prettyShitUp(items)

## Get joinDate from people.json, excluding former members
playerstats$joinDate <- people$join_date[people$status != "former"]

## Getting rid of NAs and assuming 0 (again. Don't ask.)
playerstats[is.na(playerstats)] <- 0

# Just to get a numeric ID to have an easy index and player numID. Yes, rownames, I know.
playerstats$numID <- (1:(nrow(playerstats)))

## Give people status values because lol
playerstats$joinStatus <- as.factor(people$status[people$status != "former"])

# In case of missing join date, apply NA
# For invited but not yet joined players
playerstats$joinDate[playerstats$joinDate == 0] <- NA

# Convert joinDate to POSIXct because time
playerstats$joinDate <- as.POSIXct(playerstats$joinDate, origin="1970-01-01")

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

# Get total distance column by summing up all *OneCm rows per player
playerstats$distanceTraveled <- 0
for(i in 1:nrow(playerstats)){
  playerstats$distanceTraveled[i] <- sum(playerstats[i, grep("OneCm", colnames(playerstats))])
}; rm(i);

## Resort columns to get interesting stuff first. Manually, like a fucking animal.
generalColumns <- c("player", "numID", "joinDate", "joinStatus", "serverAge", "serverBirth", "leaveGame",
                    "deaths", "timeSinceDeath", "playerKills","damageDealt","damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")
itemColumns <- c("drop", "fishCaught", "treasureFished", "junkFished")
distanceColumns <- c("distanceTraveled","walkOneCm", "crouchOneCm", "sprintOneCm", "climbOneCm", "minecartOneCm", "horseOneCm", 
                     "boatOneCm", "pigOneCm", "fallOneCm", "swimOneCm", "diveOneCm", "flyOneCm")

playerstats <- playerstats[c(generalColumns,itemColumns,distanceColumns)]
rm(generalColumns,itemColumns,distanceColumns)

## Join playerstats with achievements and entities dataframes. This feels very epic.
playerstats <- join(playerstats, achievements)
playerstats <- join(playerstats, entities)

# Delete dataframes we don't need separate anymore
rm(achievements, entities)

# Reorganizzle rownames just in case ¯\_(ツ)_/¯
#rownames(playerstats) <- playerstats$player

# Sub player names with display names from activePeople$name
playerstats$player <- factor(activePeople$name, levels=activePeople$name)
items$player <- playerstats$player

## At this point, playerstats is in a usable state, data is comfortably accessible and it contains
## both the general player stats and the achievement data. 
## What happens now is my attempt to create a log file of playerstats in playerstat.csv, which in the future 
## might be interesting as a basis for playeractivity over time.

# Sooner or later, I want a giant logfile.
playerstats$timestamp <- now

writePlayerstatsLog()
