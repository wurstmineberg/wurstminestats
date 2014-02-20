# Datapreparations

# Getting player stats
library(RCurl)
library(jsonlite)
library(ggplot2)
library(scales)     # For datetime scales on plots
library(gridExtra)  # For annotations outside of plot

## Getting some data for furthe use
# Get people.json for player id and join dates
people <- fromJSON("http://wurstmineberg.de/assets/serverstatus/people.json")
## Get player stats from wurstmineberg API
playerstats <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")

# A little cleanup
names(playerstats) <- sub("stat.","",names(playerstats))
playerstats[playerstats == "NULL"] <- "0"
playerTemp <- names(playerstats[,1])
## Add "organic" category to people$status for easier matching
people$status[is.na(people$status)] <- "later"
## Getting rid of the nested list stuff
## This took me so long, please don't even ask me about it.
for(i in (1:23)){
  playerstats[i] <- unlist(playerstats[i], use.names=F)
} 
rm(i)
## Getting rid of NAs and assuming 0
playerstats[playerstats == NA] <- 0
## Numericizzle
playerstats <- as.data.frame(mapply(as.numeric,playerstats))
## Sorting according to people.json
playerstats$player <- playerTemp; rm(playerTemp);
## Crucial part where we enhance the original list by matching with people.json
playerstats <- playerstats[match(people$minecraft[people$status != "former"], playerstats$player),]
## Get joinDate from people.json, excluding former members
playerstats$joinDate <- people$join_date[people$status != "former"]
## Get rid of unnecessary rownames column (we'll get an index later)
rownames(playerstats) <- NULL
## Resort columns to get name and joinDate first. Optional!
playerstats <- playerstats[c(ncol(playerstats)-1,
                             ncol(playerstats),
                             1:(ncol(playerstats)-2))]
## Convert player names to people.json-IDs
playerstats$player <- people$id[people$status != "former"]
## Convert to factors with appropriate levels
playerstats$player <- factor(playerstats$player, 
                             levels=playerstats$player)

## Getting rid of NAs and assuming 0 (again. Don't ask.)
playerstats[is.na(playerstats)] <- 0
## Just to get a numeric ID to have an easy index and player number
playerstats$number <- (1:(nrow(playerstats)))

## Give people status values because lol
playerstats$joinStatus <- as.factor(people$status[people$status != "former"])

## In case of missing join date, apply NA
playerstats$joinDate[playerstats$joinDate == 0] <- NA

## Convert joinDate to POSIXct because time
playerstats$joinDate <- as.POSIXct(playerstats$joinDate, 
                                   origin="1970-01-01")


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

## Get a vector of the age gaps starting from player[1]
c(0,playerstats$serverAge[1:25] - playerstats$serverAge[2:26])
