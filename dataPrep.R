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
## Add "none" category to people$status for easier matching
people$status[is.na(people$status)] <- "none"
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
playerstats <- playerstats[match(people$minecraft[people$status != "former"], playerstats$player),]

playerstats$joinDate <- people$join_date[people$status != "former"]

rownames(playerstats) <- NULL

#playerstats <- playerstats[playerstats == NA] <- "0"

playerstats <- playerstats[c(ncol(playerstats)-1,
                             ncol(playerstats),
                             1:(ncol(playerstats)-2))]

playerstats$player <- people$id[people$status != "former"]

playerstats$player <- factor(playerstats$player, 
                             levels=playerstats$player)

## Getting rid of NAs and assuming 0 (again. Don't ask.)
playerstats[is.na(playerstats)] <- 0

playerstats$number <- (1:(nrow(playerstats))) # Just to get a numeric ID

# Convert joinDate to POSIX-time
playerstats$joinDate[playerstats$joinDate == 0] <- NA

playerstats$age <- round(as.numeric(difftime(playerstats$joinDate[playerstats$number],
                    playerstats$joinDate[1], 
                    units ="days")))

playerstats$joinDate <- as.POSIXct(playerstats$joinDate, origin="1970-01-01")