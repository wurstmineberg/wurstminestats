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

## Get player stats from wurstmineberg API
playerstats <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")
## Get achievements
achievements <- fromJSON("http://api.wurstmineberg.de/server/playerstats/achievement.json")
## Get entity stats 
entities <- fromJSON("http://api.wurstmineberg.de/server/playerstats/entity.json")

#### This is where imported datasets get cleaned up so we can actually use them ####

## Reformat people.json
activePeople <- getActivePeople()

# Reformat stat datasets
playerstats   <- prettyShitUp(playerstats)
achievements  <- prettyShitUp(achievements)
entities      <- prettyShitUp(entities)

## Getting rid of NAs and assuming 0 (again. Don't ask.)
playerstats[is.na(playerstats)] <- 0

# Just to get a numeric ID to have an easy index and player numID. Yes, rownames, I know.
playerstats$numID <- (1:(nrow(playerstats)))

## Give people joinStatus and joinDate from activePoeple
playerstats$joinStatus  <- activePeople$joinStatus
playerstats$joinDate    <- activePeople$joinDate
playerstats$serverAge   <- activePeople$serverAge
playerstats$serverBirth <- activePeople$serverBirth

## Convert play time to real time hours as separate column
playerstats$playOneHour <- (playerstats$playOneMinute/20/60/60)

# Get total distance column by summing up all *OneCm rows per player
playerstats$distanceTraveled <- 0
for(i in 1:nrow(playerstats)){
  playerstats$distanceTraveled[i] <- sum(playerstats[i, grep("OneCm", colnames(playerstats))])
}; rm(i);

## Resort columns to get interesting stuff first. ##
generalColumns <- c("player", "numID", "joinDate", "joinStatus", "serverAge", "serverBirth", "leaveGame",
                    "deaths", "timeSinceDeath", "playerKills", "damageDealt", "damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")

playerstats <- playerstats[c(generalColumns, setdiff(names(playerstats), generalColumns))]
rm(generalColumns)

## Join playerstats with achievements and entities dataframes. ##
playerstats <- join(playerstats, achievements)
playerstats <- join(playerstats, entities)

# Delete dataframes we don't need separate anymore
rm(achievements, entities)

## At this point, playerstats is in a usable state, data is comfortably accessible and it contains
## the general player stats, the achievement data and entity stats. Item stats are dealt with in itemStats.R 
## What happens now is my attempt to create a log file of playerstats in playerstat.csv, which in the future 
## might be interesting as a basis for player activity over time.

# Sooner or later, I want a giant logfile.
playerstats$timestamp <- now

writePlayerstatsLog()

# Define general legend/guide for all players
playerTheme <- theme(legend.position  = "right",
                    legend.key.size   = unit(.4, "cm"),
                    legend.text       = element_text(size = rel(.8))
                    )

# Define some variables for bar chart layout and labels
plotWidth <- 6; plotHeight <- 4;
barChart  <- geom_bar(colour="black", width=.7, stat="identity")
xLable    <- xlab("Player")

# Define colour scale to keep status colours static
statusColours         <- brewer.pal(9,"Set1")
names(statusColours)  <- levels(activePeople$joinStatus)
legendTitle           <- scale_fill_manual(name = "Join Status", values = statusColours)
