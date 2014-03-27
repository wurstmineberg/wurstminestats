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

# Get a close enough timestamp for the data age. Reimport via as.POSIXct(x,origin="1970-01-01") should be sufficient, else handle as.numeric()
dataTime <- format(Sys.time(), "%s")

## Get player stats from wurstmineberg API ##
playerstats   <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")
achievements  <- fromJSON("http://api.wurstmineberg.de/server/playerstats/achievement.json")
entities      <- fromJSON("http://api.wurstmineberg.de/server/playerstats/entity.json")
items         <- fromJSON("http://api.wurstmineberg.de/server/playerstats/item.json")

####################################################################################
#### This is where imported datasets get cleaned up so we can actually use them ####
####################################################################################

## Getting a people dataset from people.json ##
activePeople <- getActivePeople()

## Reformat stat datasets ##
playerstats   <- prettyShitUp(playerstats)
achievements  <- prettyShitUp(achievements)
entities      <- prettyShitUp(entities)
items         <- prettyShitUp(items)

#################################################
## Enhancing playerstats with some useful shit ##
#################################################

playerstats$joinStatus  <- activePeople$joinStatus
playerstats$joinDate    <- activePeople$joinDate

## Convert play time to real time hours as separate column ##
playerstats$playOneHour <- (playerstats$playOneMinute/20/60/60)

## Get total distance column by summing up all *OneCm rows per player ##
playerstats$distanceTraveled <- 0
for(i in 1:nrow(playerstats)){
  playerstats$distanceTraveled[i] <- sum(playerstats[i, grep("OneCm", colnames(playerstats))])
}; rm(i);

## Resort columns to get interesting stuff first. ##
playerstats$timestamp <- dataTime

generalColumns <- c("timestamp", "player", "joinDate", "joinStatus", "leaveGame",
                    "deaths", "timeSinceDeath", "playerKills", "damageDealt", "damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")

playerstats <- playerstats[c(generalColumns, setdiff(names(playerstats), generalColumns))]
rm(generalColumns)

## Join playerstats with achievements and entities dataframes. ##
playerstats <- join(playerstats, achievements)
playerstats <- join(playerstats, entities)
rm(achievements, entities)

##########################
## Handle items dataset ##
##########################

## Get items.json from our website for names/ids ##
itemData        <- getItemData()

## Get table of item actions and readable names ##
itemActions     <- data.frame(id    = c("mineBlock", "craftItem"   , "useItem" , "breakItem"), 
                              name  = c("mined"    , "crafted"     , "used"    , "broken"))

## Merge old and new item stat IDs and whate have you ##
items <- mergeItemStats(items, itemActions, itemData)

## Get a dataframe of item stat ID, item name and action ##
itemStats <- getItemStats(items, itemActions, itemData)

## Finally join the remaining datasets ## 
playerstats <- join(playerstats, items, type="full")
rm(items)

# Sooner or later, I want a giant logfile.
writePlayerstatsLog()

######################################################
## Define some variables for plot layout and labels ##
######################################################

playerTheme <- theme(legend.position  = "right",
                    legend.key.size   = unit(.4, "cm"),
                    legend.text       = element_text(size = rel(.8)))

plotWidth <- 6; plotHeight <- 4;
barChart  <- geom_bar(colour="black", width=.7, stat="identity")
xLable    <- xlab("Player")

# Define colour scale to keep status/people colours static
statusColours         <- brewer.pal(9,"Set1")
statusFillScale       <- scale_fill_manual(   name = "Join Status", values = statusColours)
statusColourScale     <- scale_colour_manual( name = "Join Status", values = statusColours)

# Quick fix overrides
activePeople$color[activePeople$name == "Farthen"] <- "#800080"
activePeople          <- fixPeopleColors(activePeople)

legendPeople          <- scale_fill_manual(name = "People", values = activePeople$color)
