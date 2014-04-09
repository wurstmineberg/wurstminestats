## Datapreparations

# Loading all necessary libraries
suppressMessages(library(RCurl))
library(jsonlite)
library(ggplot2)
library(scales)                     # For datetime scales on plots
library(grid)                       # for unit() in ggplot theme() functions
#library(gridExtra)                 # For annotations outside of plot ## TODO
library(plyr)                       # To join() dataframes
library(RColorBrewer)               # Because colours
library(httr)                       # For direct web access stuff, apparently
suppressMessages(library(gdata))    # For some reorder() stuff. Factor levels are hell, people.
library(rCharts)                    # For interactive jsified plotting glory (http://ramnathv.github.io/rCharts/)

if(grepl("shiny$", getwd())){
  source("../functions.R")
} else {
  source("functions.R")
}

# Get a close enough timestamp for the data age. Reimport via as.POSIXct(x,origin="1970-01-01") should be sufficient, else handle as.numeric()
dataTime <- format(Sys.time(), "%s")

## Get player stats from wurstmineberg API ##
playerstats   <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")
achievements  <- fromJSON("http://api.wurstmineberg.de/server/playerstats/achievement.json")
entities      <- fromJSON("http://api.wurstmineberg.de/server/playerstats/entity.json")
items         <- fromJSON("http://api.wurstmineberg.de/server/playerstats/item.json")

## Get strings for better descriptions and names
achievementStrings    <- getAchievementStrings()
# Get strings.json for some… strings. (Mob IDs, display names)
strings <- getStrings()

####################################################################################
#### This is where imported datasets get cleaned up so we can actually use them ####
####################################################################################

## Getting a people dataset from people.json ## (Also, deaths)
activePeople  <- getActivePeople()
birthdays     <- serverBirthday(activePeople)
deaths        <- getDeathStats()

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
generalstats  <- playerstats
playerstats   <- join(playerstats, achievements)
playerstats   <- join(playerstats, entities)

## Leave stat-specific datasets be and polish them up a little for the shiny displays
achievements  <- achievements[c("player", setdiff(names(achievements), "player")) ]
entities      <- entities[c("player", setdiff(names(entities), "player")) ]

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

###################################################
## Getting sessions from /sessions/overview.json ##
###################################################

sessions        <- getSessions()
playerSessions  <- getPlayerSessions(sessions)

# Ideally sessions should be separated per day, I guess?
playerSessions  <- splitSessionsByDay(playerSessions)

# We want play time per day, sooooo… ##
playedPerDay  <- ddply(playerSessions, .(date, wday), summarize, timePlayed = sum(playedMinutes))

# We also want play time per day per person, so, well… ##
playedPerPerson <- getPlayedPerPerson(playerSessions)

# Getting per weekday stuff
playedPerWeekday      <- playedPerPerson
playedPerWeekday      <- ddply(playedPerPerson, .(wday, date, person), summarize, timePlayed=sum(timePlayed))
avgPerWeekday         <- mean(ddply(playedPerWeekday, .(date), summarize, timePlayed=sum(timePlayed))$timePlayed)

#### Getting some strings together ####
# Get general statistics from playerstats, define metadata (scale, units)
statNum             <- ncol(strings$general)
generalStats        <- data.frame(id=character(statNum), name=character(statNum), unit=character(statNum), scale=numeric(statNum))
generalStats$id     <- names(playerstats[names(strings$general)])
generalStats$name   <- unlist(strings$general[2,], use.names=F)
generalStats$unit   <- c("Animals", "km", "km", "km", "Hearts (thousands)", "Hearts (thousands)", "Deathcount", "km", "Items", "km", "Fish", "km", "km", "Jumps (thousands)", "Junk", "Quits", "km", "Mobs", "m", "Hours (real life)", "Kills", "km", "km", "Hours (real life)", "Treasure", "km")
generalStats$scale  <- c(1,         10^5, 10^5, 10^5, 10*2*10^3,            10*2*10^3,             1,           10^5,  1,      10^5,  1,     10^5,  10^5, 1000,                1,      1,      10^5,  1,    1000,  20*60*60,           1,       10^5, 10^5,  20*60*60,           1,          10^5)
rm(statNum)

##############################################################
##### Define some variables for ggplot2 layout and labels ####
##############################################################

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

#activePeople          <- fixPeopleColors(activePeople, 0.95)
legendPeople          <- scale_fill_manual(name = "People", values = activePeople$color)
