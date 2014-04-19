## Datapreparations

# Startup
if(grepl("shiny$", getwd())){
  source("../options.R")
  source("../functions.R")
} else { 
  source("options.R")
  source("functions.R")
}

# Get a close enough timestamp for the data age. Reimport via as.POSIXct(x,origin="1970-01-01") should be sufficient, else handle as.numeric()
dataTime <- format(Sys.time(), "%s")

## Get player stats from wurstmineberg API ##
generalstats  <- fromJSON(getOption("url.stats.general"))
achievements  <- fromJSON(getOption("url.stats.achievements"))
entities      <- fromJSON(getOption("url.stats.entities"))
items         <- fromJSON(getOption("url.stats.items"))

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
generalstats  <- prettyShitUp(generalstats)
achievements  <- prettyShitUp(achievements)
entities      <- prettyShitUp(entities)
items         <- prettyShitUp(items)

#################################################
## Enhancing playerstats with some useful shit ##
#################################################

## Convert play time to real time hours as separate column ##
generalstats$playOneHour <- (generalstats$playOneMinute/20/60/60)

## Get total distance column by summing up all *OneCm rows per player ##
generalstats$distanceTraveled <- rowSums(generalstats[, grep("OneCm", colnames(generalstats))])

## Resort columns to get interesting stuff first. ##

playerstats   <- join(generalstats, achievements)
playerstats   <- join(playerstats, entities)

playerstats$joinStatus  <- activePeople$joinStatus
playerstats$joinDate    <- activePeople$joinDate

playerstats$timestamp <- dataTime

generalColumns <- c("timestamp", "player", "joinDate", "joinStatus", "leaveGame",
                    "deaths", "timeSinceDeath", "playerKills", "damageDealt", "damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")

playerstats <- playerstats[c(generalColumns, setdiff(names(playerstats), generalColumns))]
rm(generalColumns)


##########################
## Handle items dataset ##
##########################

## Get items.json from our website for names/ids ##
itemData        <- getItemData()

## Get table of item actions and readable names ##
itemActions     <- data.frame(id    = c("mineBlock", "craftItem"   , "useItem" , "breakItem"), 
                              name  = c("mined"    , "crafted"     , "used"    , "broken"))

## Merge old and new item stat IDs and whate have you ##
items     <- mergeItemStats(items, itemActions, itemData)

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
playedPerWeekday      <- ddply(playedPerPerson, .(wday, person), summarize, timePlayed=sum(timePlayed))
avgPerWeekday         <- mean(ddply(playedPerWeekday, .(wday), summarize, timePlayed=sum(timePlayed))$timePlayed)

# Let's do a monthly one
playedPerMonth       <- ddply(playedPerPerson, .(month, person), summarize, timePlayed=sum(timePlayed))
avgPerMonth          <- mean(ddply(playedPerMonth, .(month), summarize, timePlayed=sum(timePlayed))$timePlayed)

# Fix playerSession person names
for(i in playerSessions$person){
  playerSessions$person[playerSessions$person == i] <- activePeople$name[activePeople$id == i]
}; rm(i)

playerSessions$person <- as.factor(playerSessions$person)
playerSessions$person <- reorder(playerSessions$person, new.order=activePeople$name)

## Experimental perPerson dataframe for googleVis
perPerson <- data.frame(date = playedPerPerson$date)
for(i in unique(playedPerPerson$person)){
  tmp <- dplyr::filter(playedPerPerson, person == i)
  tmp <- select(tmp, date, timePlayed)
  tmp <- rename(tmp, c("timePlayed" = i))
  perPerson <- join(perPerson, tmp, type="full", match="all")
}

#### Getting some strings together ####
# Get general statistics from playerstats, define metadata (scale, units)
statNum             <- ncol(strings$general)
generalStats        <- data.frame(id=character(statNum), name=character(statNum), unit=character(statNum), scale=numeric(statNum))
generalStats$id     <- names(playerstats[names(strings$general)])
generalStats$name   <- unlist(strings$general[2,], use.names=F)
generalStats$unit   <- c("Animals", "km", "km", "km", "Hearts (thousands)", "Hearts (thousands)", "Deathcount", "km", "Items", "km", "Fish", "km", "km", "Jumps (thousands)", "Junk", "Quits", "km", "Mobs", "m", "Hours (real life)", "Kills", "km", "km", "Hours (real life)", "Treasure", "km")
generalStats$scale  <- c(1,         10^5, 10^5, 10^5, 10*2*10^3,            10*2*10^3,             1,           10^5,  1,      10^5,  1,     10^5,  10^5, 1000,                1,      1,      10^5,  1,    1000,  20*60*60,           1,       10^5, 10^5,  20*60*60,           1,          10^5)
rm(statNum)