#### Datapreparations ####

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

#### Get strings for some… strings. (Mob IDs, display names, biomes, achievements…) ####
# Note: strings.biomes is required for proper achievements dataset handling
strings.general         <- getStrings(category = "general")
strings.mobs            <- getStrings(category = "mobs")
strings.achievements    <- getStrings(category = "achievements")
strings.items           <- getStrings(category = "items")
strings.biomes          <- getStrings(category = "biomes")

#### Get player stats from wurstmineberg API ####
generalstats            <- jsonlite::fromJSON(getOption("url.stats.general"))
achievements            <- jsonlite::fromJSON(getOption("url.stats.achievements"))
entities                <- jsonlite::fromJSON(getOption("url.stats.entities"))
items                   <- jsonlite::fromJSON(getOption("url.stats.items"))

#----------------------------------------------------------------------------------#
#### This is where imported datasets get cleaned up so we can actually use them ####
#----------------------------------------------------------------------------------#

## Getting a people dataset from people.json ## (Also, deaths)
activePeople  <- getActivePeople()
birthdays     <- serverBirthday(activePeople)
deaths        <- getDeathStats()
mobStats      <- getMobStats()

## Reformat stat datasets ##
generalstats  <- stats2df(generalstats)
achievements  <- stats2df(achievements, type = "achievements")
entities      <- stats2df(entities)
items         <- stats2df(items)

#---------------------------------------------------#
#### Enhancing playerstats with some useful shit ####
#---------------------------------------------------#

## Convert play time to real time hours as separate column ##
generalstats$playOneHour <- (generalstats$playOneMinute/20/60/60)
## Get total distance column by summing up all *OneCm rows per player ##
generalstats$distanceTraveled <- rowSums(generalstats[, grep("OneCm", colnames(generalstats))])
## Resort columns to get interesting stuff first. ##
playerstats             <- join(generalstats, achievements)
playerstats             <- join(playerstats, entities)
## Append other useful meta info 
playerstats$joinStatus  <- activePeople$joinStatus
playerstats$joinDate    <- activePeople$joinDate
playerstats$player_id   <- activePeople$id
playerstats$timestamp   <- dataTime
## Reordering columns
generalColumns <- c("timestamp", "player_id" , "player", "joinDate", "joinStatus", "leaveGame",
                    "deaths", "timeSinceDeath", "playerKills", "damageDealt", "damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")
playerstats <- playerstats[c(generalColumns, setdiff(names(playerstats), generalColumns))]
rm(generalColumns)

#----------------------------#
#### Handle items dataset ####
#----------------------------#

## Get table of item actions and readable names ##
itemActions     <- data.frame(id    = c("mineBlock", "craftItem"   , "useItem" , "breakItem"), 
                              name  = c("mined"    , "crafted"     , "used"    , "broken"))

## Merge old and new item stat IDs and whate have you ##
items           <- mergeItemStats(items, itemActions, strings.items)

## Get a dataframe of item stat ID, item name and action ##
itemStats       <- getItemStats(items, itemActions, strings.items)

## Finally join the remaining datasets ## 
playerstats     <- join(playerstats, items, type="full")

#-----------------------------------------------------#
#### Getting sessions from /sessions/overview.json ####
#-----------------------------------------------------#

sessions        <- getSessions()
playerSessions  <- getPlayerSessions(sessions)
## Ideally sessions should be separated per day, I guess?
playerSessions  <- splitSessionsByDay(playerSessions)

## We want play time per day, sooooo… 
playedPerDay  <- ddply(playerSessions, .(date, wday), summarize, timePlayed = sum(playedMinutes))
# Fix for missing days (ugly, sry.)
Sys.setenv(TZ = "UTC") # Don't ask
temp            <- data.frame(date = seq.Date(as.Date(playedPerDay$date[1]), as.Date(playedPerDay$date[nrow(playedPerDay)]), by="day"))
temp$wday       <- factor(wday(temp$date, T, F), levels=levels(playedPerDay$wday))
temp$timePlayed <- 0
temp$date       <- as.POSIXct(temp$date, tz = "UTC")
temp$timePlayed[temp$date %in% playedPerDay$date] <- playedPerDay$timePlayed
playedPerDay    <- temp
rm(tmp)

# We also want play time per day per person, so, well… ##
playedPerPerson <- getPlayedPerPerson(playerSessions)
# Getting per weekday stuff
playedPerWeekday      <- playedPerPerson
playedPerWeekday      <- ddply(playedPerPerson, .(wday, person), summarize, timePlayed=sum(timePlayed))
avgPerWeekday         <- mean(ddply(playedPerWeekday, .(wday), summarize, timePlayed=sum(timePlayed))$timePlayed)
# Let's do a monthly one
playedPerMonth       <- ddply(playedPerPerson, .(month, person), summarize, timePlayed=sum(timePlayed))
avgPerMonth          <- mean(ddply(playedPerMonth, .(month), summarize, timePlayed=sum(timePlayed))$timePlayed)
# Actually per person
playtime.people <- ddply(playedPerPerson, "person", summarize, timePlayed=sum(timePlayed))

# Fix playerSession person names
for(i in playerSessions$person){
  playerSessions$person[playerSessions$person == i] <- activePeople$name[activePeople$id == i]
}; rm(i)

playerSessions$person <- factor(playerSessions$person, levels=activePeople$name, ordered = T)

## Experimental perPerson dataframe for googleVis
perPerson <- data.frame(date = playedPerPerson$date)
for(i in unique(playedPerPerson$person)){
  tmp <- dplyr::filter(playedPerPerson, person == i)
  tmp <- dplyr::select(tmp, date, timePlayed)
  tmp <- rename(tmp, c("timePlayed" = i))
  perPerson <- join(perPerson, tmp, type="full", match="all")
}; rm(i, tmp)

#### Cache some objects ####
saveRDS(playerstats,          file = paste0("cache/", "playerstats", ".rds"))
saveRDS(activePeople,         file = paste0("cache/", "activePeople", ".rds"))
saveRDS(playerSessions,       file = paste0("cache/", "playerSessions", ".rds"))
saveRDS(itemStats,            file = paste0("cache/", "itemStats", ".rds"))
saveRDS(strings.general,      file = paste0("cache/", "strings.general", ".rds"))
saveRDS(strings.achievements, file = paste0("cache/", "strings.achievements", ".rds"))
saveRDS(strings.mobs,         file = paste0("cache/", "strings.mobs", ".rds"))
saveRDS(strings.biomes,       file = paste0("cache/", "strings.biomes", ".rds"))
saveRDS(strings.items,        file = paste0("cache/", "strings.items", ".rds"))

