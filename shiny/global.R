#### Datapreparations ####
# Shiny's version of dataPrep.R — It's like dataPrep.R, but shinier!
## Startup
source("../options.R")
source("../functions.R")

# Checking if wurstmineR needs an update
ver_loc    <- packageVersion("wurstmineR")
ver_github <- package_version(jsonlite::fromJSON("https://raw.githubusercontent.com/jemus42/wurstmineR/master/VERSION")$Version)
if (ver_loc == ver_github){
  message("wurstmineR is the current version, moving on.")
} else if (ver_loc < ver_github){
  message(paste("Remote version is", ver_github, "— Installing…"))
  library(devtools)
  install_github("jemus42/wurstmineR")
} else {
  message("How did you do that?")
}

library("wurstmineR")

Sys.setenv(TZ = "UTC") # Don't ask

# Get a close enough timestamp for the data age. Reimport via as.POSIXct(x,origin="1970-01-01") should be sufficient, else handle as.numeric()
dataTime <- format(Sys.time(), "%s")

#### Get player stats from wurstmineberg API ####
generalstats  <- jsonlite::fromJSON(getOption("url.stats.general"))
achievements  <- jsonlite::fromJSON(getOption("url.stats.achievements"))
entities      <- jsonlite::fromJSON(getOption("url.stats.entities"))
items         <- jsonlite::fromJSON(getOption("url.stats.items"))

#----------------------------------------------------------------------------------#
#### This is where imported datasets get cleaned up so we can actually use them ####
#----------------------------------------------------------------------------------#

## Getting a people dataset from people.json ## (Also, deaths)
activePeople  <- getActivePeople()
birthdays     <- serverBirthday(activePeople)
deaths        <- getLatestDeaths()

## Reformat stat datasets ##
generalstats  <- stats2df(generalstats)
achievements  <- stats2df(achievements, type = "achievements")
entities      <- stats2df(entities)
items         <- stats2df(items)
## Merge old and new item stat IDs and whate have you ##
items         <- mergeItemStats(items, strings.items)

#---------------------------------------------------#
#### Enhancing playerstats with some useful shit ####
#---------------------------------------------------#

## Convert play time to real time hours as separate column ##
generalstats$playOneHour      <- (generalstats$playOneMinute/20/60/60)
## Get total distance column by summing up all *OneCm rows per player ##
generalstats$distanceTraveled <- rowSums(generalstats[, grep("OneCm", colnames(generalstats))])
## Resort columns to get interesting stuff first. ##
playerstats             <- join(generalstats, achievements)
playerstats             <- join(playerstats, entities)
playerstats             <- join(playerstats, items, type="full")

## Append other useful meta info 
playerstats$joinStatus  <- activePeople$joinStatus
playerstats$joinDate    <- activePeople$joinDate
playerstats$player_id   <- activePeople$id
playerstats$timestamp   <- dataTime
## Reordering columns
generalColumns <- c("timestamp", "player_id" , "player", "joinDate", "joinStatus", "leaveGame",
                    "deaths", "timeSinceDeath", "playerKills", "damageDealt", "damageTaken", "playOneMinute", 
                    "playOneHour", "jump", "animalsBred", "mobKills")
playerstats    <- playerstats[c(generalColumns, setdiff(names(playerstats), generalColumns))]
rm(generalColumns)

#--------------------------------#
#### Handle per stat datasets ####
#--------------------------------#

## Get a dataframe of item stat ID, item name and action ##
itemStats   <- getItemStats(items)
mobStats    <- getMobStats(entities)
#-----------------------------------------------------#
#### Getting sessions from /sessions/overview.json ####
#-----------------------------------------------------#

sessions         <- getSessions()
playerSessions   <- getPlayerSessions(sessions,   splitByDay = T)

## We want play time per day, sooooo… 
playedPerDay     <- getPlayedPerX(playerSessions, sumBy = "day")

# We also want play time per day per person, so, well… ##
playedPerPerson  <- getPlayedPerX(playerSessions, sumBy = "person")
# Getting per weekday stuff
playedPerWeekday <- getPlayedPerX(playerSessions, sumBy = "weekday")
avgPerWeekday    <- mean(ddply(playedPerWeekday, .(wday), summarize, timePlayed=sum(timePlayed))$timePlayed)
# Let's do a monthly one
playedPerMonth   <- getPlayedPerX(playerSessions, sumBy = "month")
avgPerMonth      <- mean(ddply(playedPerMonth, .(month), summarize, timePlayed=sum(timePlayed))$timePlayed)
# Actually per person
playtime.people  <- ddply(playedPerPerson, "person", summarize, timePlayed=sum(timePlayed))

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