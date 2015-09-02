#### Datapreparations ####

## Startup
message("Making preparations…")
source("config/options.R")
source("functions.R")

# Checking if wurstmineR needs an update
message("Checking for wurstmineR")
# devtools::install_github("jemus42/wurstmineR", ref = "dev")
library("wurstmineR")

#Sys.setenv(TZ = "UTC") # Don't ask

# Get a close enough timestamp for the data age.
dataTime <- now(tzone = "UTC")
message("Timestamped data at ", now(tzone = "UTC"))

#### Get player stats from wurstmineberg API ####
message("Sucking data out of the API")

## Getting a people dataset from people.json ## (Also, deaths)
people        <- get_people(urls = urls, size = "full")
people_active <- get_people(urls = urls, size = "active")

stats <- get_stats(urls = urls, strings = wurstmineR::strings, people = people)

#---------------------------------------------------#
#### Enhancing playerstats with some useful shit ####
#---------------------------------------------------#

# Get total distance column by summing up all *OneCm rows per player ##
#generalstats$distanceTraveled <- rowSums(generalstats[, grep("OneCm", colnames(generalstats))])

#--------------------------------#
#### Handle per stat datasets ####
#--------------------------------#
# Get a dataframe of item stat ID, item name and action ##
itemStats   <- getItemStats(stats$items)
mobStats    <- getMobStats(stats$entities)

#-----------------------------------------------------#
#### Getting sessions from /sessions/overview.json ####
#-----------------------------------------------------#
message("Now the session data…")
sessions            <- get_sessions(urls = urls)
playerSessions      <- get_player_sessions(sessions, splitByDay = T)

# We want play time per day, sooooo… 
# playedPerDay     <- getPlayedPerX(playerSessions, people = people, sumBy = "day")
# 
# # We also want play time per day per person, so, well… ##
# playedPerPerson  <- getPlayedPerX(playerSessions, people = people, sumBy = "person")
# # Getting per weekday stuff
# playedPerWeekday <- getPlayedPerX(playerSessions, people = people, sumBy = "weekday")
# avgPerWeekday    <- mean(ddply(playedPerWeekday, .(wday), summarize, timePlayed=sum(timePlayed))$timePlayed)
# # Let's do a monthly one
# playedPerMonth   <- getPlayedPerX(playerSessions, people = people, sumBy = "month")
# avgPerMonth      <- mean(ddply(playedPerMonth, .(month), summarize, timePlayed=sum(timePlayed))$timePlayed)
# # Actually per person
# playtime.people  <- ddply(playedPerPerson, "person", summarize, timePlayed = sum(timePlayed))
# # Now per year
# playedPerYear    <- getPlayedPerX(playerSessions, people = people, sumBy = "year")
# # Now per months
# playedPerMonthYear        <- ddply(playerSessions, .(year, month, person), summarize, playedMinutes = sum(playedMinutes))
# playedPerMonthYear$person <- factor(playedPerMonthYear$person, levels = people$id, labels = people$name, ordered = T)

# Fix playerSession person names
#playerSessions$person <- factor(playerSessions$person, levels = people$id, ordered = T)

#### Add lastseen data
lastseen <- get_lastseen(urls, people)

#### Cache some objects ####
message("So far soo good, caching data…")
save.image(file = "cache/workspace.RData")

#save(playerstats,      file = paste0("cache/", "playerstats",    ".rda"))
#save(generalstats,     file = paste0("cache/", "generalstats",   ".rda"))
#save(items,            file = paste0("cache/", "items",          ".rda"))
#save(achievements,     file = paste0("cache/", "achievements",   ".rda"))
#save(entities,         file = paste0("cache/", "entities",       ".rda"))

#save(activePeople,     file = paste0("cache/", "activePeople",   ".rda"))
#save(playerSessions,   file = paste0("cache/", "playerSessions", ".rda"))
#save(itemStats,        file = paste0("cache/", "itemStats",      ".rda"))
#save(mobStats,         file = paste0("cache/", "mobStats",       ".rda"))
