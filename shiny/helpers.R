##### Parts of dataPrep.R go here #####
# This is the first thing that gets loaded

# Loading all necessary libraries
suppressMessages(library(RCurl))
library(jsonlite)
library(ggplot2)
library(scales)       # For datetime scales on plots
library(grid)         # for unit() in ggplot theme() functions
#library(gridExtra)   # For annotations outside of plot ## TODO
library(plyr)         # To join() dataframes
library(RColorBrewer) # Because colours
library(httr)         # For direct web access stuff, apparently
suppressMessages(library(gdata)) 

source("functions.R")

activePeople    <- getActivePeople()
birthdays       <- serverBirthday(activePeople)

sessions        <- getSessions()
playerSessions  <- getPlayerSessions(sessions)
playerSessions  <- splitSessionsByDay(playerSessions)
playedPerDay    <- ddply(playerSessions, .(date), summarize, timePlayed = sum(playedMinutes))

playedPerPerson <- ddply(playerSessions, .(date, person), summarize, timePlayed = sum(playedMinutes))
playedPerPerson <- arrange(playedPerPerson, date, person)

for(i in playedPerPerson$person){
  playedPerPerson$person[playedPerPerson$person == i] <- activePeople$name[activePeople$id == i]
}; rm(i)

playedPerPerson$person <- as.factor(playedPerPerson$person)
playedPerPerson$person <- reorder(playedPerPerson$person, new.order=activePeople$name)
