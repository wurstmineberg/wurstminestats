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

#### Getting sessions stuff
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

######################################################
## Define some variables for plot layout and labels ##
######################################################

playerTheme <- theme(legend.position  = "right",
                    legend.key.size   = unit(.4, "cm"),
                    legend.text       = element_text(size = rel(.8)))

# Define default barchart settings
barChart  <- geom_bar(colour="black", width=.7, stat="identity")

# Define colour scale to keep status/people colours static
statusColours         <- brewer.pal(9,"Set1")
statusFillScale       <- scale_fill_manual(   name = "Join Status", values = statusColours)
statusColourScale     <- scale_colour_manual( name = "Join Status", values = statusColours)

#activePeople          <- fixPeopleColors(activePeople, 0.95)
legendPeople          <- scale_fill_manual(name = "People", values = activePeople$color)
