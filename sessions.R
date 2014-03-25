# Lets look at sessions foo

refreshData()

sessions        <- getSessions()
playerSessions  <- getPlayerSessions(sessions)

# Ideally sessions should be separated per day, I guess?
playerSessions  <- splitSessionsByDay(playerSessions)

# Add duration column
playerSessions$playedMinutes <- as.numeric(difftime(playerSessions$leaveTime, 
                                                    playerSessions$joinTime, unit="mins"))

########################################
## We want play time per day, sooooo… ##
########################################

loggedDays    <- unique(playerSessions$date)
playedPerDay  <- data.frame(date=unique(playerSessions$date),
                           timePlayed=numeric(length(loggedDays)))

for(i in 1:length(loggedDays)){
  playedPerDay$timePlayed[i] <- sum(playerSessions$playedMinutes[playerSessions$date == loggedDays[i]])
}; rm(i)

##########################################################
## We also want play time per day per person, so, well… ##
##########################################################

playedPerPerson <- data.frame(date=character(0),
                              timePlayed=numeric(0),
                              person=character(0))

for(i in 1:length(loggedDays)){
  daySet    <- playerSessions[playerSessions$date == loggedDays[i], ]
  dayPeople <- as.character(unique(daySet$person))
  
  for(person in dayPeople){
    sumPerson   <- sum(daySet$playedMinutes[daySet$person == person])
    row         <- data.frame(date=as.character(loggedDays[i]), 
                              timePlayed=sumPerson, 
                              person=as.character(person))

    playedPerPerson <- join(playedPerPerson, row, type="full")
  }
}; rm(i, person, sumPerson, row, daySet, dayPeople, loggedDays)

playedPerPerson$date    <- as.POSIXct(playedPerPerson$date, origin="1970-01-01", tz="UTC")
playedPerPerson         <- arrange(playedPerPerson, date, person)
playedPerPerson$person  <- as.character(playedPerPerson$person)

for(i in playedPerPerson$person){
  playedPerPerson$person[playedPerPerson$person == i] <- activePeople$name[activePeople$id == i]
}; rm(i)

playedPerPerson$person <- as.factor(playedPerPerson$person)
playedPerPerson$person <- reorder(playedPerPerson$person, new.order=activePeople$name[activePeople$name %in% unique(playedPerPerson$person)])

#########################
## Plotting the things ##
#########################

# Plotting playedPerDay

p <- ggplot(data=playedPerDay)
p <- p + aes(x=date, y=timePlayed/60)
p <- p + geom_area(alpha=0.7) + geom_point() + geom_path(alpha=.8)
p <- p + geom_hline(yintercept = mean(playedPerDay$timePlayed/60), alpha=.5)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks=pretty_breaks())
p <- p + labs(y="Played Hours", x="Day", title="Total Time Played per Day")
ggsave(p, file="Plots/playTime.png", height=6, width=12)

# Plotting playedPerPerson 
fillColours <- activePeople$color[activePeople$name %in% playedPerPerson$person]

p <- ggplot(data=playedPerPerson, aes(x=date, y=timePlayed/60, fill=person))
p <- p + geom_bar(position="stack", stat="identity", colour="black")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks=pretty_breaks()) + playerTheme
p <- p + labs(y="Played Hours", x="Day", title="Total Time Played per Day")
p <- p + scale_fill_manual(name="People", values=fillColours)
ggsave(p, file="Plots/playTime_perPerson.png", height=6, width=12)
rm(p, fillColours)

###################################################
## Write playerSessions to disk in case whatever ##
###################################################

write.csv(playerSessions, "data/playerSessions.csv")
