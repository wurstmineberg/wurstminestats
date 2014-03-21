# Lets look at sessions foo

# Refresh data if older than 6 hours (only if "now" is defined)
if(length(grep("now", ls())) != 0){
  if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
    source("dataPrep.R");
  }
}

source("functions.R")

sessions <- fromJSON("http://api.wurstmineberg.de/server/sessions/overview.json")
sessions <- as.data.frame(sessions)

sessions$uptimes.startTime <- as.POSIXct(sessions$uptimes.startTime, tz="UTC")
sessions$uptimes.endTime <- as.POSIXct(sessions$uptimes.endTime, tz="UTC")

# Initialize an empty data frame for player sessions and name
playerSessions <- data.frame(minecraftNick = character(0),
                             joinTime = character(0),
                             leaveTime = character(0),
                             person = character(0))

# Fill playerSessions with data from sessions$uptimes.sessions in an ugly way because fuck JSON handling in R
numSessions <- length(sessions$uptimes.sessions)

for(i in 1:(numSessions-2)){
  temp1 <- as.data.frame(sessions$uptimes.sessions[i])
  temp2 <- as.data.frame(sessions$uptimes.sessions[i+1])
  
  if(NA %in% temp1$leaveTime){
    temp1$leaveTime[is.na(temp1$leaveTime)] <- as.character(sessions$uptimes.endTime[i])
  }
  if(NA %in% temp2$leaveTime){
    temp2$leaveTime[is.na(temp2$leaveTime)] <- as.character(sessions$uptimes.endTime[i+1])
  }
  
  tempMerge <- join(temp1, temp2, type="full")
  rm(temp1, temp2)
  playerSessions <- join(tempMerge, playerSessions, type="full")
}; rm(i, tempMerge)

playerSessions <- arrange(playerSessions, joinTime, leaveTime)

# Now we reformat shit
playerSessions$joinTime <- as.POSIXct(playerSessions$joinTime, tz="UTC")
playerSessions$leaveTime <- as.POSIXct(playerSessions$leaveTime, tz="UTC")

# Fixing remains of the last fix
for(i in 1:(nrow(playerSessions)-1)){
  if(playerSessions$minecraftNick[i] == playerSessions$minecraftNick[i+1]){
    if(playerSessions$leaveTime[i] > playerSessions$joinTime[i+1]){
      playerSessions$leaveTime[i] <- playerSessions$joinTime[i+1]
    }
  }
}; rm(i)

## Ideally sessions should be separated per day, I guess?
# Add join/leave time as date only for simplicity in the long run
playerSessions$joinDate <- format(playerSessions$joinTime, "%F")
playerSessions$joinDate <- as.POSIXct(playerSessions$joinDate, origin="1970-01-01", tz="UTC")
playerSessions$leaveDate <- format(playerSessions$leaveTime, "%F")
playerSessions$leaveDate <- as.POSIXct(playerSessions$leaveDate, origin="1970-01-01", tz="UTC")

overlaps <- playerSessions[playerSessions$leaveDate > playerSessions$joinDate, ]
noOverlaps <- playerSessions[playerSessions$leaveDate == playerSessions$joinDate, ]
overlapsNum <- nrow(overlaps)

i <- 1
for(i in 1:overlapsNum){
  temp1 <- overlaps[1,]
  temp1[1, ] <- overlaps[i, ]
  temp1[2, ] <- overlaps[i, ]
  
  temp1$leaveTime[1] <- overlaps$leaveDate[i]
  temp1$joinTime[2] <- overlaps$leaveDate[i]
  
  noOverlaps <- join(noOverlaps, temp1, type="full")
  rm(temp1)
}
playerSessions <- arrange(noOverlaps[names(noOverlaps) != c("joinDate", "leaveDate")], joinTime, person)

rm(overlaps, noOverlaps, overlapsNum)

# Add duration column
playerSessions$playedMinutes <- as.numeric(difftime(playerSessions$leaveTime, 
                                                    playerSessions$joinTime, unit="mins"))

# We want play time per day, sooooo…
playerSessions$date <- format(playerSessions$joinTime, "%F")
playerSessions$date <- as.POSIXct(playerSessions$date, origin="1970-01-01", tz="UTC")

loggedDays <- unique(playerSessions$date)
playedPerDay <- data.frame(date=unique(playerSessions$date),
                           timePlayed=numeric(length(loggedDays)))

for(i in 1:length(loggedDays)){
  playedPerDay$timePlayed[i] <- sum(playerSessions$playedMinutes[playerSessions$date == loggedDays[i]])
}; rm(i)

# We also want play time per day per person, so, well…
playedPerPerson <- data.frame(date=character(0),
                              timePlayed=numeric(0), person=character(0))

for(i in 1:length(loggedDays)){
  daySet <- playerSessions[playerSessions$date == loggedDays[i],]
  dayPeople <- as.character(unique(daySet$person))
  
  for(person in dayPeople){
    sumPerson <- sum(daySet$playedMinutes[daySet$person == person])
    row <- data.frame(date=as.character(loggedDays[i]), timePlayed=sumPerson, person=as.character(person))
    playedPerPerson <- join(playedPerPerson, row, type="full")
  }
}; rm(i, person, sumPerson, row, daySet, dayPeople)

playedPerPerson$date <- as.POSIXct(playedPerPerson$date, origin="1970-01-01", tz="UTC")
playedPerPerson <- arrange(playedPerPerson, date, person)
playedPerPerson$person <- as.factor(playedPerPerson$person)

# Plotting the things
p <- ggplot(data=playedPerDay)
p <- p + aes(x=date, y=timePlayed/60)
p <- p + geom_area(alpha=0.7)
p <- p + geom_point() + geom_path(alpha=.8)
p <- p + geom_smooth(method="loess", se=F)
p <- p + geom_hline(yintercept = mean(playedPerDay$timePlayed/60), alpha=.5)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks=pretty_breaks())
p <- p + labs(y="Played Hours", x="Day", title="Total Time Played per Day")
ggsave(p, file="Plots/playTime.png", height=6, width=12)

# Actually I want an area plot, but barcharts are fine, too
p <- ggplot(data=playedPerPerson, aes(x=date, y=timePlayed/60, fill=person))
p <- p + geom_bar(position="stack", stat="identity", colour="black")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks=pretty_breaks()) + playerTheme
p <- p + labs(y="Played Hours", x="Day", title="Total Time Played per Day")
ggsave(p, file="Plots/playTime_perPerson.png", height=6, width=12)
rm(p)

# Write playerSessions data for whatever reason
write.csv(playerSessions, "data/playerSessions.csv")
