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

# Add duration column
playerSessions$playedMinutes <- as.numeric(difftime(playerSessions$leaveTime, 
                                                    playerSessions$joinTime, unit="mins"))

# We want play time per day, sooooo…
playerSessions$date <- format(playerSessions$joinTime, "%F")
playerSessions$date <- as.POSIXct(playerSessions$date, origin="1970-01-01", tz="UTC")

loggedDays <- length(unique(playerSessions$date))
playedPerDay <- data.frame(date=unique(playerSessions$date),
                           timePlayed=numeric(loggedDays))

for(i in 1:loggedDays){
  playedPerDay$timePlayed[i] <- sum(playerSessions$playedMinutes[playerSessions$date == unique(playerSessions$date)[i]])
}; rm(i)

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
p <- p + labs(y="Played Hours", x="Date", title="Total Time Played per Day")
#print(p)
ggsave(p, file="Plots/playTime.png", height=6, width=12)

# Actually I want an area plot
playerSessions$person <- as.factor(playerSessions$person)

p <- ggplot(data=playerSessions, aes(x=joinTime, 
                                     y=playedMinutes/60,
                                     fill=person))
p <- p + geom_area(position="stack")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks=pretty_breaks())
p <- p + labs(y="Played Hours", x="Date", title="Total Time Played per Day")
#print(p)
#ggsave(p, file="Plots/playTime_area.png", height=6, width=12)