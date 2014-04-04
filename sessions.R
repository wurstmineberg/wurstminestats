# Lets look at sessions foo

refreshData()
if(!exists("activePeople")){
  activePeople <- getActivePeople()
}

sessions        <- getSessions()
playerSessions  <- getPlayerSessions(sessions)

# Ideally sessions should be separated per day, I guess?
playerSessions  <- splitSessionsByDay(playerSessions)

########################################
## We want play time per day, sooooo… ##
########################################

playedPerDay  <- ddply(playerSessions, .(date), summarize, timePlayed = sum(playedMinutes))

##########################################################
## We also want play time per day per person, so, well… ##
##########################################################

playedPerPerson <- getPlayedPerPerson(playerSessions)

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
