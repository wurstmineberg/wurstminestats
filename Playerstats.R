## Some miscellaneous plots

# Refresh data if older than 6 hours
if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
  source("dataPrep.R");
}

source("functions.R")

# Online hours relative to age on server
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playOneHour/serverAge), y=playOneHour/serverAge)) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Online Time (h) by Server Age (d)") +
  ggtitle("Online Time by Server Age") + coord_flip() +
  scale_fill_manual(name = "Join Status", values = statusColours)
ggsave(file="Plots/OnlineTimebyServerAge.png", height=plotHeight, width=plotWidth)

# Deaths per online hour 
ggplot(playerstats, aes(x = playOneHour, y = deaths)) + 
  geom_point(aes(colour=joinStatus), group=1) + 
  ylab("Deaths") + xlab("Online time (hours (real time))") +
  ggtitle("Deaths vs. Online time") +
  playerTheme +
  scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(file="Plots/Deaths_OnlineTime.png", height=6, width=8)

# Damage taken vs deaths
ggplot(playerstats, aes(y=deaths, x=(damageTaken/2000), label=player)) + 
  geom_point(aes(colour=joinStatus, group=1)) + 
  geom_text(size=2, hjust=-.2, vjust=.4) +
  xlab("Damage Taken (Hearts in thousands)") + ylab("Deaths") +
  ggtitle("Damage Taken vs. Deaths") +
  playerTheme +
  scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(file="Plots/Deaths_DamageTaken.png", height=6, width=8)

# Throw linear modeling at stuff
summary(lm(playerstats$deaths ~ playerstats$damageTaken))
cor(playerstats$deaths, playerstats$damageTaken)^2

# Mob kills vs play time
ggplot(playerstats, aes(x=playOneHour, y=mobKills, label=player)) + 
  geom_point(aes(colour=joinStatus, group=1)) + 
  geom_text(size=2, hjust=-.2, vjust=.4) +
  ylab("Mob kills (absolute)") + xlab("Online time (hours (real time)))") +
  ggtitle("Mob kills vs. Online time") +
  playerTheme +
  scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(file="Plots/MobKills_OnlineTime.png", height=plotHeight, width=plotWidth)

# Distance walked per online time
ggplot(playerstats, aes(x = playOneHour,  y = (walkOneCm/1000000), label=player)) + 
  geom_point(aes(colour=joinStatus, group=1)) + 
  ylab("Distance walked (km)") + xlab("Online time (hours (real time)))") +
  ggtitle("Distance walked vs. Online time") +
  playerTheme +
  scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(file="Plots/DistanceWalked_OnlineTime.png", height=6, width=8)

# Server growth
p <- ggplot(playerstats, aes(x = joinDate, y = numID, label = player)) + 
  geom_point(aes(colour=joinStatus), stat="identity") + 
  geom_text(size=2, vjust=-.2, hjust=-.2) +
  ylab("Whitelist Count") + xlab("Date") +
  ggtitle("Wurstmineberg Server Growth") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(name = "Join Status", 
                    values = statusColours) +
  scale_x_datetime(labels = date_format("%y-%m-%d"),
                   breaks = date_breaks("month"),
                   expand=c(.2,1));
ggsave(p, file="Plots/WhitelistGrowth.png", height=6, width=8)

# Play time vs server age
ggplot(playerstats, aes(x = serverAge, y = playOneHour, label=player)) +
  geom_point(aes(colour=joinStatus, group=1)) + 
  geom_text(size=2, hjust=.5, vjust=-1) +
  ylab("Time spent on server (hours)") + xlab("Age on server (days)") +
  ggtitle("Time spent on server vs. relative age on server") +
  playerTheme +
  scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(file="Plots/ServerAge_PlayTime.png", height=plotHeight, width=plotWidth)

# Distance Traveled Total
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,distanceTraveled), y=(distanceTraveled/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Traveled in Total")
ggsave(file="Plots/DistanceTraveled.png", height=plotHeight, width=plotWidth)
