## Some miscellaneous plots

# Refresh data if older than 6 hours (only if "now" is defined)
if(length(grep("now", ls())) != 0){
    if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
      source("dataPrep.R");
    }
}

source("functions.R")

# Online hours relative to age on server
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playOneHour/serverAge), y=playOneHour/serverAge))
p <- p + geom_bar(colour="black", width=.7, stat="identity")
p <- p + labs(x="Player", y="Online Time (h) by Server Age (d)")
p <- p + ggtitle("Online Time by Server Age") + coord_flip()
p <- p + scale_fill_manual(name = "Join Status", values = statusColours)
ggsave(p, file="Plots/OnlineTimebyServerAge.png", height=plotHeight, width=plotWidth)

# Deaths per online hour 
p <- ggplot(playerstats, aes(x = playOneHour, y = deaths)) 
p <- p + geom_point(aes(colour=joinStatus), group=1) 
p <- p + labs(y="Deaths", x="Online time (hours (real time))") 
p <- p + ggtitle("Deaths vs. Online time") 
p <- p + playerTheme 
p <- p + scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(p, file="Plots/Deaths_OnlineTime.png", height=6, width=8)

# Damage taken vs deaths
p <- ggplot(playerstats, aes(y=deaths, x=(damageTaken/20000), label=player)) 
p <- p + geom_point(aes(colour=joinStatus, group=1)) 
p <- p + geom_text(size=2, hjust=-.2, vjust=.4) 
p <- p + labs(x="Damage Taken (Hearts in thousands)", y="Deaths") 
p <- p + ggtitle("Damage Taken vs. Deaths") 
p <- p + playerTheme 
p <- p + scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(p, file="Plots/Deaths_DamageTaken.png", height=6, width=8)

# Throw linear modeling at stuff
summary(lm(playerstats$deaths ~ playerstats$damageTaken))
cor(playerstats$deaths, playerstats$damageTaken)^2

# Mob kills vs play time
p <- ggplot(playerstats, aes(x=playOneHour, y=mobKills, label=player)) 
p <- p + geom_point(aes(colour=joinStatus, group=1)) 
p <- p + geom_text(size=2, hjust=-.2, vjust=.4) 
p <- p + labs(y="Mob kills (absolute)", x="Online time (hours (real time)))") 
p <- p + ggtitle("Mob kills vs. Online time") 
p <- p + playerTheme 
p <- p + scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(p, file="Plots/MobKills_OnlineTime.png", height=plotHeight, width=plotWidth)

# Distance walked per online time
p <- ggplot(playerstats, aes(x = playOneHour,  y = (walkOneCm/1000000), label=player)) 
p <- p + geom_point(aes(colour=joinStatus, group=1)) 
p <- p + ylab("Distance walked (km)") + xlab("Online time (hours (real time)))") 
p <- p + ggtitle("Distance walked vs. Online time") 
p <- p + playerTheme 
p <- p + scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(p, file="Plots/DistanceWalked_OnlineTime.png", height=6, width=8)

# Server growth
p <- ggplot(playerstats, aes(x = joinDate, y = numID, label = player)) 
p <- p + geom_point(aes(colour=joinStatus), stat="identity") 
p <- p + geom_text(size=2, vjust=-.2, hjust=-.2) 
p <- p + labs(y="Whitelist Count", x="Date") 
p <- p + ggtitle("Wurstmineberg Server Growth") 
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_colour_manual(name = "Join Status", 
                             values = statusColours) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("month"),
                          expand=c(.2,1));
ggsave(p, file="Plots/WhitelistGrowth.png", height=6, width=8)

# Play time vs server age
p <- ggplot(playerstats, aes(x = serverAge, y = playOneHour, label=player)) 
p <- p + geom_point(aes(colour=joinStatus, group=1)) 
p <- p + geom_text(size=2, hjust=.5, vjust=-1) 
p <- p + ylab("Time spent on server (hours)") + xlab("Age on server (days)") 
p <- p + ggtitle("Time spent on server vs. relative age on server") 
p <- p + playerTheme 
p <- p + scale_colour_manual(name = "Join Status", values = statusColours) 
ggsave(p, file="Plots/ServerAge_PlayTime.png", height=plotHeight, width=plotWidth)

# Distance Traveled Total
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled), y=(distanceTraveled/1000000))
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km)", title="Distance Traveled in Total")
ggsave(p, file="Plots/DistanceTraveled.png", height=plotHeight, width=plotWidth)

# Distance Traveled Total by Online Time
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled/playOneHour), y=(distanceTraveled/1000000)/playOneHour)
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km) per Hour (real time)", title="Total Distance Traveled by Online Time")
ggsave(p, file="Plots/DistanceTraveled_weighted.png", height=plotHeight, width=plotWidth)

rm(p)
