## Some miscellaneous plots

refreshData()

# Online hours relative to age on server
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playOneHour/serverAge, mean, order=T), y=playOneHour/serverAge))
p <- p + geom_bar(colour="black", width=.7, stat="identity")
p <- p + labs(x="Player", y="Online Time (h) by Server Age (d)")
p <- p + ggtitle("Online Time by Server Age") + coord_flip()
p <- p + scale_fill_manual(name = "Join Status", values = statusColours)
ggsave(p, file="Plots/OnlineTimebyServerAge.png", height=plotHeight, width=plotWidth)

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

# Distance Traveled Total
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled, mean, order=T), y=(distanceTraveled/1000000))
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km)", title="Distance Traveled in Total")
ggsave(p, file="Plots/DistanceTraveled.png", height=plotHeight, width=plotWidth)

# Distance Traveled Total by Online Time
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled/playOneHour, mean, order=T), y=(distanceTraveled/1000000)/playOneHour)
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km) per Hour (real time)", title="Total Distance Traveled by Online Time")
ggsave(p, file="Plots/DistanceTraveled_weighted.png", height=plotHeight, width=plotWidth)

##################
### Death stats ##
##################

deaths <- data.frame(player = names(latestdeaths$deaths[,1]))
deaths$timestamp <- unlist(latestdeaths$deaths[,1], use.names=F)
deaths$cause <- unlist(latestdeaths$deaths[,2], use.names=F)
deaths$timestamp <- as.POSIXct(deaths$timestamp, tz="UTC")
deaths$daysSince <- as.numeric(round(difftime(Sys.time(),deaths$timestamp, units="days")))

p <- ggplot(data=deaths)
p <- p + aes(x=reorder(player,daysSince, mean, order=T), y=daysSince)
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Days Since Death", title="Days Since Players' Latest Death")
ggsave(p, file="Plots/LatestDeaths.png", height=plotHeight, width=plotWidth)
rm(p)
