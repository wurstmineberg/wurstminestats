## Some miscellaneous plots

refreshData()

## Get a vector of the age gaps starting from player[1]
inviteGaps <- c(0,
                round(
                      as.numeric(
                                difftime(
                                          playerstats$joinDate[2:nrow(playerstats)], 
                                          playerstats$joinDate[1:(nrow(playerstats)-1)], 
                                          units="days")
                                )
                      )
                )

# current server age total
wurstminebergAge <- round(as.numeric(difftime(Sys.time(),
                        playerstats$joinDate[1], 
                        units ="days")))

# Online hours relative to age on server
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,playOneHour/serverAge, mean, order=T), y=playOneHour/serverAge)
p <- p + barChart + coord_flip() + legendTitle
p <- p + labs(x="Player", y="Online Time (h) by Server Age (d)")
p <- p + ggtitle("Online Time by Server Age")
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
ggsave(p, file="Plots/WhitelistGrowth.png", height=6, width=12)

##################
### Death stats ##
##################
deaths <- getDeathStats()

p <- ggplot(data=deaths)
p <- p + aes(fill=joinStatus, x=reorder(player, daysSince, mean, order=T), y=daysSince)
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Days Since Death", title="Days Since Players' Latest Death")
ggsave(p, file="Plots/LatestDeaths.png", height=plotHeight, width=plotWidth)
rm(p)
