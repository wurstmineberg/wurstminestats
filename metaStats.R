## Some miscellaneous plots

refreshData()

## Get a vector of the age gaps starting from player[1]
inviteGaps <- c(0, round(as.numeric(difftime(
                                          activePeople$joinDate[2:nrow(activePeople)], 
                                          activePeople$joinDate[1:(nrow(activePeople)-1)], 
                                          units="days"))))

# current server age total
wurstminebergAge <- round(as.numeric(difftime(Sys.time(),
                                            activePeople$joinDate[1], 
                                            units ="days")))

serverBirthday(activePeople)

# Online hours relative to age on server
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=sortLevels(player, playOneHour/activePeople$serverAge), 
                              y=playOneHour/activePeople$serverAge)
p <- p + barChart + coord_flip() + statusFillScale + playerTheme
p <- p + labs(x="Player", y="Online Time (h) by Server Age (d)")
p <- p + ggtitle("Online Time by Server Age")
ggsave(p, file="Plots/OnlineTimebyServerAge.png", height=plotHeight, width=plotWidth)

# Server growth
p <- ggplot(data=activePeople) 
p <- p + aes(x = joinDate, y = 1:nrow(activePeople), label = name, colour=joinStatus)
p <- p + geom_point(stat="identity") + geom_text(size=3, hjust=-.2) 
p <- p + labs(y = "Whitelist Count", x = "Date", title = "Wurstmineberg Server Growth") 
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + statusColourScale + playerTheme
p <- p + scale_x_datetime(labels = date_format("%Y %b"),
                          breaks = date_breaks("month"),
                          minor_breaks = date_breaks("week"),
                          expand = c(.2,1))
p <- p + scale_y_discrete(breaks=pretty_breaks())
ggsave(p, file="Plots/WhitelistGrowth.png", height=6, width=12)

##################
### Death stats ##
##################
deaths <- getDeathStats()

p <- ggplot(data=deaths)
p <- p + aes(fill=joinStatus, x=reorder(player, desc(daysSince), mean, order=T), y=daysSince)
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable + labs(y="Days Since Death", title="Days Since Players' Latest Death")
ggsave(p, file="Plots/LatestDeaths.png", height=plotHeight, width=plotWidth)
rm(p)
