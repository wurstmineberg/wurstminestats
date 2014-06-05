#-----------------------------------------------#
#### Meta stats (server stuff, death stats…) ####
#-----------------------------------------------#
message("Generating meta stats plots")
# Online hours relative to age on server
p <- ggplot(data  = playerstats)
p <- p + aes(fill = joinStatus, 
             x    = sortLevels(player, playOneHour/activePeople$serverAge), 
             y    = playOneHour/activePeople$serverAge)
p <- p + barChart + coord_flip() + statusFillScale + playerTheme
p <- p + labs(x = "Player", y = "Online Time (h) by Server Age (d)")
p <- p + ggtitle("Online Time by Server Age")
ggsave(p, file = "Plots/OnlineTimebyServerAge.png", height = plotHeight, width = plotWidth)

# Server growth
p <- ggplot(data = people) 
p <- p + aes(x   = joinDate, y = 1:nrow(people), label = name, colour = joinStatus)
p <- p + geom_point(stat = "identity") + geom_text(size = 3, hjust = -.2) 
p <- p + labs(y  = "Whitelist Count", x = "Date", title = "Wurstmineberg Server Growth") 
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + statusColourScale + playerTheme
p <- p + scale_x_datetime(labels = date_format("%Y %b"),
                          breaks = date_breaks("month"),
                          minor_breaks = date_breaks("week"),
                          expand = c(.2,1))
p <- p + scale_y_discrete(breaks = seq(0, nrow(activePeople), by = 2))
ggsave(p, file = "Plots/WhitelistGrowth.png", height = 6, width = 12)

### Death stats (latest) ###

p <- ggplot(data  = deaths)
p <- p + aes(fill = joinStatus, x = sortLevels(player, desc(daysSince)), y = daysSince)
p <- p + barChart + coord_flip() + statusFillScale
p <- p + xLable   + labs(y = "Days Since Death", title = "Days Since Players' Latest Death")
#p <- p + geom_text(aes(y = 2, label = cause.simple), size = 5, hjust = 0)
ggsave(p, file = "Plots/LatestDeaths.png", height = plotHeight, width = plotWidth)


#### Quick & dirty people color overview ####
ppl <- getActivePeople()
ppl$dummy <- 1
ppl$name <- factor(rev(ppl$name), levels = rev(ppl$name), ordered = TRUE)
p <- ggplot(data = ppl)
p <- p + aes(x = rev(name), y = rev(dummy), fill = name)
p <- p + geom_bar(stat = "identity", colour = "black") + coord_flip()
p <- p + geom_text(aes(x = rev(name), y = 0, label = ppl$color), hjust = -2, size = 4)
p <- p + geom_text(aes(x = rev(name), y = 0, label = ppl$color), hjust = -4, size = 4, colour = "white")
p <- p + scale_fill_manual(name = "Player", values = ppl$color, guide = F)
p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
               axis.ticks.x = element_blank())
p <- p + labs(title = "People Color Preview", x = "Person")

ggsave(p, file = "Plots/PeopleColors.png", width = 8, height = 7)
