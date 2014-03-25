## Trying to do something with items

refreshData()

######################################
## Plotting item stats as they come ##
######################################

for(i in 1:length(itemStats$stat)){

    stat        <- itemStats$stat[i]
    action      <- itemStats$action[i]
    itemName    <- itemStats$item[i]
    filename    <- paste("Plots/items/", action, "/", stat, ".png", sep="")
    title       <- paste("Times item was ", action, ": ", itemName, sep="")

    p <- ggplot(data=playerstats)
    p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, stat], mean, order=T), y=playerstats[, stat])
    p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
    p <- p + labs(x="Player", y=paste("Times", action), title=title)
    ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(p, i, title, filename, stat, action)

############################################
## Now for something completely different ##
############################################

## Now to look at the different item actions ##

# subset for each action, get top 20 items for each action, and plot them
for(action in itemActions$name){
  itemStatsPerAction <- itemStats[itemStats$action == action,]
  itemStatsPerAction <- head(arrange(itemStatsPerAction, desc(total)), 20)

  p <- ggplot(data=itemStatsPerAction)
  p <- p + aes(x=sortLevels(item, total), y=total)
  p <- p + barChart + coord_flip()
  p <- p + labs(x="Item", y=paste("Times", action, sep=" "))
  p <- p + ggtitle(paste("Top", action, "items", sep=" "))
  ggsave(plot=p, file=paste("Plots/items/top_", action, ".png", sep=""), height=plotHeight, width=plotWidth)
  
}; rm(action, itemStatsPerAction)

# Write that stuff to disk. 
write.csv(itemStats,    "data/itemStats.csv")
