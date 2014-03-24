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

# Now to look at the different item actions
itemsTop <- as.data.frame(matrix(0, 10, nrow(itemActions)))
names(itemsTop) <- itemActions$name

# subset for each action, get top 10 items for each action
for(action in itemActions$name){
  itemStatsPerAction <- itemStats[itemStats$action == action,]
  itemStatsPerAction <- head(arrange(itemStatsPerAction, desc(total)), 10)
  itemsTop[action]   <- paste(itemStatsPerAction$item, ": ", itemStatsPerAction$total, sep="")
}; rm(action, itemStatsPerAction)

# Write that stuff to disk. 
write.csv(itemStats,    "data/itemStats.csv")
write.csv(itemsTop,     "data/itemsTop.csv")
