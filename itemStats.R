## Trying to do something with items

refreshData()

# Get item stats
items <- fromJSON("http://api.wurstmineberg.de/server/playerstats/item.json")
items <- prettyShitUp(items)

# Get items.json from our website for names/ids
itemData        <- fromJSON("http://wurstmineberg.de/static/json/items.json")
itemData$numID  <- names(itemData$id)
itemData$ID     <- unlist(itemData$id, use.names=F)
itemData$ID     <- sub(":",".",itemData$ID)
itemData$name   <- unlist(itemData$name, use.names=F)
itemData        <- subset(itemData, select=c("numID","ID","name"))

# Get table of item actions and readable names
itemActions         <- data.frame(id = character(4), name = character(4))
itemActions$id      <- c("mineBlock", "craftItem", "useItem", "breakItem")
itemActions$name    <- c("mined", "crafted", "used", "broken")

# Merge old and new item stat IDs and whate have you
items <- mergeItemStats(items)

# Get a dataframe of item stat ID, item name and action
itemStats <- getItemStats()

items$player        <- activePeople$name
items$joinStatus    <- activePeople$joinStatus

#############################
## Plotting the whole mess ##
#############################

for(i in 1:length(itemStats$stat)){

    stat        <- itemStats$stat[i]
    action      <- itemStats$action[i]
    itemName    <- itemStats$item[i]
    filename    <- paste("Plots/items/", stat, ".png", sep="")
    title       <- paste("Times item was ", action, ": ", itemName, sep="")

    p <- ggplot(data=items)
    p <- p + aes(fill=joinStatus, x=reorder(player, items[, stat], mean, order=T), y=items[, stat])
    p <- p + barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
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
  itemsTop[action] <- paste(itemStatsPerAction$item, ": ", itemStatsPerAction$total, sep="")
}; rm(action, itemStatsPerAction)

# Write that stuff to disk. 
write.csv(itemStats, "data/itemStats.csv")
write.csv(itemsTop, "data/itemsTop.csv")
