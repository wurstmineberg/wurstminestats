## Trying to do something with items

refreshData()

# Exclude old item IDs because I really don't want to deal with that
#items <- items[grep("[^0-9]$", names(items))]

# Get items.json from our website for names/ids
itemData <- fromJSON("http://wurstmineberg.de/static/json/items.json")

itemData$numID <- names(itemData$id)
itemData$ID <- unlist(itemData$id, use.names=F)
itemData$ID <- sub(":",".",itemData$ID)
itemData$name <- unlist(itemData$name, use.names=F)
# Get rid of everything else by subsetting
itemData <- subset(itemData, select=c("numID","ID","name"))
itemActions <- c("mineBlock", "craftItem", "useItem", "breakItem")
itemActions <- data.frame(id = as.character(itemActions), 
                          name=c("mined", "crafted", "used", "broken"))
itemActions$id <- as.character(itemActions$id)
itemActions$name <- as.character(itemActions$name)

# Get list of num and new IDs actually existing in items dataset
existingNumIDs <- names(items)[grep("\\.[0-9]+$", names(items))]
for(action in itemActions$id){
  existingNumIDs <- sub(paste(action, ".", sep=""), "", existingNumIDs)
}; rm(action)

# I honestly have no idea anymore what I did here, but it merges old and new item stats
tempStats <- items[ , !names(items) %in% names(items)[grep("\\.[0-9]+$", names(items))]]
tempStats <- as.character(names(tempStats)[grep("[^player]", names(tempStats))])

for(i in 1:length(existingNumIDs)){
  for(action in itemActions$id){
    ID <- itemData$ID[itemData$numID == existingNumIDs[i]]
    
    if(paste(action, ".", ID, sep="") %in% tempStats){
      newIDstat <- items[, paste(action, ".", ID, sep="")]
      
      if(paste(action, ".", existingNumIDs[i], sep="") %in% names(items)){
        oldIDstat <- items[, paste(action, ".", existingNumIDs[i], sep="")]
        items[, paste(action, ".", ID, sep="")] <- newIDstat + oldIDstat
      }
    }
  }
}; rm(i, action, ID, newIDstat, oldIDstat, tempStats)

# Exclude now unneeded old item ID columns
items <- items[ , !names(items) %in% names(items)[grep("\\.[0-9]+$", names(items))]]

# Let's just construct a dataframe of stats, their items and actions
existingIDs <- names(items)[grep("[^player]", names(items))]
itemStats <- data.frame(stat=as.character(existingIDs))

# Setting items to the stat name minus the action portion
itemStats$item <- existingIDs
for(item in itemStats$item){
  for(id in itemActions$id){
    itemStats$item[itemStats$item == item] <- sub(paste(id, ".", sep=""), "", item)
  }
}; rm(item, id)

# Setting the action to the stat name minus the item portion
for(i in 1:length(itemStats$item)){
  action <- sub(paste(".", itemStats$item[i], sep=""), "", itemStats$stat[i])
  itemStats$action[i] <- action
}; rm(i, action)

# Substituting action with a more readable name
for(action in itemStats$action){
  for(i in 1:nrow(itemActions)){
    itemStats$action[itemStats$action == itemActions$id[i]] <- as.character(itemActions$name[i])
  }
}; rm(action, i)

for(i in 1:length(itemStats$item)){
  name <- itemData$name[itemData$ID == itemStats$item[i]]
  itemStats$item[i] <- name[1]
}; rm(i, name)

itemStats$stat <- as.character(itemStats$stat)

####
# Plotting the whole mess
for(i in 1:length(itemStats$stat)){

    filename <- paste("Plots/items/", itemStats$stat[i], ".png", sep="")
    title <- paste("Times item was ", itemStats$action[i], ": ", itemStats$item[i], sep="")
    
    p <- ggplot(data=items)
    p <- p + aes(x=reorder(player, items[, itemStats$stat[i]], mean, order=T), 
                 y=items[, itemStats$stat[i]])
    p <- p + barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
    p <- p + xLable + labs(y=paste("Times", itemStats$action[i]), title=title)
    ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(p, i, title, filename)

############################################
## Now for something completely different ##
############################################

# Add some more columns to itemStats for data's sake
for(i in 1:length(itemStats$stat)){
  
  stat <- itemStats$stat[i]
  itemStats$total[i] <- sum(items[, stat], na.rm=T)
  
  statPlayers <- items[items[, stat] == max(items[, stat]), c("player", stat)]
  itemStats$leadingPlayer[i] <- as.character(statPlayers[1,1])
  itemStats$playerMax[i] <- statPlayers[1,2]

}; rm(i, stat, statPlayers)

# Now to look at the different item actions
itemsTop <- as.data.frame(matrix(0, 10, nrow(itemActions)))
names(itemsTop) <- itemActions$name

# subset for "mined", substitue with loop over itemActions$name
for(action in itemActions$name){
  itemStatsPerAction <- itemStats[itemStats$action == action,]
  itemStatsPerAction <- head(arrange(itemStatsPerAction, desc(total)), 10)
  itemsTop[action] <- paste(itemStatsPerAction$item, ": ", itemStatsPerAction$total, sep="")
}; rm(action, itemStatsPerAction)

# Write that stuff to disk. Apparently some columns are "lists", which write.csv() hates
class(itemStats$leadingPlayer) <- "character"
class(itemStats$playerMax) <- "numeric"

write.csv(itemStats, "data/itemStats.csv")
write.csv(itemsTop, "data/itemsTop.csv")
