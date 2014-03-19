## Trying to do something with items

# Refresh data if older than 6 hours (only if "now" is defined)
if(length(grep("now", ls())) != 0){
  if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
    source("dataPrep.R");
  }
}

source("functions.R")



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

# Get list of num and new IDs actually existing in items dataset
existingNumIDs <- names(items)[grep("\\.[0-9]+$", names(items))]
existingNumIDs <- sub("craftItem.", "", existingNumIDs)
existingNumIDs <- sub("useItem.",   "", existingNumIDs)
existingNumIDs <- sub("mineBlock.", "", existingNumIDs)
existingNumIDs <- sub("breakItem.", "", existingNumIDs)
existingNumIDs <- unique(existingNumIDs)


# I honestly have no idea anymore what I did here, but it merges old and new item stats
for(i in 1:length(existingNumIDs)){
  if(length(grep("TRUE", existingNumIDs[i] == itemData$numID)) == 1){
    for(j in itemActions$id){
      if(ncol(items[names(items) == paste(j, ".", itemData$numID[i], sep="")]) == 1 & ncol(items[names(items) == paste(j, ".", itemData$ID[i], sep="")]) == 1){
      
      itemOldID <- items[names(items) == paste(j, ".", itemData$numID[i], sep="")]
      itemNewID <- items[names(items) == paste(j, ".", itemData$ID[i], sep="")]
      itemNewID <- itemNewID + itemOldID 
      
      }
    }
  }
}; rm(i, j, itemOldID, itemNewID, existingNumIDs)

# Exclude now unneeded old item ID columns
items <- items[ , !names(items) %in% names(items)[grep("\\.[0-9]+$", names(items))]]

# Let's just construct a dataframe of stats, their items and actions
existingIDs <- names(items)[grep("[^player]", names(items))]
itemStats <- data.frame(stat=as.character(existingIDs))
itemStats$item <-  sub("craftItem.","", existingIDs)
itemStats$item <- sub("useItem.","", itemStats$item)
itemStats$item <- sub("mineBlock.","", itemStats$item)
itemStats$item <- sub("breakItem.","", itemStats$item) 

for(i in 1:length(itemStats$item)){
  action <- sub(paste(".", itemStats$item[i], sep=""), "", itemStats$stat[i])
  itemStats$action[i] <- action
}; rm(i, action)

itemStats$action[itemStats$action == "mineBlock"] <- "mined"
itemStats$action[itemStats$action == "craftItem"] <- "crafted"
itemStats$action[itemStats$action == "useItem"] <- "used"
itemStats$action[itemStats$action == "breakItem"] <- "broken"

for(i in 1:length(itemStats$item)){
  name <- itemData$name[itemData$ID == itemStats$item[i]]
  itemStats$item[i] <- name[1]
}; rm(i, name)

itemStats$stat <- as.character(itemStats$stat)

####

for(i in 1:length(itemStats$stat)){

    filename <- paste("Plots/items/", itemStats$stat[i], ".png", sep="")
    title <- paste("Times item was ", itemStats$action[i], ": ", itemStats$item[i], sep="")
    
    p <- ggplot(data=items)
    p <- p + aes(x=reorder(player, items[, itemStats$stat[i]]), 
                 y=items[, itemStats$stat[i]])
    p <- p + barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
    p <- p + xLable + labs(y=paste("Times", itemStats$action[i]), title=title)
    ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(i, title, filename)

## Now for something completely different

for(i in 1:length(itemStats$stat)){
  
  stat <- itemStats$stat[i]
  itemStats$total[i] <- sum(items[, stat], na.rm=T)
  
  statPlayers <- items[items[, stat] == max(items[, stat]), c("player", stat)]
  itemStats$leadingPlayer[i] <- as.character(statPlayers[1,1])
  itemStats$playerMax[i] <- statPlayers[1,2]

}; rm(stat, statPlayers)

# Now to look at the different item actions
# itemStats[itemStats$action == "mined", c("leadingPlayer", "total", "item")]


# Write that stuff to disk. Apparently some columns are "lists", which write.csv hates
class(itemStats$leadingPlayer) <- "character"
class(itemStats$playerMax) <- "numeric"

write.csv(itemStats, "data/itemStats.csv")

