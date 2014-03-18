## Trying to do something with items

# Refresh data if older than 6 hours (only if "now" is defined)
if(length(grep("now", ls())) != 0){
  if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
    source("dataPrep.R");
  }
}

source("functions.R")



# Exclude old item IDs because I really don't want to deal with that
items <- items[grep("[^0-9]$", names(items))]

# Sorting stuff
craftItem <- items[grep("craftItem",names(items))]
mineBlock <- items[grep("mineBlock",names(items))]
useItem <- items[grep("useItem",names(items))]

# Get items.json from our website for names/ids
itemData <- fromJSON("http://wurstmineberg.de/static/json/items.json")

itemData$numID <- names(itemData$id)
itemData$ID <- unlist(itemData$id, use.names=F)
itemData$ID <- sub(":",".",itemData$ID)
itemData$name <- unlist(itemData$name, use.names=F)
# Get rid of everything else by subsetting
itemData <- subset(itemData, select=c("numID","ID","name"))
