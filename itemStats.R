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
itemActions <- c("mineBlock.", "craftItem.", "useItem.", "breakItem.")

# Get list of IDs actually existing in items dataset
existingNumIDs <- names(items)[grep("\\.[0-9]+$", names(items))]
existingNumIDs <- sub("craftItem.","", existingNumIDs)
existingNumIDs <- sub("useItem.","", existingNumIDs)
existingNumIDs <- sub("mineBlock.","", existingNumIDs)
existingNumIDs <- sub("breakItem.","", existingNumIDs)
existingNumIDs <- unique(existingNumIDs)
existingIDs <- rep(NULL,length(existingNumIDs))
for(i in 1:length(existingNumIDs)){
  existingIDs[i] <- sub(existingNumIDs[i], itemData$ID[itemData$numID == existingNumIDs[i]], existingNumIDs[i])
}; rm(i)

# I honestly have no idea anymore what I did here
for(i in 1:length(existingNumIDs)){
  if(length(grep("TRUE", existingNumIDs[i] == itemData$numID)) == 1){
    for(k in itemActions){
      if(ncol(items[names(items) == paste(k,itemData$numID[i], sep="")]) == 1 & ncol(items[names(items) == paste(k,itemData$ID[i], sep="")]) == 1){
      
      itemOldID <- items[names(items) == paste(k,itemData$numID[i], sep="")]
      itemNewID <- items[names(items) == paste(k,itemData$ID[i], sep="")]
      itemNewID <- itemNewID + itemOldID 
      }
    }
  }
}; rm(i, k, itemOldID, itemNewID, existingNumIDs)

# Exclude now unneeded old item ID columns
items <- items[ , !names(items) %in% names(items)[grep("\\.[0-9]+$", names(items))]]