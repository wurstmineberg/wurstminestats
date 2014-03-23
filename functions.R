## Defining some functions and variables used by other scripts

## Sooner or later, I want a giant log file.
# Call this after periodic data refreshes

writePlayerstatsLog <- function(){

  playerstatsOld <- read.csv(file="data/playerstats.csv", row.names=1)

  # Only append saved date if the new data is at least 6h newer then the last saved data
  nowDate <- as.POSIXct(as.numeric(now), origin="1970-01-01")
  lastSavedDate <- as.POSIXct(max(as.numeric(playerstatsOld$timestamp)), origin="1970-01-01")

  if(as.numeric(difftime(nowDate, lastSavedDate, units ="hours")) > 6){

    # Join new data with saved data and order by joinDate, player, then timestamp
    playerstatsFull <- join(playerstats,playerstatsOld, type="full", match="all")
    playerstatsFull <- arrange(playerstatsFull, as.Date(joinDate), player, timestamp)

    # Write dataset to file for ze easy access
    write.csv(playerstatsFull, "data/playerstats.csv")
    rm(playerstatsFull)
  };

  rm(playerstatsOld, nowDate)
}

# Refresh data if older than 6 hours (only if "now" is defined)
refreshData <- function(force=FALSE){
   if("now" %in% ls()){
    if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
      source("dataPrep.R");
    } else if (force == TRUE){
        source("dataPrep.R")
    }
  }
}

# Define function to transform JSON from playerstats API to nice dataframe
prettyShitUp <- function(data){
    ## Removing "stat." and "achievement." prefixes from columns
    names(data) <- sub("stat.","",names(data))
    names(data) <- sub("achievement.","",names(data))
    # Fix for exploreAllBiomes shit
    if("exploreAllBiomes" %in% names(data)){
        data <- subset(data, select=-exploreAllBiomes)
    }
    
    data[data == "NULL"] <- "0"

    ## Extract player names to separate variable
    # Note: These are the minecraft usernames
    playerTemp <- names(data[,1])

    ## Getting rid of the nested list stuff
    # This took me so long, please don't even ask me about it.
    for(i in (1:(ncol(data)))) {
      data[i] <- unlist(data[i], use.names=F)
    }; rm(i);

    ## Getting rid of NAs and assuming 0
    data[data == NA] <- 0

    ## Numericizzle
    data <- as.data.frame(mapply(as.numeric,data))

    ## Sorting according to people.json, requires activePoeple to be generated from people.json
    data$player <- playerTemp
    data <- data[match(activePeople$mc, data$player), ]

    rm(playerTemp)
    return(data)
}

getDeathStats <- function(){
    latestdeaths <- fromJSON("http://api.wurstmineberg.de/server/deaths/latest.json")

    deaths <- data.frame(player = names(latestdeaths$deaths[,1]))
    deaths$timestamp <- unlist(latestdeaths$deaths[,1], use.names=F)
    deaths$cause <- unlist(latestdeaths$deaths[,2], use.names=F)
    deaths$timestamp <- as.POSIXct(deaths$timestamp, tz="UTC")
    deaths$daysSince <- as.numeric(round(difftime(Sys.time(),deaths$timestamp, units="days")))

    # Match against activePeople and sort accordingly
    deaths <- deaths[match(activePeople$id, deaths$player), ]

    deaths$player <- activePeople$name
    deaths$joinStatus <- activePeople$joinStatus

    # Remove NAs introduced by matching with activePoeple
    # Should be uneccessary when everyone hase been active / died since logging started
    deaths <- deaths[!is.na(deaths$timestamp), ]

    return(deaths)
}

getActivePeople <- function(){
    ## Get people.json for player id and join dates
    people <- fromJSON("http://wurstmineberg.de/assets/serverstatus/people.json")
    people <- as.data.frame(people[1])
    names(people) <- sub("people.","",names(people))
    # Add category to people$status for easier matching
    people$status[is.na(people$status)] <- "later"

    activePeople <- data.frame(id=rep(0, length(people$minecraft[people$status != "former"])), 
                               mc=rep(0, length(people$minecraft[people$status != "former"])))
    activePeople$mc <- people$minecraft[people$status != "former"]
    activePeople$name <- people$name[people$status != "former"]
    activePeople$id <- people$id[people$status != "former"]

    # Get people names, and if not set, use ID instead
    for(i in 1:length(activePeople$name)){
      if(is.na(activePeople$name[i])){
        activePeople$name[i] <- activePeople$id[i]
      }
    }

    activePeople$joinDate <- people$join_date[people$status != "former"]
    # In case of missing join date, apply NA
    # For invited but not yet joined players
    activePeople$joinDate[people$joinDate == 0] <- NA
    # Convert joinDate to POSIXct UTC because time
    activePeople$joinDate <- as.POSIXct(activePeople$joinDate, origin="1970-01-01", tz="UTC")

    # player specific server age (days since their whitelisting)
    activePeople$serverAge <- round(as.numeric(difftime(Sys.time(),
                                                       activePeople$joinDate, 
                                                       units ="days")))
    
    # player specific server birth (days they've been whitelisted after server creation)
    activePeople$serverBirth <-round(as.numeric(difftime(activePeople$joinDate,
                                                        activePeople$joinDate[1], 
                                                        units ="days")))
    
    activePeople$joinStatus <- as.factor(people$status[people$status != "former"])

    return(activePeople)
}

mergeItemStats <- function(items){
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
    }

    # Exclude now unneeded old item ID columns
    items <- items[ , !names(items) %in% names(items)[grep("\\.[0-9]+$", names(items))]]

    return(items)
}

# Requires mergeItemStats to be executed on items dataframe
getItemStats <- function(){
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

    # Add some more columns to itemStats for data's sake
    for(i in 1:length(itemStats$stat)){
      stat <- itemStats$stat[i]
      itemStats$total[i] <- sum(items[, stat], na.rm=T)
      
      statPlayers <- items[items[, stat] == max(items[, stat]), c("player", stat)]
      itemStats$leadingPlayer[i] <- as.character(statPlayers[1,1])
      itemStats$playerMax[i] <- statPlayers[1,2]
    }

    # Apparently some columns are "lists", which write.csv() hates
    class(itemStats$leadingPlayer) <- "character"
    class(itemStats$playerMax) <- "numeric"

    return(itemStats)
}
