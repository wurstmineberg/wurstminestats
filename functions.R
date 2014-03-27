## Defining some functions used by other scripts

#########################
### General functions ###
#########################

writePlayerstatsLog <- function(){

  playerstatsOld <- read.csv(file="data/playerstats.csv", row.names=1)

  # Only append saved date if the new data is at least 6h newer then the last saved data
  nowDate <- as.POSIXct(as.numeric(dataTime), origin="1970-01-01")
  lastSavedDate <- as.POSIXct(max(as.numeric(playerstatsOld$timestamp)), origin="1970-01-01")

  if(as.numeric(difftime(nowDate, lastSavedDate, units ="hours")) > 6){

    # Join new data with saved data and order by joinDate, player, then timestamp
    playerstatsFull <- join(playerstats, playerstatsOld, type="full", match="all")
    playerstatsFull <- arrange(playerstatsFull, as.Date(joinDate), player, timestamp)

    # Write dataset to file for ze easy access
    write.csv(playerstatsFull, "data/playerstats.csv")
  }
}

# Refresh data if older than 6 hours (only if "dataTime" is defined)
refreshData <- function(force=FALSE){
   if("dataTime" %in% ls()){
    if((as.numeric(format(Sys.time(), "%s")) - as.numeric(dataTime))/60/60 > 6){
      source("dataPrep.R");
    } else if (force == TRUE){
        source("dataPrep.R")
    }
  }
}

getStrings <- function(){
    if("strings" %in% ls() == F){
        strings <- fromJSON("http://wurstmineberg.de/static/json/strings.json")
    }
    return(strings)
}

getItemData <- function(){
    ## Get items.json from our website for names/ids ##
    itemData        <- fromJSON("http://wurstmineberg.de/static/json/items.json")
    itemData$numID  <- names(itemData$id)
    itemData$ID     <- unlist(itemData$id, use.names=F)
    itemData$ID     <- sub(":",".", itemData$ID)
    itemData$name   <- unlist(itemData$name, use.names=F)
    itemData        <- subset(itemData, select=c("numID","ID","name"))
}

sortLevels <- function(factors, reference, sortFunction = mean){
  sortedLevels <- reorder(factors, reference, sortFunction, order=T)
  
  return(sortedLevels)
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

    data$player <- factor(activePeople$name, levels=activePeople$name)
    
    return(data)
}

getDeathStats <- function(){
    latestdeaths <- fromJSON("http://api.wurstmineberg.de/server/deaths/latest.json")

    deaths <- data.frame(player = names(latestdeaths$deaths[,1]))
    deaths$timestamp    <- unlist(latestdeaths$deaths[,1], use.names=F)
    deaths$cause        <- unlist(latestdeaths$deaths[,2], use.names=F)
    deaths$timestamp    <- as.POSIXct(deaths$timestamp, tz="UTC")
    deaths$daysSince    <- as.numeric(round(difftime(Sys.time(),deaths$timestamp, units="days")))

    # Match against activePeople and sort accordingly
    deaths <- deaths[match(activePeople$id, deaths$player), ]

    deaths$player       <- activePeople$name
    deaths$joinStatus   <- activePeople$joinStatus

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
    people$numID <- 1:nrow(people)
    
    # Handle favColours
    for(col in c("red", "green", "blue")){
      people$favColor[, col] <- as.hexmode(people$favColor[, col])
    }
    people$color <- paste("#",  people$favColor[, "red"], 
                                people$favColor[, "green"], 
                                people$favColor[, "blue"], sep="")
    people$color[people$color == "#NANANA"] <- NA

    ## Start to construct activePeople, which is like people.json, but useful ##
    
    activePeople        <- data.frame(numID = people$numID[people$status != "former"])
    activePeople$id     <- people$id[people$status != "former"]
    activePeople$mc     <- people$minecraft[people$status != "former"]
    activePeople$name   <- people$name[people$status != "former"]
    activePeople$color  <- people$color[people$status != "former"]

    # If people name not set, use ID instead
    for(i in 1:length(activePeople$name)){
      if(is.na(activePeople$name[i])){
        activePeople$name[i] <- activePeople$id[i]
      }
    }

    activePeople$joinDate <- people$join_date[people$status != "former"]
    # In case of missing join date, apply NA / For invited but not yet joined players
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

#################################
### Functions for itemStats.R ###
#################################

mergeItemStats <- function(items, itemActions, itemData){
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
getItemStats <- function(items, itemActions, itemData){
    # Let's just construct a dataframe of stats, their items and actions
    existingIDs <- names(items)[grep("[^player]", names(items))]
    itemStats   <- data.frame(stat=as.character(existingIDs))

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

################################
### Functions for sessions.R ###
################################

getSessions <- function(){
    sessions <- fromJSON("http://api.wurstmineberg.de/server/sessions/overview.json")
    sessions <- as.data.frame(sessions)

    sessions$uptimes.startTime  <- as.POSIXct(sessions$uptimes.startTime, tz="UTC")
    sessions$uptimes.endTime    <- as.POSIXct(sessions$uptimes.endTime, tz="UTC")

    ## Fill playerSessions with data from sessions$uptimes.sessions in an ugly way because fuck JSON handling in R
    numSessions <- length(sessions$uptimes.sessions)

    # If the latest session is NA, we'll just and it RIGHT NOW
    if(is.na(sessions$uptimes.endTime[numSessions])){
      sessions$uptimes.endTime[numSessions] <- Sys.time()  
    }

    return(sessions)
}

getPlayerSessions <- function(sessions){
    # Initialize an empty data frame for player sessions and name
    playerSessions <- data.frame(minecraftNick = character(0),
                                 joinTime = character(0),
                                 leaveTime = character(0),
                                 person = character(0))

    # This uses session endTimes as leaveTime in case the session ended in a non-standard way
    for(i in 1:(nrow(sessions)-1)){
      temp1 <- as.data.frame(sessions$uptimes.sessions[i])
      temp2 <- as.data.frame(sessions$uptimes.sessions[i+1])
      
      if(NA %in% temp1$leaveTime){
        temp1$leaveTime[is.na(temp1$leaveTime)] <- as.character(sessions$uptimes.endTime[i])
      }
      if(NA %in% temp2$leaveTime){
        temp2$leaveTime[is.na(temp2$leaveTime)] <- as.character(sessions$uptimes.endTime[i+1])
      }
      
      tempMerge <- join(temp1, temp2, type="full")
      rm(temp1, temp2)
      playerSessions <- join(tempMerge, playerSessions, type="full")
    }; rm(i, tempMerge)

    playerSessions <- arrange(playerSessions, joinTime, leaveTime)

    # Now we reformat shit
    playerSessions$joinTime  <- as.POSIXct(playerSessions$joinTime, tz="UTC")
    playerSessions$leaveTime <- as.POSIXct(playerSessions$leaveTime, tz="UTC")

    # Fixing remains of the last fix (overlapping sessions get sequentialized)
    for(i in 1:(nrow(playerSessions)-1)){
      if(playerSessions$minecraftNick[i] == playerSessions$minecraftNick[i+1]){
        if(playerSessions$leaveTime[i] > playerSessions$joinTime[i+1]){
          playerSessions$leaveTime[i] <- playerSessions$joinTime[i+1]
        }
      }
    }

    playerSessions$date <- format(playerSessions$joinTime, "%F")
    playerSessions$date <- as.POSIXct(playerSessions$date, origin="1970-01-01", tz="UTC")

    return(playerSessions)
}

splitSessionsByDay <- function(playerSessions){
    playerSessions$joinDate     <- format(playerSessions$joinTime, "%F")
    playerSessions$joinDate     <- as.POSIXct(playerSessions$joinDate, origin="1970-01-01", tz="UTC")
    playerSessions$leaveDate    <- format(playerSessions$leaveTime, "%F")
    playerSessions$leaveDate    <- as.POSIXct(playerSessions$leaveDate, origin="1970-01-01", tz="UTC")

    overlaps    <- playerSessions[playerSessions$leaveDate > playerSessions$joinDate, ]
    noOverlaps  <- playerSessions[playerSessions$leaveDate == playerSessions$joinDate, ]
    overlapsNum <- nrow(overlaps)

    for(i in 1:overlapsNum){
      temp1 <- overlaps[1,]
      temp1[1, ] <- overlaps[i, ]
      temp1[2, ] <- overlaps[i, ]
      
      temp1$leaveTime[1] <- overlaps$leaveDate[i]
      temp1$joinTime[2]  <- overlaps$leaveDate[i]
      
      noOverlaps <- join(noOverlaps, temp1, type="full")
      rm(temp1)
    }
    
    playerSessions <- arrange(noOverlaps, joinTime, person)
    playerSessions <- playerSessions[c("person", "date", "joinTime", "leaveTime")]
    
    return(playerSessions)
}

## Handling the clusterfuck that is colors

colDiff <- function(col.i, col.j){
  absDiff <- sum(abs(col2rgb(col.i) - col2rgb(col.j)))
  return(absDiff)
}

colErrors <- function(peopleTemp){
  peopleTemp$colConflict <- 0
  for(i in 1:nrow(peopleTemp)){
    for(j in nrow(peopleTemp):1){
      if(peopleTemp$name[i] == peopleTemp$name[j]){next}
      
      if(colDiff(peopleTemp$color[i], peopleTemp$color[j]) < 50){
        peopleTemp$colConflict[i] <- peopleTemp$colConflict[i] + 1
      }
    }
  }
  return(peopleTemp)
}

fixPeopleColors <- function(peopleTemp){
    peopleTemp$colFixed <- !is.na(peopleTemp$color)
    peopleTemp$colConflict <- 1
    while(sum(peopleTemp$colConflict) > 0){
      for(i in 1:nrow(peopleTemp)){

        if(peopleTemp$colConflict[i] > 0){
          if(!peopleTemp$colFixed[i]){
            peopleTemp$color[i] <- sample(colours(), 1)
          } else if(is.na(peopleTemp$color[i])){
            peopleTemp$color[i] <- sample(colours(), 1)
          }
        }

      }
      peopleTemp <- colErrors(peopleTemp)
    }
    peopleTemp <- peopleTemp[, !names(peopleTemp) %in% c("colFixed", "colConflict")]
    return(peopleTemp)
}

