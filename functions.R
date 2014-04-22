## Defining some functions used by other scripts

# First up, some convenience variables
require(lubridate)
list.months <- months(seq(from = as.Date("14-01-01", "%F"), to = as.Date("14-12-01", "%F"), by = "month"))
list.wdays  <- as.character(wday(c(2:7,1), T, F))

#########################
### General functions ###
#########################

getListElement <- function(list_, level = 1){
  # This is used to access a level of a nested list as a vector, helping with fromJSON imports
  list2 <- unlist(lapply(list_, function(x) cbind(x[[level]])), use.names = F)
  return(list2)
}

writePlayerstatsLog <- function(){

  playerstatsOld <- read.csv(file="data/playerstats.csv", row.names=1)

  # Only append saved date if the new data is at least 6h newer then the last saved data
  nowDate <- as.POSIXct(as.numeric(dataTime), origin="1970-01-01")
  lastSavedDate <- as.POSIXct(max(as.numeric(playerstatsOld$timestamp)), origin="1970-01-01")

  if(as.numeric(difftime(nowDate, lastSavedDate, units ="hours")) > 6){

    # Join new data with saved data and order by joinDate, player, then timestamp
    playerstatsFull <- join(playerstats, playerstatsOld, type="full", match="all")
    playerstatsFull <- arrange(playerstatsFull, joinDate, player, timestamp)

    # Write dataset to file for ze easy access
    write.csv(playerstatsFull, "data/playerstats.csv")
  }
}

getStrings <- function(category = "general"){
  require(jsonlite)
    strings                <- fromJSON(getOption("url.strings.general"))
    strings                <- melt(strings$stats)
    names(strings)         <- c("name", "id", "category")
   if (category == "general"){
       strings.general        <- dplyr::filter(strings, category == "general")
       strings.general$unit   <- c("Animals", "km", "km", "km", "Hearts (thousands)", "Hearts (thousands)", "Deathcount", "km", "Items", "km", "Fish", "km", "km", "Jumps (thousands)", "Junk", "Quits", "km", "Mobs", "m", "Hours (real life)", "Kills", "km", "km", "Hours (real life)", "Treasure", "km")
       strings.general$scale  <- c(1,         10^5, 10^5, 10^5, 10*2*10^3,            10*2*10^3,             1,           10^5,  1,      10^5,  1,     10^5,  10^5, 1000,                1,      1,      10^5,  1,    1000,  20*60*60,           1,       10^5, 10^5,  20*60*60,           1,          10^5)
       return(strings.general)
   } else if (category == "mobs"){
       strings.mobs           <- dplyr::filter(strings, category == "mobs")
     return(strings.mobs)
   }
}

## Get achievement descriptions from website and append IDs as extra column
getAchievementStrings <- function(){
  require(jsonlite)
  
  acs    <- fromJSON(getOption("url.strings.achievements"))

  strings.achievements             <- data.frame(id = names(acs))
  strings.achievements$description <- unlist(lapply(acs, function(x) cbind(x[[1]])), use.names=F)
  strings.achievements$name        <- unlist(lapply(acs, function(x) cbind(x[[2]])), use.names=F)
  strings.achievements$id          <- as.character(strings.achievements$id) # defactorize
  return(strings.achievements)
}

getItemData <- function(){
  require(jsonlite)
    ## Get items.json from our website for names/ids ##
    itemsJSON        <- fromJSON(getOption("url.strings.items"))
    itemData        <- data.frame(numID = names(itemsJSON))
    itemData$ID     <- getListElement(itemsJSON, "id")
    itemData$ID     <- sub(":",".", itemData$ID)
    itemData$name   <- getListElement(itemsJSON, "name")

    return(itemData)
}

# Define function to transform JSON from playerstats API to nice dataframe
prettyShitUp <- function(data){
    ## Removing "stat." and "achievement." prefixes from columns
    names(data) <- sub("stat.","",names(data))
    names(data) <- sub("achievement.","",names(data))
    # Fix for exploreAllBiomes shit ("Fix" = Ignore this shit)
    if("exploreAllBiomes" %in% names(data)){
        data <- subset(data, select=-exploreAllBiomes)
    }
    
    data[data == "NULL"] <- "0"

    ## Extract player names to separate variable
    # Note: These are the minecraft usernames
    playerTemp <- names(data[,1])

    ## Getting rid of the nested list stuff
    data <- colwise(unlist, use.names=F)(data)
    
    ## Getting rid of NAs and assuming 0
    data[data == NA] <- 0

    ## Numericizzle
    data <- colwise(as.numeric)(data)

    ## Sorting according to people.json, requires activePoeple to be generated from people.json
    data$player <- playerTemp
    data        <- data[match(activePeople$mc, data$player), ]
    data$player <- factor(activePeople$name, levels=activePeople$name)
    data        <- data[c("player", setdiff(names(data), "player")) ]
    return(data)
}

stats2df <- function(data, type = "default"){
  # Spiritual successor to prettyShitUp()

  # First we need to get the nested lists into a nice data.frame
  # Achievements need to be handled separately because of the sublist
  if (type == "achievements"){
    AdventuringProgress <<- lapply(data, function(x){ 
                                   rbind(x$achievement.exploreAllBiomes.progress)
                             })
    data.df <- ldply(data, function(player){
                          stats <- names(player)
                          data.frame(player[stats != "achievement.exploreAllBiomes"])
                })
    
  } else {
    # Everything else can be handled quite simply
    data.df <- ldply(data, data.frame)
  }
  # .id is a leftover of ldply(), contains player names, i.e. elements of 'data'
  data.df <- rename(data.df, replace=c(".id" = "player"))
  # Nullifying NAs for plotting reasons
  data.df[is.na(data.df)] <- 0
  # Removing "stat." and "achievement." prefixes from column names
  names(data.df) <- sub("stat.", "", names(data.df))
  names(data.df) <- sub("achievement.", "", names(data.df))
  # Sorting according to activePeople, reordering the rows and factor levels
  data.df        <- data.df[match(activePeople$mc, data.df$player), ]
  data.df$player <- factor(activePeople$name, levels = activePeople$name, ordered = T)
  
  return(data.df)
  
}

getDeathStats <- function(){
  require(jsonlite)
   
    latestdeaths <- fromJSON(getOption("url.general.deaths.latest"))
    
    deaths              <- data.frame(player = names(latestdeaths$deaths))
    deaths$timestamp    <- getListElement(latestdeaths$deaths, "timestamp")
    deaths$cause        <- getListElement(latestdeaths$deaths, "cause")
    
   #deaths$timestamp    <- unlist(latestdeaths$deaths[,1], use.names=F)
   #deaths$cause        <- unlist(latestdeaths$deaths[,2], use.names=F)
    deaths$timestamp    <- as.POSIXct(deaths$timestamp, tz="UTC")
    deaths$daysSince    <- as.numeric(round(difftime(Sys.time(), deaths$timestamp, units="days")))

    # Match against activePeople and sort accordingly
    deaths              <- deaths[match(activePeople$id, deaths$player), ]
    deaths$player       <- activePeople$name
    deaths$joinStatus   <- activePeople$joinStatus

    # Remove NAs introduced by matching with activePoeple
    # Should be uneccessary when everyone hase been active / died since logging started
    deaths <- deaths[!is.na(deaths$timestamp), ]

    return(deaths)
}

getActivePeople <- function(){
  require(jsonlite)
    ## Get people.json for player id and join dates
    people <- fromJSON(getOption("url.strings.people"))
    people <- as.data.frame(people[1])
    names(people) <- sub("people.","",names(people))
    # Add category to people$status for easier matching, give numIDs excluding Dinnerbone
    people$status[is.na(people$status)] <- "later"
    people$numID[people$id != "dinnerbone"] <- 1:nrow(people[people$id != "dinnerbone", ])
    
    ## Handle favColours
    people$color[!is.na(people$favColor[1])] <- rgb(people$favColor[!is.na(people$favColor[1]), c("red", "green", "blue")], maxColorValue=255)

    ## Start to construct activePeople, which is like people.json, but useful ##
    
    activePeople        <- data.frame(numID = people$numID[people$status != "former"])
    activePeople$id     <- people$id[people$status != "former"]
    activePeople$mc     <- people$minecraft[people$status != "former"]
    activePeople$name   <- people$name[people$status != "former"]
    activePeople$color  <- people$color[people$status != "former"]
    activePeople$invitedBy <- people$invitedBy[people$status != "former"]

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
    
    activePeople$inviteGap <- c(0, round(as.numeric(difftime(
                                          activePeople$joinDate[2:nrow(activePeople)], 
                                          activePeople$joinDate[1:(nrow(activePeople)-1)], 
                                          units="days"))))

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
  require(jsonlite)
    sessions <- fromJSON(getOption("url.general.sessions"))
    sessions <- as.data.frame(sessions)

    sessions$uptimes.startTime  <- as.POSIXct(sessions$uptimes.startTime, tz="UTC")
    sessions$uptimes.endTime    <- as.POSIXct(sessions$uptimes.endTime, tz="UTC")

    ## Fill playerSessions with data from sessions$uptimes.sessions in an ugly way because fuck JSON handling in R
    numSessions <- nrow(sessions)

    # If the latest session is NA, we'll just and it RIGHT NOW
    if(is.na(sessions$uptimes.endTime[numSessions])){
      sessions$uptimes.endTime[numSessions] <- as.POSIXct(as.POSIXlt(Sys.time(), tz="UTC"))
    }
    
    # If current session is empty, change indexing bound to let it go
    if(is.null(sessions$uptimes.sessions[[numSessions]])){
      numSessions <- numSessions - 1}
    
    if("leaveTime" %in% names(sessions$uptimes.sessions[[numSessions]]) == FALSE){
      sessions$uptimes.sessions[[numSessions]]$leaveTime <- as.character(sessions$uptimes.endTime[numSessions])
    }   
    
    # This uses session endTimes as leaveTime in case the session ended in a non-standard way
    for(i in 1:numSessions){
      if(is.null(sessions$uptimes.sessions[[i]])){next} # Skip empty sessions
      NAcond <- is.na(sessions$uptimes.sessions[[i]]["leaveTime"])
      sessions$uptimes.sessions[[i]][NAcond, "leaveTime"] <- as.character(sessions$uptimes.endTime[i])
    }
    
    return(sessions[1:numSessions,])
}

getPlayerSessions <- function(sessions){

    # Now we can use the sessions set as a dataframe of player sessions
    playerSessions <- ldply(sessions$uptimes.sessions, as.data.frame)
    playerSessions <- arrange(playerSessions, joinTime, leaveTime)
    
    
    # Now we reformat shit because time is a fucking mess
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
    
    # Add duration column
    playerSessions$playedMinutes <- as.numeric(difftime(playerSessions$leaveTime, 
                                                        playerSessions$joinTime, unit="mins"))

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
      
      noOverlaps <- rbind(noOverlaps, temp1)
      rm(temp1)
    }
    
    playerSessions      <- arrange(noOverlaps, joinTime, person)
    playerSessions$date <- format(playerSessions$joinTime, "%F")
    playerSessions$date <- as.POSIXct(playerSessions$date, origin="1970-01-01", tz="UTC")
    playerSessions      <- select(playerSessions, -joinDate, -leaveDate)

    # Update duration column
    playerSessions$playedMinutes <- as.numeric(difftime(playerSessions$leaveTime, 
                                                        playerSessions$joinTime, unit="mins"))
    
    playerSessions$wday <- factor(weekdays(playerSessions$date), 
                                  levels = list.wdays, ordered = T)
    playerSessions$month <- factor(months(playerSessions$date), 
                                  levels = list.months, ordered = T)
    
    return(playerSessions)
}

getPlayedPerPerson <- function(PlayerSessions){
  playedPerPerson <- ddply(playerSessions, .(date, person, wday, month), summarize, timePlayed = sum(playedMinutes))
  playedPerPerson <- arrange(playedPerPerson, date, person)

  for(i in playedPerPerson$person){
    playedPerPerson$person[playedPerPerson$person == i] <- activePeople$name[activePeople$id == i]
  }

  playedPerPerson$person <- as.factor(playedPerPerson$person)
  playedPerPerson$person <- reorder(playedPerPerson$person, new.order=activePeople$name)

  return(playedPerPerson)
}

###############################################
### Handling the clusterfuck that is colors ###
###############################################

colErrors <- function(peopleTemp, simLimit = 0.92){
  peopleTemp$colConflict <- 0
  for(i in 1:nrow(peopleTemp)){
    for(j in nrow(peopleTemp):1){
      if(peopleTemp$name[i] == peopleTemp$name[j]){next}
      
      if(colSimilarity(peopleTemp$color[i], peopleTemp$color[j]) > simLimit){
        peopleTemp$colConflict[i] <- peopleTemp$colConflict[i] + 1
      }
    }
  }
  return(peopleTemp)
}

randCol <- function(n = 1){
  col <- character(0)
  for(i in 1:n){
    col[i] <- rgb(runif(1, 0, 1), runif(1, 0, 1), runif(1, 0, 1))
  }
  return(col)
}

fixPeopleColors <- function(peopleTemp, simLimit = 0.92){
    peopleTemp$colFixed <- !is.na(peopleTemp$color)
    peopleTemp$colConflict <- 1
    while(sum(peopleTemp$colConflict) > 5){
      for(i in 1:nrow(peopleTemp)){

        if(peopleTemp$colConflict[i] > 0){
          if(!peopleTemp$colFixed[i]){
            peopleTemp$color[i] <- randCol()
          } else if(is.na(peopleTemp$color[i])){
            peopleTemp$color[i] <- randCol()
          }
        }

      }
      peopleTemp <- colErrors(peopleTemp, simLimit)
    }
    peopleTemp <- peopleTemp[, !names(peopleTemp) %in% c("colFixed", "colConflict")]
    return(peopleTemp)
}

colSimilarity <- function(col.i, col.j){
  col.i     <- as.vector(col2rgb(col.i))
  col.j     <- as.vector(col2rgb(col.j))
  scalProd  <- col.i %*% col.j
  normProd  <- sqrt(sum(col.i * col.i)) * sqrt(sum(col.j * col.j))
  if(normProd == 0 & scalProd > 0){
    colSim  <- 1
  } else if(normProd == 0 & scalProd == 0){
    colSim <- 0
    } else {
    colSim  <- scalProd / normProd 
  }
  
  return(colSim)
}

#########################
### For the lulz shit ###
#########################

serverBirthday <- function(activePeople){
  require(lubridate)
  now           <- as.POSIXlt(Sys.time(), "UTC")
  now$year      <- now$year + 1900
  ydays         <- as.POSIXlt(activePeople$joinDate)$yday - now$yday
  daysToNext    <- min(ydays[ydays > 0])
  daysSinceLast <- max(ydays[ydays < 0])
  nextPerson    <- activePeople$name[ydays == daysToNext]
  lastPerson    <- activePeople$name[ydays == daysSinceLast]
  nextDate      <- format(activePeople$joinDate[activePeople$name == nextPerson[1]], "%m-%d")
  lastDate      <- format(activePeople$joinDate[activePeople$name == lastPerson[1]], "%m-%d")
  
  birthdays <- data.frame(nextPerson = nextPerson, nextDate = nextDate,
                          lastPerson = lastPerson, lastDate = lastDate)
  return(birthdays)
}

##############################
### Generally useful stuff ###
##############################

# Via http://stackoverflow.com/a/18339562/409362
moveCol <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  return(x)
}

sortLevels <- function(factors, reference, sortFunction = mean){
  require(gdata)
  sortedLevels <- reorder(factors, reference, sortFunction, order=T)
  return(sortedLevels)
}
