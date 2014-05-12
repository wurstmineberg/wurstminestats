# Defining some functions used by other scripts -------------------------------

# First up, some convenience variables
#require(lubridate)
#list.months <- months(seq(from = as.Date("14-01-01", "%F"), to = as.Date("14-12-01", "%F"), by = "month"))
#list.wdays  <- as.character(wday(c(2:7,1), T, F))

#-------------------------#
#### General functions ####
#-------------------------#

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

#-----------------------------------------------#
#### Handling the clusterfuck that is colors ####
#-----------------------------------------------#

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

#-------------------------#
#### For the lulz shit ####
#-------------------------#

serverBirthday <- function(activePeople){
  require(lubridate)
  now           <- as.POSIXlt(Sys.time(), "UTC")
  now$year      <- now$year + 1900
  ydays         <- as.POSIXlt(activePeople$joinDate)$yday - now$yday
  daysToNext    <- min(ydays[ydays > 0])
  daysSinceLast <- max(ydays[ydays < 0])
  nextPerson    <- activePeople$name[ydays == daysToNext]
  lastPerson    <- activePeople$name[ydays == daysSinceLast]
  nextDate      <- format(activePeople$joinDate[activePeople$name == nextPerson], "%m-%d")
  lastDate      <- format(activePeople$joinDate[activePeople$name == lastPerson], "%m-%d")
  
  birthdays <- data.frame(nextPerson = nextPerson, nextDate = nextDate,
                          lastPerson = lastPerson, lastDate = lastDate,
                          daysToNext = daysToNext, daysSinceLast = daysSinceLast)
  return(birthdays)
}

statOfTheDay <- function(player = "random", category = "general"){
  if (player == "random"){
    rPlayer    <- activePeople$name[round(runif(1, 1, nrow(activePeople)))]
  } else if (player %in% activePeople$name){
    rPlayer  <- player
  } else {
    stop("Player not found :(")
  }
  if (category == "general"){
    statset <- generalstats[!(names(generalstats) %in% c("playOneHour", "distanceTraveled"))]
    r2      <- round(runif(1, 2, ncol(statset)))
    stat    <- names(statset[r2])
    desc    <- as.character(strings.general$name[strings.general$id == stat])
    number  <- round(statset[statset$player == rPlayer, r2] / strings.general$scale[strings.general$id == stat], 2)
    unit    <- strings.general$unit[strings.general$id == stat]
    msg     <- paste0("Random stat for ", rPlayer, ": ", number, " ", unit, " in category: ", desc)
  } else if (category == "items"){
    statset <- items
    r2      <- round(runif(1, 2, ncol(statset)))
    stat    <- names(statset[r2])
    desc    <- itemStats$item[itemStats$stat == stat]
    action  <- itemStats$action[itemStats$stat == stat]
    number  <- round(statset[statset$player == rPlayer, r2])
    msg     <- paste0("Random stat! ", rPlayer, " has ", action, " the item “", desc, "” ", number, " times")
  } else if (category == "mobs"){
    statset <- entities
    r2      <- round(runif(1, 2, ncol(statset)))
    stat    <- names(statset[r2])
    desc    <- mobStats$mob[mobStats$stat == stat]
    action  <- mobStats$action[mobStats$stat == stat]
    number  <- round(statset[statset$player == rPlayer, r2])
    if (action == "killed"){
      msg   <- paste0("Random stat! ", rPlayer, " has ", action, " the mob “", desc, "” ", number, " times")
    } else {
      msg   <- paste0("Random stat! ", rPlayer, " was ", action, " the mob “", desc, "” ", number, " times")
    }
  } else {
    stop("Category not recognized, only general, items and mobs work.")
  }
  msg <- paste(msg, "#randomstat")
  return(msg)
}

randomItemStat <- function(){
  r1         <- round(runif(1, 1, nrow(itemStats)))
  item       <- itemStats$item[r1]
  action     <- itemStats$action[r1]
  number     <- itemStats$total[r1]
  leadPlayer <- itemStats$leadingPlayer[r1]
  playerMax  <- itemStats$playerMax[r1]
  msg        <- paste0('The item “', item, '” was ', action, ' ', number, ' times in total, with ', leadPlayer, ' leading with ', playerMax, ' — Accounting for ', round((playerMax/number)*100, 2), '%')
  if (nchar(msg) < 130){
    msg <- paste(msg, "#itemstat")
  }
  return(msg)
}

randomAchievement <- function(player = "random"){
  if (player == "random"){
    rPlayer  <- activePeople$name[round(runif(1, 1, nrow(activePeople)))]
  } else if (player %in% activePeople$name){
    rPlayer  <- player
  } else {
    stop("Player not found :(")
  }
  r1       <- round(runif(1, 2, ncol(achievements)))
  achValue <- achievements[achievements$player == rPlayer, r1]
  achName  <- strings.achievements$displayname[strings.achievements$id == names(achievements[r1])]
  msg      <- paste0(rPlayer, "'s achievement progress for “", achName, "” is ", achValue)
  if (names(achievements[r1]) == "exploreAllBiomesProgress"){
    achName <- "Adventuring Time"
    msg      <- paste0(rPlayer, "'s achievement progress for “", achName, "” is ", achValue, " of 36 biomes")
  }
  if (nchar(msg) < 126){
    msg <- paste(msg, "#achievements")
  }
  return(msg)
}

randomMobStat <- function(){
  entry      <- mobStats[runif(1, 1, nrow(mobStats)), ]
  maxPercent <- round((entry$playerMax/entry$total)*100, 2)
  if (entry$action == "killed"){
    msg <- paste0("The mob “", entry$mob, "” was killed ", entry$total, " times, mostly by ",
                  entry$leadingPlayers, ", who killed ", entry$playerMax, " (", maxPercent, "%)")
  } else if (entry$action == "killed by"){
    msg <- paste0("The mob “", entry$mob, "” caused ", entry$total, " player deaths, mostly for ",
                  entry$leadingPlayers, ", who died ", entry$playerMax, " times (", maxPercent, "%) because of it")
  } else {
    stop("Something went wrong.")
  }
  if (nchar(msg) < 131){
    msg <- paste(msg, "#mobstat")
  }
  return(msg)
}

mostActiveDay <- function(daysAgo = 7){
  data    <- playedPerDay[playedPerDay$date >= now() - days(daysAgo), ]
  maximum <- data[data$timePlayed == max(data$timePlayed), ]
  msg     <- paste0("The most active day in the past ", daysAgo, " days was ", 
                    maximum$wday, " (", maximum$date, "), with ", 
                    round(maximum$timePlayed/60, 1), " (combined) hours played in total")
  if (nchar(msg) < 115){
    msg <- paste(msg, "#thisWeekInWurstmineberg")
  }
  return(msg)
}

dailyActivity <- function(daysAgo = 1){
  pastDay        <- playedPerPerson[playedPerPerson$date > (now() - days(daysAgo + 1)), ]
  if (nrow(pastDay) == 0){
    stop("Nobody played :(")
  }
  people         <- unique(pastDay$person)
  hoursPlayed    <- sum(pastDay$timePlayed)/60
  peopleMax      <- pastDay$person[pastDay$timePlayed == max(pastDay$timePlayed)]
  hoursPlayedMax <- sum(pastDay$timePlayed[pastDay$person == peopleMax])/60
  msg <- paste0("In the past ",  daysAgo*24, " hours, ", length(people)[1], " people were online, accumulating ", 
                round(hoursPlayed, 2), " hours total. ", peopleMax, " played the most: ",
                round(hoursPlayedMax, 2), "h (", round((hoursPlayedMax/hoursPlayed)*100, 2), "%)")
  if (nchar(msg) < 118){
    msg <- paste(msg, "#todayInWurstmineberg")
  }
  return(msg)
}

getBirthdayNotification <- function(birthdays, forceoutput = FALSE){
  if (birthdays$daysToNext == 1){
    msg <- paste0("Tomorrow is going to be the server birthday of ", birthdays$nextPerson, ", yaay!")
  } else if (birthdays$daysToNext > 1 && birthdays$daysToNext < 10 | forceoutput){
    msg <- paste0("In ", birthdays$daysToNext, " days, it's going to be the server birthday of ", birthdays$nextPerson, ", yaay!")
  } else {
    msg <- NULL
  }
  return(msg)
}
#------------------------------#
#### Generally useful stuff ####
#------------------------------#

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
  sortedLevels <- reorder(factors, reference, sortFunction, order = T)
  return(sortedLevels)
}

# Via: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
