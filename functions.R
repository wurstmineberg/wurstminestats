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
    deaths <- deaths[match(activePeople$id, deaths$player), ]
    deaths$player <- activePeople$name
    deaths$joinStatus <- playerstats$joinStatus
    deaths <- deaths[!is.na(deaths$timestamp), ]

    return(deaths)
}
