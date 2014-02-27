# I wonder if the entity statistics are interesting.

# Refresh data if older than 6 hours
if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 2){
  source("dataPrep.R");
}

# Get strings.json from website for easier mob name replacement
strings <- fromJSON("http://wurstmineberg.de/static/json/strings.json")

# Get columns for killEntity and KilledBy categories respectively
killEntity <- grep("killEntity", names(playerstats))
killedByEntity <- grep("KilledBy", names(playerstats))

# Get a list of mob names from those columns
killEntityMobs <- sub("killEntity.","",names(playerstats[killEntity]))
killedByEntityMobs <- sub("entityKilledBy.","",names(playerstats[killedByEntity]))

# Substitute mob names with more familiar names
for(i in 1:length(names(strings$mobs[2,]))){
  
  killedByEntityMobs <- sub(names(strings$mobs[2,])[i], strings$mobs[2,i], killedByEntityMobs)
  killEntityMobs <- sub(names(strings$mobs[2,])[i], strings$mobs[2,i], killEntityMobs)

}; rm(i);

# Generate graphs for killEntity stats
for(countKill in 1:length(killEntity)){

  killEntityFilename <- paste("Plots/mobs/",names(playerstats[killEntity[countKill]]),".png", sep="")
  
  p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerstats[,killEntity[countKill]]), y=playerstats[,killEntity[countKill]])) + 
    barChart + legendTitle + coord_flip() +
    xLable + labs(y="Kills", title=paste("Kills of:",killEntityMobs[countKill]))
  ggsave(plot=p, file=killEntityFilename, height=plotHeight, width=plotWidth)

}

# Generate graphs for entityKilledBy stats
for(countDeath in 1:length(killedByEntity)){
  
  killedByEntityFilename <- paste("Plots/mobs/",names(playerstats[killedByEntity[countDeath]]),".png", sep="")
  
  p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerstats[,killedByEntity[countDeath]]), y=playerstats[,killedByEntity[countDeath]])) + 
    barChart + legendTitle + coord_flip() +
    xLable + labs(y="Deaths", title=paste("Killed by:",killedByEntityMobs[countDeath]))
  ggsave(plot=p, file=killedByEntityFilename, height=plotHeight, width=plotWidth)
  
}