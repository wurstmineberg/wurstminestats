# I wonder if the entity statistics are interesting.

# Refresh data if older than 6 hours
if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
  source("dataPrep.R");
}

source("functions.R")

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
for(i in 1:length(killEntity)){

  Filename <- paste("Plots/mobs/",names(playerstats[killEntity[i]]),".png", sep="")
  
  p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerstats[,killEntity[i]]), y=playerstats[,killEntity[i]])) + 
    barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks()) +
    xLable + labs(y="Kills", title=paste("Kills of:",killEntityMobs[i]))
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)

}; rm(i); rm(Filename)

# Generate graphs for entityKilledBy stats
for(i in 1:length(killedByEntity)){
  
  Filename <- paste("Plots/mobs/",names(playerstats[killedByEntity[i]]),".png", sep="")
  
  p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerstats[,killedByEntity[i]]), y=playerstats[,killedByEntity[i]])) + 
    barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks()) +
    xLable + labs(y="Deaths", title=paste("Killed by:",killedByEntityMobs[i]))
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)
  
}; rm(i); rm(Filename)

# # Classify by hostile and friendly mobs
# 
# for( i in names(playerstats[killEntity]))
# killFriendlyMobs <- 0
# for(i in 1:nrow(playerstats)){
#   killFriendyMobs[i] <- sum(playerstats[i, grep("Pig | Ozelot | MushroomCow | Cow", names(playerstats[killEntity]))])
# }; rm(i);


rm(p)
rm(killedByEntity, killedByEntityMobs, killEntity, killEntityMobs)
