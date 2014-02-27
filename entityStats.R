# I wonder if the entity statistics are interesting.

# Refresh data if older than 6 hours
if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 2){
  source("dataPrep.R");
}

# Get columns for killEntity and KilledBy categories respectively
killEntity <- grep("killEntity", names(playerstats))
killedByEntity <- grep("KilledBy", names(playerstats))

# Get a list of mob names from those columns
killEntityMobs <- sub("killEntity.","",names(playerstats[killEntity]))
killedByEntityMobs <- sub("entityKilledBy.","",names(playerstats[killedByEntity]))

# Variables to make plotting more readable
killEntityFilename <- paste("Plots/mobs/",names(playerstats[killEntity[1]]),".png", sep="")

p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerstats[,killEntity[1]]), y=playerstats[,killEntity[1]])) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Kills", title=paste("Kills of:",killEntityMobs[1]))
ggsave(plot=p, file=killEntityFilename, height=plotHeight, width=plotWidth)

