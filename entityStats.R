# I wonder if the entity statistics are interesting.

# Get columns for killEntity and KilledBy categories respectively
killEntity      <- grep("killEntity", names(playerstats))
killedByEntity  <- grep("entityKilledBy", names(playerstats))

# Get a list of mob names from those columns
killEntityMobs      <- sub("killEntity.", "", names(playerstats[killEntity]))
killedByEntityMobs  <- sub("entityKilledBy.", "", names(playerstats[killedByEntity]))

# Substitute mob names with more familiar names
for(i in 1:ncol(strings$mobs)){
  
  killedByEntityMobs  <- sub(names(strings$mobs[2,])[i], strings$mobs[2,i], killedByEntityMobs)
  killEntityMobs      <- sub(names(strings$mobs[2,])[i], strings$mobs[2,i], killEntityMobs)

}; rm(i);

##########################################
## Generate graphs for killEntity stats ##
##########################################

for(i in 1:length(killEntity)){

  filename <- paste("Plots/mobs/", names(playerstats[killEntity[i]]),".png", sep="")
  
  p <- ggplot(data=playerstats) 
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[,killEntity[i]], mean, order=T), y=playerstats[,killEntity[i]], mean, order=T)
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
  p <- p + xLable + labs(y="Kills", title=paste("Kills of:",killEntityMobs[i]))
  
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(i, p, filename)

##############################################
## Generate graphs for entityKilledBy stats ##
##############################################

for(i in 1:length(killedByEntity)){
  
  filename <- paste("Plots/mobs/", names(playerstats[killedByEntity[i]]),".png", sep="")
  
  p <- ggplot(data=playerstats) 
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[,killedByEntity[i]], mean, order=T), 
                                y=playerstats[,killedByEntity[i]])
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
  p <- p + xLable + labs(y="Deaths", title=paste("Killed by:",killedByEntityMobs[i]))
  
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)
  
}; rm(i, p, filename)


############################################
## Generate top killed / deaths by charts ##
############################################

# Kills per mob #

mobsKilled <- data.frame(killedMob =  killEntityMobs,
                         nKills    =  unlist(colwise(sum)(playerstats[killEntity]), use.names=F))

p <- ggplot(data=mobsKilled) 
p <- p + aes(x=reorder(killedMob, nKills, mean, order=T), y=nKills)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + labs(x="Mobs", y="Kills", title="Killed Mobs")
ggsave(plot=p, file="Plots/mobs/Kills_byMob.png", height=plotHeight, width=plotWidth)

# … Let's filter out Endermen #
mobsKilledFiltered <- mobsKilled[mobsKilled$killedMob != "Enderman",]

p <- ggplot(data=mobsKilledFiltered) 
p <- p + aes(x=reorder(killedMob, nKills, mean, order=T), y=nKills)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + labs(x="Mobs", y="Kills", title="Killed Mobs (except Endermen)")
ggsave(plot=p, file="Plots/mobs/Kills_byMob_except_Endermen.png", height=plotHeight, width=plotWidth)

# Deaths by mob #
mobsKilledBy <- data.frame(killedByMob  = killedByEntityMobs,
                           nKilledBy    = unlist(colwise(sum)(playerstats[killedByEntity]), use.names=F))

p <- ggplot(data=mobsKilledBy) 
p <- p + aes(x=reorder(killedByMob, nKilledBy, mean, order=T), y=nKilledBy)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + labs(x="Mobs", y="Deaths", title="Deaths by Mob")
ggsave(plot=p, file="Plots/mobs/Deaths_byMob.png", height=plotHeight, width=plotWidth)

###########################################
## Classify by hostile and friendly mobs ##
###########################################

killFriendly  <- c("killEntity.Bat", "killEntity.Sheep", "killEntity.Pig", "killEntity.Chicken", "killEntity.Cow", 
                  "killEntity.EntityHorse", "killEntity.Squid", "killEntity.MushroomCow", "killEntity.Villager", 
                  "killEntity.Ozelot", "killEntity.Wolf")
killHostile   <- c("killEntity.PigZombie", "killEntity.Blaze", "killEntity.Enderman", "killEntity.Spider", 
                  "killEntity.Zombie", "killEntity.Skeleton", "killEntity.Creeper", "killEntity.Silverfish", 
                  "killEntity.Witch", "killEntity.Slime", "killEntity.CaveSpider", "killEntity.MushroomCow", 
                  "killEntity.LavaSlime", "killEntity.Ghast", "killEntity.Endermite")

for(i in 1:nrow(playerstats)){

  playerstats$killFriendly[i]   <- sum(playerstats[i, killFriendly])
  playerstats$killHostile[i]    <- sum(playerstats[i, killHostile])

}; rm(i);

##############################
## Generate graphs for that ##
##############################

# Friendly Mobs #
p <- ggplot(data=playerstats) 
p <- p + aes(fill=joinStatus, x=reorder(player, killFriendly, mean, order=T), y=killFriendly)
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + xLable + labs(y="Kills", title="Total Friendly Mobs Killed")
ggsave(plot=p, file="Plots/mobs/KillFriendlies.png", height=plotHeight, width=plotWidth)

# Hostile Mobs #
p <- ggplot(data=playerstats) 
p <- p + aes(fill=joinStatus, x=reorder(player, killHostile, mean, order=T), y=killHostile)
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + xLable + labs(y="Kills", title="Total Hostile Mobs Killed")
ggsave(plot=p, file="Plots/mobs/KillHostiles.png", height=plotHeight, width=plotWidth)


rm(killedByEntity, killedByEntityMobs, killEntity, killEntityMobs)
