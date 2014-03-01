#! /usr/bin/Rscript
## Graphs for the wurstmineberg stats page

# Refresh data if older than 6 hours
if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
  source("dataPrep.R");
}

source("functions.R")

# Get general statistics from playerstats, define metadata (scale, units)

statNum <- ncol(strings$general)
generalStats <- data.frame(id=rep(0,statNum), name=rep(0,statNum), 
                           units=rep(0,statNum), scale=rep(0,statNum))
generalStats$id <- names(playerstats[names(strings$general)])
generalStats$name <- unlist(strings$general[2,], use.names=F)
generalStats$unit <- c("Animals", "km", "km", "Hearts (thousands)", "Hearts (thousands)", "Deathcount", "km", "Items", "km", "Fish", "km", "km", "Jumps (thousands)", "Junk", "Quits", "km", "Mobs", "m", "Hours (real life)", "Kills", "km", "Treasure", "km")
generalStats$scale <- c(1,10^6,10^6,2000,2000,1,10^6,1,10^6,1,10^6,10^6,1000,1,1,10^6,1,1000,(20*60*60),1,10^6,1,10^6)

# Generate general stats barcharts ## Experimental
for(i in 1:statNum){
   
  Filename <- paste("Plots/statspage/", generalStats$id[i],".png", sep="")
   
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, generalStats$id[i]]), 
                                y=playerstats[, generalStats$id[i]] / generalStats$scale[i])
  p <- p + barChart + legendTitle + coord_flip()
  p <- p + scale_y_discrete(breaks=pretty_breaks())
  p <- p + xLable + labs(y=generalStats$unit[i], title=generalStats$name[i])
  
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)
   
}; rm(i, Filename, p, statNum)


###################### Weighted by hours played ######################

# Animals Bred
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, animalsBred/playOneHour), y=animalsBred/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Animals per Hour (real time)", title="Animals Bred per Online Time")
ggsave(plot=p, file="Plots/statspage/animalsBred_weighted.png", height=plotHeight, width=plotWidth)

# Damage Dealt
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, damageDealt/playOneHour), y=(damageDealt/2000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Hearts (thousands) per Hour (real time)", title="Damage Dealt per Online Time")
ggsave(plot=p, file="Plots/statspage/damageDealt_weighted.png", height=plotHeight, width=plotWidth)

# Damage Taken
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, damageTaken/playOneHour), y=(damageTaken/2000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Hearts (thousands) per Hour (real time)", title="Damage Taken per Online Time")
ggsave(plot=p, file="Plots/statspage/damageTaken_weighted.png", height=plotHeight, width=plotWidth)

# Distance by Boat
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, boatOneCm/playOneHour), y=(boatOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance by Boat per Online Time")
ggsave(plot=p, file="Plots/statspage/boatOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance by Horse
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, horseOneCm/playOneHour), y=(horseOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance by Horse per Online Time")
ggsave(plot=p, file="Plots/statspage/horseOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance by Minecart
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, minecartOneCm/playOneHour), y=(minecartOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance by Minecart per Online Time")
ggsave(plot=p, file="Plots/statspage/minecartOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance by Pig
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, pigOneCm/playOneHour), y=(pigOneCm/1000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (m) per Hour (real time)", title="Distance by Pig per Online Time")
ggsave(plot=p, file="Plots/statspage/pigOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance Climbed
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, climbOneCm/playOneHour), y=(climbOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance Climbed per Online Time")
ggsave(plot=p, file="Plots/statspage/climbOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance Dove
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, diveOneCm/playOneHour), y=(diveOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance Dove per Online Time")
ggsave(plot=p, file="Plots/statspage/diveOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance Fallen
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, fallOneCm/playOneHour), y=(fallOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance Fallen per Online Time")
ggsave(plot=p, file="Plots/statspage/fallOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance Flown
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, flyOneCm/playOneHour), y=(flyOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance Flown per Online Time")
ggsave(plot=p, file="Plots/statspage/flyOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance Swum
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, swimOneCm/playOneHour), y=(swimOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance Swum per Online Time")
ggsave(plot=p, file="Plots/statspage/swimOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Distance Walked
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, walkOneCm/playOneHour), y=(walkOneCm/1000000)/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km) per Hour (real time)", title="Distance walked per Online Time")
ggsave(plot=p, file="Plots/statspage/walkOneCm_weighted.png", height=plotHeight, width=plotWidth)

# Fish Caught
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, fishCaught/playOneHour), y=fishCaught/playOneHour)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Fish per Hour (real time)", title="Fish Caught per Online Time")
ggsave(plot=p, file="Plots/statspage/fishCaught_weighted.png", height=plotHeight, width=plotWidth)

# Games Quit
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, leaveGame/playOneHour), y=leaveGame/playOneHour)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Games Quit per Hour (real time)", title="Games Quit per Online Time")
ggsave(plot=p, file="Plots/statspage/leaveGame_weighted.png", height=plotHeight, width=plotWidth)

# Items Dropped
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, drop/playOneHour), y=drop/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable+ labs(y="Items per Hour (real time)", title="Items Dropped per Online Time")
ggsave(plot=p, file="Plots/statspage/drop_weighted.png", height=plotHeight, width=plotWidth)

# Jumps
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, jump/playOneHour), y=jump/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable+ labs(y="Jumps per Hour (real time)", title="Jumps per Online Time")
ggsave(plot=p, file="Plots/statspage/jump_weighted.png", height=plotHeight, width=plotWidth)

# Junk Fished
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, junkFished/playOneHour), y=junkFished/playOneHour)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Junk per Hour (real time)", title="Junk Fished per Online Time")
ggsave(plot=p, file="Plots/statspage/junkFished_weighted.png", height=plotHeight, width=plotWidth)

# Mob Kills
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, mobKills/playOneHour), y=mobKills/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Mobs killed per Hour (real time)", title="Mob Kills per Online Time")
ggsave(plot=p, file="Plots/statspage/mobKills_weighted.png", height=plotHeight, width=plotWidth)

# Number of Deaths
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, deaths/playOneHour), y=deaths/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Deaths per Hour (real time)", title="Deaths per Online Time")
ggsave(plot=p, file="Plots/statspage/deaths_weighted.png", height=plotHeight, width=plotWidth)

# Player Kills
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerKills/playOneHour), y=playerKills/playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Player Kills per Hour (real time)", title="Player Kills per Online Time")
ggsave(plot=p, file="Plots/statspage/playerKills_weighted.png", height=plotHeight, width=plotWidth)

# Treasure Fished
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, treasureFished/playOneHour), y=treasureFished/playOneHour)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Treasure per Hour (real time)", title="Treasure Fished per Online Time")
ggsave(plot=p, file="Plots/statspage/treasureFished_weighted.png", height=plotHeight, width=plotWidth)


rm(p)
