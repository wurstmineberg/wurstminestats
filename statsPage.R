#! /usr/bin/Rscript
## Graphs for the wurstmineberg stats page

# Refresh data if older than 6 hours
if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
  source("dataPrep.R");
}

source("functions.R")

# # Get general statistics from playerstats # Experimental!
# 
# generalStats <- names(playerstats[names(strings$general)])
# generalStatNames <- unlist(strings$general[2,], use.names=F)
# 
# # Generate general stats barcharts ## Experimental
# for(i in 1:length(generalStats)){
#   
#   Filename <- paste("Plots/statspage/", generalStats[i],".png", sep="")
#   
#   p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerstats[, generalStats[i]]), y=playerstats[, generalStats[i]])) + 
#     barChart + legendTitle + coord_flip() + scale_y_discrete(breaks=pretty_breaks()) +
#     xLable + labs(y="Units", title=generalStatNames[i])
#   ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)
#   
# }; rm(i); rm(Filename); rm(p);

############################

# Animals Bred
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, animalsBred), y=animalsBred)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Animals", title="Animals Bred")
ggsave(plot=p, file="Plots/statspage/animalsBred.png", height=plotHeight, width=plotWidth)

# Damage Dealt
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, damageDealt), y=damageDealt/2000)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Hearts (thousands)", title="Damage Dealt")
ggsave(plot=p, file="Plots/statspage/damageDealt.png", height=plotHeight, width=plotWidth)

# Damage Taken
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, damageTaken), y=damageTaken/2000)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Hearts (thousands)", title="Damage Taken")
ggsave(plot=p, file="Plots/statspage/damageTaken.png", height=plotHeight, width=plotWidth)

# Distance by Boat
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, boatOneCm), y=(boatOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Boat")
ggsave(plot=p, file="Plots/statspage/boatOneCm.png", height=plotHeight, width=plotWidth)

# Distance by Horse
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, horseOneCm), y=(horseOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Horse")
ggsave(plot=p, file="Plots/statspage/horseOneCm.png", height=plotHeight, width=plotWidth)

# Distance by Minecart
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, minecartOneCm), y=(minecartOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Minecart")
ggsave(plot=p, file="Plots/statspage/minecartOneCm.png", height=plotHeight, width=plotWidth)

# Distance by Pig
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, pigOneCm), y=(pigOneCm/1000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (m)", title="Distance by Pig")
ggsave(plot=p, file="Plots/statspage/pigOneCm.png", height=plotHeight, width=plotWidth)

# Distance Climbed
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, climbOneCm), y=(climbOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Climbed")
ggsave(plot=p, file="Plots/statspage/climbOneCm.png", height=plotHeight, width=plotWidth)

# Distance Dove
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, diveOneCm), y=(diveOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Dove")
ggsave(plot=p, file="Plots/statspage/diveOneCm.png", height=plotHeight, width=plotWidth)

# Distance Fallen
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, fallOneCm), y=(fallOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Fallen")
ggsave(plot=p, file="Plots/statspage/fallOneCm.png", height=plotHeight, width=plotWidth)

# Distance Flown
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, flyOneCm), y=(flyOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Flown")
ggsave(plot=p, file="Plots/statspage/flyOneCm.png", height=plotHeight, width=plotWidth)

# Distance Swum
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, swimOneCm), y=(swimOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Swum")
ggsave(plot=p, file="Plots/statspage/swimOneCm.png", height=plotHeight, width=plotWidth)

# Distance Walked
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, walkOneCm), y=(walkOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance walked")
ggsave(plot=p, file="Plots/statspage/walkOneCm.png", height=plotHeight, width=plotWidth)

# Fish Caught
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, fishCaught), y=fishCaught)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Amount of Fish", title="Fish Caught")
ggsave(plot=p, file="Plots/statspage/fishCaught.png", height=plotHeight, width=plotWidth)

# Games Quit
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, leaveGame), y=leaveGame)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Games Quit", title="Games Quit")
ggsave(plot=p, file="Plots/statspage/leaveGame.png", height=plotHeight, width=plotWidth)

# Items Dropped
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, drop), y=drop)) + 
  barChart + legendTitle + coord_flip() +
  xLable+ labs(y="Items", title="Items Dropped")
ggsave(plot=p, file="Plots/statspage/drop.png", height=plotHeight, width=plotWidth)

# Jumps
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, jump), y=jump/1000)) + 
  barChart + legendTitle + coord_flip() +
  xLable+ labs(y="Jumps (thousands)", title="Jumps")
ggsave(plot=p, file="Plots/statspage/jump.png", height=plotHeight, width=plotWidth)

# Junk Fished
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, junkFished), y=junkFished)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Amount of Junk", title="Junk Fished")
ggsave(plot=p, file="Plots/statspage/junkFished.png", height=plotHeight, width=plotWidth)

# Mob Kills
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, mobKills), y=mobKills)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Mobs killed", title="Mob Kills")
ggsave(plot=p, file="Plots/statspage/mobKills.png", height=plotHeight, width=plotWidth)

# Number of Deaths
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, deaths), y=deaths)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Deathcount", title="Number of Deaths")
ggsave(plot=p, file="Plots/statspage/deaths.png", height=plotHeight, width=plotWidth)

# Player Kills
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playerKills), y=playerKills)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Player Kills", title="Player Kills")
ggsave(plot=p, file="Plots/statspage/playerKills.png", height=plotHeight, width=plotWidth)

# Time Played / Idled
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, playOneHour), y=playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Online time (hours (real time))", title="Time Played / Idled") 
ggsave(plot=p, file="Plots/statspage/playOneMinute.png", height=plotHeight, width=plotWidth)

# Treasure Fished
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, treasureFished), y=treasureFished)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Amount of Treasure", title="Treasure Fished")
ggsave(plot=p, file="Plots/statspage/treasureFished.png", height=plotHeight, width=plotWidth)

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
