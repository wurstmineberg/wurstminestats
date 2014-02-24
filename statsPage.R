#! /usr/bin/rscript
## Graphs for the wurstmineberg stats page
# Pull recent data
source("dataPrep.R")

# Define some variables for plot layout and labels
plotWidth <- 6; plotHeight <- 4;
barChart <- geom_bar(colour="black", width=.7, stat="identity")
legendTitle <- scale_fill_discrete(name = "Join Status")
xLable <- xlab("Player")

# Animals Bred
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,animalsBred), y=animalsBred)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Number of Animals", title="Animals Bred")
ggsave(plot=p,file="Plots/statspage/AnimalsBred.png", height=plotHeight, width=plotWidth)

# Damage Dealt
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,damageDealt), y=damageDealt/2000)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Hearts (thousands)", title="Damage Dealt")
ggsave(plot=p, file="Plots/statspage/DamageDealt.png", height=plotHeight, width=plotWidth)

# Damage Taken
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,damageTaken), y=damageTaken/2000)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Hearts (thousands)", title="Damage Taken")
ggsave(plot=p, file="Plots/statspage/DamageTaken.png", height=plotHeight, width=plotWidth)

# Distance by Boat
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,boatOneCm), y=(boatOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Boat")
ggsave(plot=p, file="Plots/statspage/DistanceByBoat.png", height=plotHeight, width=plotWidth)

# Distance by Horse
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,horseOneCm), y=(horseOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Horse")
ggsave(plot=p, file="Plots/statspage/DistanceByHorse.png", height=plotHeight, width=plotWidth)

# Distance by Minecart
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,minecartOneCm), y=(minecartOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Minecart")
ggsave(plot=p, file="Plots/statspage/DistanceByMinecart.png", height=plotHeight, width=plotWidth)

# Distance by Pig
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,pigOneCm), y=(pigOneCm/1000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (m)", title="Distance by Pig")
ggsave(plot=p, file="Plots/statspage/DistanceByPig.png", height=plotHeight, width=plotWidth)

# Distance Climbed
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,climbOneCm), y=(climbOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Climbed")
ggsave(plot=p, file="Plots/statspage/DistanceClimbed.png", height=plotHeight, width=plotWidth)

# Distance Dove
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,diveOneCm), y=(diveOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Dove")
ggsave(plot=p, file="Plots/statspage/DistanceDove.png", height=plotHeight, width=plotWidth)

# Distance Fallen
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,fallOneCm), y=(fallOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Fallen")
ggsave(plot=p, file="Plots/statspage/DistanceFallen.png", height=plotHeight, width=plotWidth)

# Distance Flown
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,flyOneCm), y=(flyOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Flown")
ggsave(plot=p, file="Plots/statspage/DistanceFlown.png", height=plotHeight, width=plotWidth)

# Distance Swum
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,swimOneCm), y=(swimOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance Swum")
ggsave(plot=p, file="Plots/statspage/DistanceSwum.png", height=plotHeight, width=plotWidth)

# Distance Walked
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,walkOneCm), y=(walkOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance walked")
ggsave(plot=p, file="Plots/statspage/DistanceWalked.png", height=plotHeight, width=plotWidth)

# Fish Caught
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,fishCaught), y=fishCaught)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Amount of Fish", title="Fish Caught")
ggsave(plot=p, file="Plots/statspage/FishCaught.png", height=plotHeight, width=plotWidth)

# Games quit
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,leaveGame), y=leaveGame)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Games quit", title="Games Quit")
ggsave(plot=p, file="Plots/statspage/GamesQuit.png", height=plotHeight, width=plotWidth)

# Items Dropped
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,drop), y=drop)) + 
  barChart + legendTitle + coord_flip() +
  xLable+ labs(y="Items", title="Items Dropped")
ggsave(plot=p, file="Plots/statspage/ItemsDropped.png", height=plotHeight, width=plotWidth)

# Jumps
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,jump), y=jump/1000)) + 
  barChart + legendTitle + coord_flip() +
  xLable+ labs(y="Jumps (thousands)", title="Jumps")
ggsave(plot=p, file="Plots/statspage/Jumps.png", height=plotHeight, width=plotWidth)

# Junk Fished
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,junkFished), y=junkFished)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Amount of Junk", title="Junk Fished")
ggsave(plot=p, file="Plots/statspage/JunkFished.png", height=plotHeight, width=plotWidth)

# Mob Kills
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,mobKills), y=mobKills)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Mobs killed", title="Mob Kills")
ggsave(plot=p, file="Plots/statspage/MobKills.png", height=plotHeight, width=plotWidth)

# Number of Deaths
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,deaths), y=deaths)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Deathcount", title="Number of Deaths")
ggsave(plot=p, file="Plots/statspage/NumberOfDeaths.png", height=plotHeight, width=plotWidth)

# Player Kills
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playerKills), y=playerKills)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Player Kills", title="Player Kills")
ggsave(plot=p, file="Plots/statspage/PlayerKills.png", height=plotHeight, width=plotWidth)

# Time Played / Idled
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playOneHour), y=playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Online time (hours (real time))", title="Time Played / Idled") 
ggsave(plot=p, file="Plots/statspage/TimePlayedIdled.png", height=plotHeight, width=plotWidth)

# Treasure Fished
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,treasureFished), y=treasureFished)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Amount of Treasure", title="Treasure Fished")
ggsave(plot=p, file="Plots/statspage/TreasureFished.png", height=plotHeight, width=plotWidth)

rm(p)