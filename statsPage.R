## Graphs for the wurstmineberg stats page
# Pull recent data
source("dataPrep.R")

# Define some variables for plot layout and labels
plotWidth <- 6; plotHeight <- 4;
barChart <- geom_bar(colour="black", width=.7, stat="identity")
legendTitle <- scale_fill_discrete(name = "Join Status")
xLable <- xlab("Player")

# Animals Bred
# Damage Dealt
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,damageDealt), y=damageDealt)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Damage Dealt", title="Damage Dealt")
ggsave(file="Plots/statspage/DamageDealt.png", height=plotHeight, width=plotWidth)

# Damage Taken
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,abs(damageTaken)), y=abs(damageTaken))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Damage Dealt", title="Damage Taken (broken as of now)")
ggsave(file="Plots/statspage/DamageTaken.png", height=plotHeight, width=plotWidth)

# Distance by Boat
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,boatOneCm), y=(boatOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Boat")
ggsave(file="Plots/statspage/DistanceByBoat.png", height=plotHeight, width=plotWidth)

# Distance by Horse
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,horseOneCm), y=(horseOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Horse")
ggsave(file="Plots/statspage/DistanceByHorse.png", height=plotHeight, width=plotWidth)

# Distance by Minecart
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,minecartOneCm), y=(minecartOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance by Minecart")
ggsave(file="Plots/statspage/DistanceByMinecart.png", height=plotHeight, width=plotWidth)

# Distance by Pig
# Distance Climbed
# Distance Dove
# Distance Fallen
# Distance Flown
# Distance Swum

# Distance Walked
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,walkOneCm), y=(walkOneCm/1000000))) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Distance (km)", title="Distance walked")
ggsave(file="Plots/statspage/DistanceWalked.png", height=plotHeight, width=plotWidth)

# Fish Caught

# Games quit
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,leaveGame), y=leaveGame)) +
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Games quit", title="Games Quit")
ggsave(file="Plots/statspage/GamesQuit.png", height=plotHeight, width=plotWidth)

# Items Dropped
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,drop), y=drop)) + 
  barChart + legendTitle + coord_flip() +
  xLable+ labs("Drops", title="Items Dropped")
ggsave(file="Plots/statspage/NumberOfDeaths.png", height=plotHeight, width=plotWidth)

# Jumps

# Junk Fished

# Mob Kills
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,mobKills), y=mobKills)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Mobs killed", title="Mob Kills")
ggsave(file="Plots/statspage/MobKills.png", height=plotHeight, width=plotWidth)

# Number of Deaths
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,deaths), y=deaths)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Deathcount", title="Number of Deaths") +
ggsave(file="Plots/statspage/NumberOfDeaths.png", height=plotHeight, width=plotWidth)

# Player Kills
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playerKills), y=playerKills)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Player Kills", title="Player Kills")
ggsave(file="Plots/statspage/PlayerKills.png", height=plotHeight, width=plotWidth)

# Time Played / Idled
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playOneHour), y=playOneHour)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Online time (hours (real time))", title="Time Played / Idlet") 
ggsave(file="Plots/statspage/TimePlayedIdlet.png", height=plotHeight, width=plotWidth)

# Treasure Fished