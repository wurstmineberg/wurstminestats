## Graphs for the wurstmineberg stats page
# Pull recent data
source("dataPrep.R")

# Animals Bred
# Damage Dealt
# Damage Taken
# Distance by Boat
# Distance by Horse
# Distance by Minecart
# Distance by Pig
# Distance Climbed
# Distance Dove
# Distance Fallen
# Distance Flown
# Distance Swum
# Distance Walked
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,walkOneCm), y=(walkOneCm/1000000))) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Distance (km)") +
  ggtitle("Distance walked") + coord_flip() +
  scale_fill_discrete(name = "Join Status")
ggsave("Plots/statspage/DistanceWalked.png")
# Fish Caught
# Games quit
# Items Dropped
# Jumps
# Junk Fished
# Mob Kills
# Number of Deaths
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,deaths), y=deaths)) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Deathcount") +
  ggtitle("Number of Deaths") + coord_flip() +
  scale_fill_discrete(name = "Join Status")
ggsave("Plots/statspage/NumberOfDeaths.png")
# Player Kills
# Time Played / Idled
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,playOneHour), y=playOneHour)) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Online time (hours (real time))") +
  ggtitle("Time played / ildet") +
  coord_flip() +
  scale_fill_discrete(name = "Join Status")
ggsave("Plots/statspage/TimePlayedIdlet.png")
# Treasure Fished