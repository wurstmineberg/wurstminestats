## plots
source("dataPrep.R")

# Define general legend/guide for all players
playerTheme <- theme(legend.position="right",
                legend.key.size = unit(.4, "cm"),
                legend.text = element_text(size = rel(.8)));

# Convert play time to real time hours
playTimeHours <- (playerstats$playOneMinute/20/60/60)

## Actual plot stuff below

ggplot(data=playerstats, aes(x=reorder(player,deaths), y=deaths)) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Deathcount") +
  ggtitle("Deaths on Wurstmineberg") + coord_flip()
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("Plots/Deaths.png")

ggplot(data=playerstats, aes(x=reorder(player,playTimeHours), y=playTimeHours)) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Online time (hours (real time))") +
  ggtitle("Online time of players on Wurstmineberg") +
  coord_flip()
 # theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Plots/PlayTime.png")

ggplot(data=playerstats, aes(x=reorder(player,walkOneCm), y=(walkOneCm/1000000))) + 
  geom_bar(width=.7, stat="identity") + 
  xlab("Player") + ylab("Distance (km)") +
  ggtitle("Distance walked on Wurstmineberg") + coord_flip()
 # theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("Plots/Distance.png")
  
ggplot(playerstats, aes(x = playTimeHours, y = deaths)) + 
  geom_point(aes(colour=player), group=1) + 
 # geom_smooth(method = lm) + 
  ylab("Deaths") + xlab("Online time (hours (real time))") +
  ggtitle("Deaths vs. Online time") +
  playerTheme +
  scale_colour_discrete(name = "Name") 
ggsave("Plots/Deaths_OnlineTime.png")

#cor(playerstats$deaths,playerstats$playOneMinute)^2

# Mob kills vs play time
ggplot(playerstats, aes(x=playTimeHours, y=mobKills, label=player)) + 
  geom_smooth(method = lm) + 
  geom_point(aes(colour=player, group=1)) + 
  geom_text(size=2, hjust=-.2, vjust=.4) +
  ylab("Mob kills (absolute)") + xlab("Online time (hours (real time)))") +
  ggtitle("Mob kills vs. Online time") +
  playerTheme +
  scale_colour_discrete(name = "Name")
ggsave("Plots/MobKills_OnlineTime.png")

# Distance walked per online time
ggplot(playerstats, aes(x = playTimeHours,  y = (walkOneCm/1000000), label=player)) + 
  geom_point(aes(colour=player, group=1)) + 
 # geom_smooth(method = lm) + 
  ylab("Distance walked (km)") + xlab("Online time (hours (real time)))") +
  ggtitle("Distance walked vs. Online time") +
  playerTheme +
  scale_colour_discrete(name = "Name")
ggsave("Plots/DistanceWalked_OnlineTime.png")

# Server growth
ggplot(playerstats, aes(x = joinDate, y = number, label = player)) + 
  #geom_smooth(method = loess, se=F) + 
  geom_text(size=2, vjust=-.7, hjust=-.2) +
  geom_point() + 
  ylab("Whitelist count") + xlab("Date") +
  ggtitle("Wurstmineberg Server Growth") +
  playerTheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_discrete(name = "Name") +
  scale_x_datetime(labels = date_format("%y-%m-%d"),
                   breaks = date_breaks("month"));
ggsave("Plots/WhitelistGrowth.png")
cor(playerstats$serverAge,playerstats$number, method="spearman")



# Play time vs server age
ggplot(playerstats, aes(x = serverAge, y = playTimeHours, label=player)) +
  geom_text(size=2, hjust=.5, vjust=-1) +
  geom_point(aes(colour=joinStatus, group=1)) + 
  ylab("Time spent on server (hours)") + xlab("Age on server (days)") +
  ggtitle("Time spent on server vs. relative age on server") +
  playerTheme +
  scale_colour_discrete(name = "Join Category")
ggsave("Plots/ServerAge_PlayTime.png")
