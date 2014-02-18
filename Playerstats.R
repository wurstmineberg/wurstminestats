# Getting player stats
library(RCurl)
library(jsonlite)
library(ggplot2)

API="http://api.wurstmineberg.de/server/playerstats/general.json"
rawPlayerstats <- getURL(API)
playerstats <- fromJSON(rawPlayerstats)

names(playerstats) <- sub("stat.","",names(playerstats))
playerstats[playerstats == "NULL"] <- "0"
players <- names(playerstats[,1])

## Getting rid of the nested list stuff
## This took me so long, please don't even ask me about it.

for(i in (1:23)){
  playerstats[i] <- unlist(playerstats[i], use.names=F)
}

playerstats <- as.data.frame(mapply(as.numeric,playerstats))
playerstats$player <- players


## plots
ggplot(data=playerstats, aes(x=player, y=deaths)) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Deathcount") +
  ggtitle("Deaths on Wurstmineberg") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("Plots/Deaths.png")

ggplot(data=playerstats, aes(x=player, y=(walkOneCm/1000000))) + 
  geom_bar(colour="black", width=.7, stat="identity") + 
  xlab("Player") + ylab("Distance (km)") +
  ggtitle("Distance walked on Wurstmineberg") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("Plots/Distance.png")
  
