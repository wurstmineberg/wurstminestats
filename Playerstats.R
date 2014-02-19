# Getting player stats
library(RCurl)
library(jsonlite)
library(ggplot2)

# Getting some data for furthe use
people <- fromJSON("http://wurstmineberg.de/assets/serverstatus/people.json")
playerstats <- fromJSON("http://api.wurstmineberg.de/server/playerstats/general.json")

# A little cleanup
names(playerstats) <- sub("stat.","",names(playerstats))
playerstats[playerstats == "NULL"] <- "0"
playerTemp <- names(playerstats[,1])
## Add "none" category to people$status for easier matching
people$status[is.na(people$status)] <- "none"
## Getting rid of the nested list stuff
## This took me so long, please don't even ask me about it.
for(i in (1:23)){
  playerstats[i] <- unlist(playerstats[i], use.names=F)
} 
rm(i)
## Getting rid of NAs and assuming 0
playerstats[playerstats == NA] <- 0
## Numericizzle
playerstats <- as.data.frame(mapply(as.numeric,playerstats))
## Sorting according to people.json
playerstats$player <- playerTemp; rm(playerTemp);
playerstats <- playerstats[match(people$minecraft[people$status != "former"], playerstats$player),]

playerstats$joinDate <- people$join_date[people$status != "former"]

rownames(playerstats) <- NULL
#playerstats <- playerstats[playerstats == NA] <- "0"

playerstats <- playerstats[c(ncol(playerstats)-1,
                 ncol(playerstats),
                 1:(ncol(playerstats)-2))]

playerstats$player <- people$id[people$status != "former"]

playerstats$player <- factor(playerstats$player, 
                             levels=playerstats$player)

## Getting rid of NAs and assuming 0 (again. Don't ask.)
playerstats[is.na(playerstats)] <- 0
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
  
ggplot(playerstats, aes(x = playOneMinute, 
                       y = deaths)) + 
  geom_point(shape = 1, aes(colour=as.factor(player), group=1)) + 
  geom_smooth(method = lm) + 
  ylab("Deaths") + xlab("Online time") +
  ggtitle("Deaths vs. Online time") +
  theme(legend.position="right")+
  theme(legend.key.size = unit(.4, "cm")) +
  theme(legend.text = element_text(size = rel(.7))) +
  scale_colour_discrete(name = "Name")
  ggsave("Plots/Deaths_OnlineTime.png")

cor(playerstats$deaths,playerstats$playOneMinute)^2

