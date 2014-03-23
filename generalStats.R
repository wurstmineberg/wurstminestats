## Graphs for the wurstmineberg stats page

refreshData()

# Get general statistics from playerstats, define metadata (scale, units)

statNum <- ncol(strings$general)
generalStats <- data.frame(id=character(statNum), name=character(statNum), unit=character(statNum), scale=numeric(statNum))
generalStats$id <- names(playerstats[names(strings$general)])
generalStats$name <- unlist(strings$general[2,], use.names=F)
generalStats$unit   <- c("Animals", "km", "km", "km", "Hearts (thousands)", "Hearts (thousands)", "Deathcount", "km", "Items", "km", "Fish", "km", "km", "Jumps (thousands)", "Junk", "Quits", "km", "Mobs", "m", "Hours (real life)", "Kills", "km", "km", "Hours (real life)", "Treasure", "km")
generalStats$scale  <- c(1,         10^5, 10^5, 10^5, 10*2*10^3,            10*2*10^3,             1,           10^5,  1,      10^5,  1,     10^5,  10^5, 1000,                1,      1,      10^5,  1,    1000,  20*60*60,           1,       10^5, 10^5,  20*60*60,           1,          10^5)

# Generate general stats barcharts

for(i in 1:statNum){

  Filename <- paste("Plots/statspage/", generalStats$id[i],".png", sep="")

  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, generalStats$id[i]], mean, order=T), 
                                y=playerstats[, generalStats$id[i]] / generalStats$scale[i])
  p <- p + barChart + legendTitle + coord_flip()
  p <- p + scale_y_discrete(breaks=pretty_breaks()) 
  p <- p + xLable + labs(y=generalStats$unit[i], title=generalStats$name[i])
  
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)

  # Weighted by hours played

  if(generalStats$id[i] == "playOneMinute" | generalStats$id[i] == "timeSinceDeath"){ next };
  
  Filename <- paste("Plots/statspage/scaled/", generalStats$id[i],"_scaled.png", sep="")
  
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, generalStats$id[i]]/playerstats$playOneHour, mean, order=T), 
               y=(playerstats[, generalStats$id[i]] / generalStats$scale[i])/playerstats$playOneHour)
  p <- p + barChart + legendTitle + coord_flip()
#  p <- p + scale_y_discrete(breaks=pretty_breaks()) 
  p <- p + xLable + labs(y=paste(generalStats$unit[i], "per hour"), title=paste(generalStats$name[i], "weighted by Online Time"))
  
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)
  
}; rm(i, Filename, p)

# Distance Traveled Total
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled, mean, order=T), y=(distanceTraveled/1000000))
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km)", title="Distance Traveled in Total")
ggsave(p, file="Plots/statspage/DistanceTraveled.png", height=plotHeight, width=plotWidth)

# Distance Traveled Total by Online Time
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled/playOneHour, mean, order=T), y=(distanceTraveled/1000000)/playOneHour)
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km) per hour", title="Total Distance Traveled weighted by Online Time")
ggsave(p, file="Plots/statspage/DistanceTraveled_scaled.png", height=plotHeight, width=plotWidth)
