## Graphs for the wurstmineberg stats page

refreshData()

# Get strings.json for some… strings. (General stat IDs, display names)
strings <- getStrings()

# Get general statistics from playerstats, define metadata (scale, units)
statNum             <- ncol(strings$general)
generalStats        <- data.frame(id=character(statNum), name=character(statNum), unit=character(statNum), scale=numeric(statNum))
generalStats$id     <- names(playerstats[names(strings$general)])
generalStats$name   <- unlist(strings$general[2,], use.names=F)
generalStats$unit   <- c("Animals", "km", "km", "km", "Hearts (thousands)", "Hearts (thousands)", "Deathcount", "km", "Items", "km", "Fish", "km", "km", "Jumps (thousands)", "Junk", "Quits", "km", "Mobs", "m", "Hours (real life)", "Kills", "km", "km", "Hours (real life)", "Treasure", "km")
generalStats$scale  <- c(1,         10^5, 10^5, 10^5, 10*2*10^3,            10*2*10^3,             1,           10^5,  1,      10^5,  1,     10^5,  10^5, 1000,                1,      1,      10^5,  1,    1000,  20*60*60,           1,       10^5, 10^5,  20*60*60,           1,          10^5)

#########################################################################
## Generate general stats barcharts and versions scaled by online time ##
#########################################################################

for(i in 1:statNum){

  filename  <- paste("Plots/statspage/", generalStats$id[i],".png", sep="")
  stat      <- generalStats$id[i]
  statScale <- generalStats$scale[i]
  statName  <- generalStats$name[i]
  statUnit  <- generalStats$unit[i]

  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, stat], mean, order=T), 
                                y=playerstats[, stat] / statScale)
  p <- p + barChart + legendTitle + coord_flip()
  p <- p + scale_y_discrete(breaks=pretty_breaks()) 
  p <- p + xLable + labs(y=statUnit, title=statName)
  
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

  ## Weighted by hours played ##

  if(stat == "playOneMinute" | stat == "timeSinceDeath"){ next };
  
  filename <- paste("Plots/statspage/scaled/", stat,"_scaled.png", sep="")
  
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, stat] / playerstats$playOneHour, mean, order=T), 
               y=(playerstats[, stat] / statScale) / playerstats$playOneHour)
  p <- p + barChart + legendTitle + coord_flip()
  p <- p + xLable + labs(y=paste(statUnit, "per hour"), title=paste(statName, "weighted by Online Time"))
  
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)
  
}; rm(i, filename, p, stat, statScale, statUnit, statName)

# Distance Traveled Total
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled, mean, order=T), y=(distanceTraveled/1000000))
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km)", title="Distance Traveled in Total")
ggsave(plot=p, file="Plots/statspage/DistanceTraveled.png", height=plotHeight, width=plotWidth)

# Distance Traveled Total by Online Time
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled/playOneHour, mean, order=T), y=(distanceTraveled/1000000)/playOneHour)
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Distance (km) per hour", title="Total Distance Traveled weighted by Online Time")
ggsave(plot=p, file="Plots/statspage/DistanceTraveled_scaled.png", height=plotHeight, width=plotWidth)
