#-------------------------------------------------------------------------------#
#### General stats barcharts and versions scaled by online time // Statspage ####
#-------------------------------------------------------------------------------#
cat("Generating general stats plots \n")

statNum <- nrow(strings.general)

for(i in 1:statNum){
  
  filename  <- paste("Plots/statspage/", strings.general$id[i],".png", sep="")
  stat      <- strings.general$id[i]
  statScale <- strings.general$scale[i]
  statName  <- strings.general$name[i]
  statUnit  <- strings.general$unit[i]
  
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, stat], mean, order=T), 
               y=playerstats[, stat] / statScale)
  p <- p + barChart + statusFillScale + coord_flip()
  p <- p + scale_y_discrete(breaks=pretty_breaks()) 
  p <- p + xLable + labs(y=statUnit, title=statName)
  
  cat(paste("Saving", filename, "\n"))
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)
  
  ## Weighted by hours played ##
  
  if(stat == "playOneMinute" | stat == "timeSinceDeath"){ next };
  
  filename <- paste("Plots/statspage/scaled/", stat,"_scaled.png", sep="")
  
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, stat] / playerstats$playOneHour, mean, order=T), 
               y=(playerstats[, stat] / statScale) / playerstats$playOneHour)
  p <- p + barChart + statusFillScale + coord_flip()
  p <- p + xLable + labs(y=paste(statUnit, "per hour"), title=paste(statName, "weighted by Online Time"))
  
  cat(paste("Saving", filename, "\n"))
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)
  
}; rm(i, filename, p, stat, statScale, statUnit, statName, statNum)

# Distance Traveled Total
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled, mean, order=T), y=(distanceTraveled/1000000))
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable + labs(y="Distance (km)", title="Distance Traveled in Total")
ggsave(plot=p, file="Plots/statspage/DistanceTraveled.png", height=plotHeight, width=plotWidth)

# Distance Traveled Total by Online Time
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=reorder(player,distanceTraveled/playOneHour, mean, order=T), y=(distanceTraveled/1000000)/playOneHour)
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable + labs(y="Distance (km) per hour", title="Total Distance Traveled weighted by Online Time")
ggsave(plot=p, file="Plots/statspage/DistanceTraveled_scaled.png", height=plotHeight, width=plotWidth)
