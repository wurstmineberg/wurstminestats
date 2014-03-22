## Graphs for the wurstmineberg stats page

# Refresh data if older than 6 hours (only if "now" is defined)
if(length(grep("now", ls())) != 0){
    if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
      source("dataPrep.R");
    }
}

source("functions.R")

# Get general statistics from playerstats, define metadata (scale, units)

statNum <- ncol(strings$general)
generalStats <- data.frame(id=rep(0,statNum), name=rep(0,statNum), unit=rep(0,statNum), scale=rep(0,statNum))
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
    if(generalStats$unit[i] != "km" & generalStats$unit[i] != "m"){
      p <- p + scale_y_discrete(breaks=pretty_breaks()) 
    }
  p <- p + xLable + labs(y=generalStats$unit[i], title=generalStats$name[i])
  
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)
   
}; rm(i, Filename, p)


# Weighted by hours played

for(i in 1:statNum){
  
  if(generalStats$id[i] == "playOneMinute"){ next };
  
  Filename <- paste("Plots/statspage/scaled/", generalStats$id[i],"_scaled.png", sep="")
  
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, generalStats$id[i]]/playerstats$playOneHour, mean, order=T), 
               y=(playerstats[, generalStats$id[i]] / generalStats$scale[i])/playerstats$playOneHour)
  p <- p + barChart + legendTitle + coord_flip()
  #  if(generalStats$unit[i] != "km" & generalStats$unit[i] != "m"){
  #    p <- p + scale_y_discrete(breaks=pretty_breaks()) 
  #  }
  p <- p + xLable + labs(y=paste(generalStats$unit[i], "per hour"), title=paste(generalStats$name[i], "weighted by Online Time"))
  
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)
  
}; rm(i, Filename, p)
