## Plots based on achievement stats

# Refresh data if older than 6 hours (only if "now" is defined)
if(length(grep("now", ls())) != 0){
    if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
      source("dataPrep.R");
    }
}

## Generate basic plots for all achievements
for(i in 1:nrow(achievementStrings)){

  if(achievementStrings$id[i] == "exploreAllBiomes"){ next };

  Filename <- paste("Plots/achievements/",achievementStrings$id[i],".png", sep="")
  
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[ , achievementStrings$id[i]]), 
                                y=playerstats[,achievementStrings$id[i]]) 
  p <- p + barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
  p <- p + xLable + ylab("Times Achieved")
      if(nchar(achievementStrings$description[i], type="width") > 44){
        p <- p + theme(plot.title = element_text(size=12))
      }
  p <- p + ggtitle(paste("Achievement:", achievementStrings$displayname[i], "\n", achievementStrings$description[i]))
  
  ggsave(plot=p, file=Filename, height=plotHeight, width=plotWidth)

}; rm(i, p, Filename)


## Generate selected weighted charts

# Cow kill to cow breed ratio
cowRatio <- as.numeric(sub("NaN", "0", playerstats$breedCow/(playerstats$killCow)))
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,cowRatio), y=cowRatio))
p <- p + barChart + legendTitle + coord_flip()
p <- p + xLable + labs(y="Breeds per Kills", title="Cow Breed to Cow Kill Ratio")
ggsave(p, file="Plots/achievements/breedCow_by_killCow.png", height=plotHeight, width=plotWidth)

# Number of opened inventories per played hour
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,openInventory/playOneHour), y=openInventory/playOneHour))
p <- p + barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + xLable + labs(y="Inventories Opened per Hour", title="Inventories Opened weighted by Online Time")
ggsave(p, file="Plots/achievements/openInventory_by_Time.png", height=plotHeight, width=plotWidth)

rm(p, cowRatio)
