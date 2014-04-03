## Plots based on achievement stats

refreshData()

achievementStrings    <- getAchievementStrings()

###############################################
## Generate basic plots for all achievements ##
###############################################

for(i in 1:nrow(achievementStrings)){

  ID          <- achievementStrings$id[i]
  name        <- achievementStrings$displayname[i]
  description <- achievementStrings$description[i]

  if(ID == "exploreAllBiomes"){ next };

  filename <- paste("Plots/achievements/", ID,".png", sep="")
  
  p <- ggplot(data=playerstats)
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[ , ID], mean, order = T), 
                                y=playerstats[ , ID]) 
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
  p <- p + xLable + ylab("Times Achieved")
      if(nchar(description, type="width") > 44){
        p <- p + theme(plot.title = element_text(size=12))
      }
  p <- p + ggtitle(paste("Achievement:", name, "\n", description))
  
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(i, p, filename, name, ID, description)

#######################################
## Generate selected weighted charts ##
#######################################

# Cow kill to cow breed ratio
cowRatio <- as.numeric(sub("NaN", "0", playerstats$breedCow/(playerstats$killCow)))
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,cowRatio, mean, order=T), y=cowRatio))
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable + labs(y="Breeds per Kills", title="Cow Breed to Cow Kill Ratio")
ggsave(p, file="Plots/achievements/breedCow_by_killCow.png", height=plotHeight, width=plotWidth)

# Number of opened inventories per played hour
p <- ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player, openInventory/playOneHour, mean, order=T), 
                                                   y=openInventory/playOneHour))
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + xLable + labs(y="Inventories Opened per Hour", title="Inventories Opened weighted by Online Time")
ggsave(p, file="Plots/achievements/openInventory_by_Time.png", height=plotHeight, width=plotWidth)

rm(p, cowRatio)
