#-------------------------#
#### Achievement plots ####
#-------------------------#
message("Generating achievement plots")

for(i in 1:nrow(strings.achievements)){
  
  ID          <- strings.achievements$id[i]
  name        <- strings.achievements$displayname[i]
  description <- strings.achievements$description[i]
  
  if(ID == "exploreAllBiomes"){ next };
  
  filename <- paste("Plots/achievements/", ID,".png", sep="")
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = joinStatus, 
               x    = sortLevels(player, playerstats[[ID]]), 
               y    = playerstats[[ID]]) 
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
  p <- p + xLable   + ylab("Times Achieved")
  if(nchar(description, type  = "width") > 44){
    p <- p + theme(plot.title = element_text(size = 12))
  }
  p <- p + ggtitle(paste("Achievement:", name, "\n", description))
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(i, p, filename, name, ID, description)

## Generate selected weighted charts ##

# Cow kill to cow breed ratio
cowRatio <- as.numeric(sub("NaN", "0", playerstats$breedCow/(playerstats$killCow)))
p <- ggplot(data  = playerstats, aes(fill = joinStatus, x = sortLevels(player,cowRatio), y = cowRatio))
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable   + labs(y = "Breeds per Kills", title = "Cow Breed to Cow Kill Ratio")
ggsave(p, file    = "Plots/achievements/breedCow_by_killCow.png", height = plotHeight, width = plotWidth)

# Number of opened inventories per played hour
p <- ggplot(data  = playerstats) 
p <- p + aes(fill = joinStatus, x = sortLevels(player, openInventory / playOneHour), y = openInventory / playOneHour)
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
p <- p + xLable   + labs(y = "Inventories Opened per Hour", title = "Inventories Opened weighted by Online Time")
ggsave(p, file    = "Plots/achievements/openInventory_by_Time.png", height = plotHeight, width = plotWidth)
rm(cowRatio)

# Adventuring time progress in number of biomes visited
p <- ggplot(playerstats)
p <- p + aes(fill   = joinStatus, x = sortLevels(player, exploreAllBiomes.count), y = exploreAllBiomes.count)
p <- p + barChart   + coord_flip() + statusFillScale + xLable
p <- p + labs(title = "Number of biomes explored \n (Only biomes relevant to Adventuring Time)", y = "Biomes")
ggsave(p, file = "Plots/achievements/exploreAllBiomesProgress.png", height = plotHeight, width = plotWidth)
