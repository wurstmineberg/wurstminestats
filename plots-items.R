#----------------------------------------#
#### Plotting item stats as they come ####
#----------------------------------------#
message("Generating item stats plots")

for(i in 1:length(itemStats$stat)){
  
  stat        <- itemStats$stat[i]
  action      <- itemStats$action[i]
  itemName    <- itemStats$item[i]
  filename    <- paste0("Plots/items/",    action, "/",  stat, ".png")
  title       <- paste0("Times item was ", action, ": ", itemName)
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = joinStatus, 
               x    = sortLevels(player, playerstats[[stat]]), 
               y    = playerstats[[stat]])
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
  p <- p + labs(x   = "Player", y = paste("Times", action), title = title)
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(p, i, title, filename, stat, action, itemName)

## Now to look at the different item actions ##
message("Generating item actions plots")

# subset for each action, get top 20 items for each action, and plot them
for(action in itemActions$name){
  itemStatsPerAction <- itemStats[itemStats$action == action,]
  itemStatsPerAction <- head(arrange(itemStatsPerAction, desc(total)), 20)
  filename           <- paste0("Plots/items/top_", action, ".png")
  
  p <- ggplot(data  = itemStatsPerAction)
  p <- p + aes(x    = sortLevels(item, total), 
               y    = total/1000)
  p <- p + barChart + coord_flip()
  p <- p + labs(x   = "Item", y = paste("Times", action, "(in thousands)"))
  p <- p + ggtitle(paste("Top", action, "items"))
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
}; rm(action, itemStatsPerAction)
