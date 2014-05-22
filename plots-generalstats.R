#-------------------------------------------------------------------------------#
#### General stats barcharts and versions scaled by online time // Statspage ####
#-------------------------------------------------------------------------------#
message("Generating general stats plots")

for(i in 1:nrow(strings.general)){
  
  stat      <- strings.general$id[i]
  statScale <- strings.general$scale[i]
  statName  <- strings.general$name[i]
  statUnit  <- strings.general$unit[i]
  filename  <- paste0("Plots/statspage/", stat,".png")
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = joinStatus, 
               x    = sortLevels(player, playerstats[[stat]]), 
               y    = playerstats[[stat]] / statScale)
  p <- p + barChart + statusFillScale + coord_flip()
  p <- p + scale_y_discrete(breaks = pretty_breaks()) 
  p <- p + xLable + labs(y = statUnit, title = statName)
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
  ## Weighted by hours played ##
  
  if(stat == "playOneMinute" | stat == "timeSinceDeath"){ next };
  
  filename <- paste0("Plots/statspage/scaled/", stat,"_scaled.png")
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = joinStatus, 
               x    = sortLevels(player, (playerstats[[stat]] / playerstats$playOneHour)), 
               y    = (playerstats[[stat]] / statScale) / playerstats$playOneHour)
  p <- p + barChart + statusFillScale + coord_flip()
  p <- p + xLable   + labs(y = paste(statUnit, "per hour"), title = paste(statName, "weighted by Online Time"))
  
  message("Saving", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(i, filename, p, stat, statScale, statUnit, statName)

# Distance Traveled Total
p <- ggplot(data  = playerstats)
p <- p + aes(fill = joinStatus, 
             x    = sortLevels(player, distanceTraveled), 
             y    = (distanceTraveled / 10^6))
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable   + labs(y="Distance (km)", title="Distance Traveled in Total")
ggsave(plot = p, file = "Plots/statspage/DistanceTraveled.png", height = plotHeight, width = plotWidth)

# Distance Traveled Total by Online Time
p <- ggplot(data  = playerstats)
p <- p + aes(fill = joinStatus, 
             x    = sortLevels(player, (distanceTraveled / playOneHour)), 
             y    = (distanceTraveled / 10^6) / playOneHour)
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable   + labs(y = "Distance (km) per hour", title = "Total Distance Traveled weighted by Online Time")
ggsave(plot = p, file = "Plots/statspage/DistanceTraveled_scaled.png", height = plotHeight, width = plotWidth)
