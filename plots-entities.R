#------------------------------------------------------#
#### Dealing with entitiy stats. Kind of a big one. ####
#------------------------------------------------------#
message("Generating entity stat plots")

for(i in 1:length(mobStats$stat)){
  
  stat       <- mobStats$stat[i]
  action     <- mobStats$action[i]
  mobName    <- mobStats$mob[i]
  filename   <- paste0("Plots/mobs/", stat, ".png")
  
  if (action == "killed"){
    title    <- paste0("Times mob “", mobName, "” was killed")
    ylab     <- "Kills"
  } else if (action == "killed by"){
    title    <- paste0("Deaths caused by mob “", mobName, "”")
    ylab     <- "Caused Deaths"
  }
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = joinStatus, 
               x    = sortLevels(player, playerstats[[stat]]), 
               y    = playerstats[[stat]])
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
  p <- p + labs(x   = "Player", y = ylab, title = title)
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(p, i, title, filename, stat, action, mobName)

#----------------------------------------------#
#### Generate top killed / deaths by charts ####
#----------------------------------------------#

# Kills per mob #
mobsKilled <- mobStats[mobStats$action == "killed", ]

p <- ggplot(data = mobsKilled)
p <- p + aes(x = sortLevels(mob, total), y = total)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
p <- p + labs(x = "Mobs", y = "Kills", title = "Killed Mobs")
ggsave(plot = p, file = "Plots/mobs/Kills_byMob.png", height = plotHeight, width = plotWidth)

# … Let's filter out Endermen #
mobsKilledFiltered <- mobsKilled[mobsKilled$mob != "Enderman", ]

p <- ggplot(data = mobsKilledFiltered) 
p <- p + aes(x = reorder(mob, total), y = total)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
p <- p + labs(x = "Mobs", y = "Kills", title = "Killed Mobs (except Endermen)")
ggsave(plot=p, file="Plots/mobs/Kills_byMob_except_Endermen.png", height=plotHeight, width=plotWidth)

#### Deaths by mob ####
mobsKilledBy <- mobStats[mobStats$action == "killed by", ]

p <- ggplot(data = mobsKilledBy) 
p <- p + aes(x = sortLevels(mob, total), y = total)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
p <- p + labs(x = "Mobs", y = "Deaths", title = "Deaths by Mob")
ggsave(plot = p, file = "Plots/mobs/Deaths_byMob.png", height = plotHeight, width = plotWidth)

## Classify by hostile and friendly mobs ##
killFriendly  <- c("killEntity.Bat", "killEntity.Sheep", "killEntity.Pig", "killEntity.Chicken", "killEntity.Cow", 
                   "killEntity.EntityHorse", "killEntity.Squid", "killEntity.MushroomCow", "killEntity.Villager", 
                   "killEntity.Ozelot")
killHostile   <- c("killEntity.PigZombie", "killEntity.Blaze", "killEntity.Enderman", "killEntity.Spider", 
                   "killEntity.Zombie", "killEntity.Skeleton", "killEntity.Creeper", "killEntity.Silverfish", 
                   "killEntity.Witch", "killEntity.Slime", "killEntity.CaveSpider", "killEntity.MushroomCow", 
                   "killEntity.LavaSlime", "killEntity.Ghast", "killEntity.Endermite", "killEntity.Wolf")

for(i in 1:nrow(playerstats)){
  
  playerstats$killFriendly[i]   <- sum(playerstats[i, killFriendly])
  playerstats$killHostile[i]    <- sum(playerstats[i, killHostile])
  
}; rm(i);

## Generate graphs for that ##

# Friendly Mobs #
p <- ggplot(data  = playerstats) 
p <- p + aes(fill = joinStatus, x = sortLevels(player, killFriendly), y = killFriendly)
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
p <- p + xLable + labs(y = "Kills", title = "Total Friendly Mobs Killed")
ggsave(plot = p, file ="Plots/mobs/KillFriendlies.png", height = plotHeight, width = plotWidth)

# Hostile Mobs #
p <- ggplot(data  = playerstats) 
p <- p + aes(fill = joinStatus, x = sortLevels(player, killHostile), y = killHostile)
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
p <- p + xLable + labs(y = "Kills", title = "Total Hostile Mobs Killed")
ggsave(plot = p, file = "Plots/mobs/KillHostiles.png", height = plotHeight, width = plotWidth)
