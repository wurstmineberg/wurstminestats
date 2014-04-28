#### First of all, getting some tweets out ####
tweet(statOfTheDay())
tweet(statOfTheDay(type = "itemStats"))
tweet(randomAchievement("random"))

#-----------------------------------------------#
#### Meta stats (server stuff, death stats…) ####
#-----------------------------------------------#
cat("Generating meta stats plots \n")
# Online hours relative to age on server
p <- ggplot(data=playerstats)
p <- p + aes(fill=joinStatus, x=sortLevels(player, playOneHour/activePeople$serverAge), 
                              y=playOneHour/activePeople$serverAge)
p <- p + barChart + coord_flip() + statusFillScale + playerTheme
p <- p + labs(x="Player", y="Online Time (h) by Server Age (d)")
p <- p + ggtitle("Online Time by Server Age")
ggsave(p, file="Plots/OnlineTimebyServerAge.png", height=plotHeight, width=plotWidth)

# Server growth
p <- ggplot(data=activePeople) 
p <- p + aes(x = joinDate, y = 1:nrow(activePeople), label = name, colour=joinStatus)
p <- p + geom_point(stat="identity") + geom_text(size=3, hjust=-.2) 
p <- p + labs(y = "Whitelist Count", x = "Date", title = "Wurstmineberg Server Growth") 
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + statusColourScale + playerTheme
p <- p + scale_x_datetime(labels = date_format("%Y %b"),
                          breaks = date_breaks("month"),
                          minor_breaks = date_breaks("week"),
                          expand = c(.2,1))
p <- p + scale_y_discrete(breaks=pretty_breaks())
ggsave(p, file="Plots/WhitelistGrowth.png", height=6, width=12)

### Death stats ###

p <- ggplot(data=deaths)
p <- p + aes(fill=joinStatus, x=reorder(player, desc(daysSince), mean, order=T), y=daysSince)
p <- p + barChart + statusFillScale + coord_flip()
p <- p + xLable + labs(y="Days Since Death", title="Days Since Players' Latest Death")
ggsave(p, file="Plots/LatestDeaths.png", height=plotHeight, width=plotWidth)

#----------------#
#### Sessions ####
#----------------#
cat("Generating session plots \n")

# Plotting playedPerDay
p <- ggplot(data=playedPerDay)
p <- p + aes(x=date, y=timePlayed/60)
p <- p + geom_area(alpha=0.7) + geom_point() + geom_path(alpha=.8)
p <- p + geom_hline(yintercept = mean(playedPerDay$timePlayed/60), alpha=.5)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("weeks"),
                          minor_breaks = "days")
p <- p + scale_y_continuous(breaks=pretty_breaks())
p <- p + labs(y="Played Hours", x="Day", title="Total Time Played per Day")
ggsave(p, file="Plots/sessions/playTime.png", height=6, width=12)

# Testing "played per weekday"
fillColours   <- activePeople$color[activePeople$name %in% playedPerWeekday$person]

p <- ggplot(data=playedPerWeekday)
p <- p + aes(x=wday, y=timePlayed/60, fill=person)
p <- p + geom_bar(position="stack", stat="identity", colour="black")
p <- p + geom_hline(yintercept = avgPerWeekday/60, alpha=.5)
p <- p + labs(y="Played Hours", x="Weekdays", title="Total Time Played per Day of Week")
p <- p + scale_fill_manual(name="People", values=fillColours) + playerTheme
ggsave(p, file="Plots/sessions/playTime_weekdays.png", height=6, width=12)
rm(avgPerWeekday)

# Plotting playedPerPerson 
fillColours <- activePeople$color[activePeople$name %in% playedPerPerson$person]

p <- ggplot(data=playedPerPerson, aes(x=format(date, "%d"), y=timePlayed/60, fill=person))
p <- p + geom_bar(position="stack", stat="identity", colour="black")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
#                          breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks=pretty_breaks()) + playerTheme
p <- p + labs(y="Played Hours", x="Day of Month", title="Time Played per Day: 2014")
p <- p + scale_fill_manual(name="People", values=fillColours)
p <- p + facet_grid(month ~ .)
monthNum <- length(unique(playedPerPerson$month))
ggsave(p, file="Plots/sessions/playTime_perPerson.png", height=(monthNum*2), width=12)
rm(p, fillColours, monthNum)

# Plotting playedPerMonth 
fillColours   <- activePeople$color[activePeople$name %in% playedPerMonth$person]

p <- ggplot(data=playedPerMonth)
p <- p + aes(x=month, y=timePlayed/60, fill=person)
p <- p + geom_bar(position="stack", stat="identity", colour="black")
p <- p + geom_hline(yintercept = avgPerMonth/60, alpha=.5)
p <- p + labs(y="Played Hours", x="Months", title="Total Time Played per Month")
p <- p + scale_fill_manual(name="People", values=fillColours) + playerTheme
ggsave(p, file="Plots/sessions/playTime_months.png", height=6, width=12)

# JoinTime hours histogram
fillColours   <- activePeople$color[activePeople$name %in% playerSessions$person]

p <- ggplot(data=playerSessions)
p <- p + aes(x=hour(playerSessions$joinTime), fill=person)
p <- p + geom_histogram(colour="black", binwidth=.7)
p <- p + labs(y="Frequency", x="Hour of Day", title="Join Time Frequencies")
p <- p + scale_fill_manual(name="People", values=fillColours) + playerTheme
p <- p + scale_x_discrete(limits=seq(0, 23, by=1))
p <- p + scale_y_continuous(breaks=pretty_breaks())
ggsave(p, file="Plots/sessions/joinTime_hours.png", height=6, width=12)

# LeaveTime hours histogram because it's only right
p <- ggplot(data=playerSessions)
p <- p + aes(x=hour(playerSessions$leaveTime), fill=person)
p <- p + geom_histogram(colour="black", binwidth=.7)
p <- p + labs(y="Frequency", x="Hour of Day", title="Leave Time Frequencies")
p <- p + scale_fill_manual(name="People", values=fillColours) + playerTheme
p <- p + scale_x_discrete(limits=seq(0, 23, by=1))
p <- p + scale_y_continuous(breaks=pretty_breaks())
ggsave(p, file="Plots/sessions/leaveTime_hours.png", height=6, width=12)
rm(fillColours)

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

#-------------------------#
#### Achievement plots ####
#-------------------------#
cat("Generating achievement plots \n")

for(i in 1:nrow(strings.achievements)){

  ID          <- strings.achievements$id[i]
  name        <- strings.achievements$name[i]
  description <- strings.achievements$description[i]

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
  
  cat(paste("Saving", filename, "\n"))
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(i, p, filename, name, ID, description)

## Generate selected weighted charts ##

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
rm(cowRatio)

# Adventuring time progress in number of biomes visited
p <- ggplot(playerstats)
p <- p + aes(fill=joinStatus, x=sortLevels(player, exploreAllBiomesProgress), y=exploreAllBiomesProgress)
p <- p + barChart + coord_flip() + statusFillScale + xLable
p <- p + labs(title="Number of biomes explored \n (Only biomes relevant to Adventuring Time)", y="Biomes")
ggsave(p, file="Plots/achievements/exploreAllBiomesProgress.png", height=plotHeight, width=plotWidth)

#----------------------------------------#
#### Plotting item stats as they come ####
#----------------------------------------#
cat("Generating item stats plots \n")

for(i in 1:length(itemStats$stat)){

    stat        <- itemStats$stat[i]
    action      <- itemStats$action[i]
    itemName    <- itemStats$item[i]
    filename    <- paste("Plots/items/", action, "/", stat, ".png", sep="")
    title       <- paste("Times item was ", action, ": ", itemName, sep="")

    p <- ggplot(data=playerstats)
    p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[, stat], mean, order=T), y=playerstats[, stat])
    p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
    p <- p + labs(x="Player", y=paste("Times", action), title=title)
    
    cat(paste("Saving", filename, "\n"))
    ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(p, i, title, filename, stat, action, itemName)

## Now to look at the different item actions ##
cat("Generating item actions plots \n")

# subset for each action, get top 20 items for each action, and plot them
for(action in itemActions$name){
  itemStatsPerAction <- itemStats[itemStats$action == action,]
  itemStatsPerAction <- head(arrange(itemStatsPerAction, desc(total)), 20)
  filename           <- paste("Plots/items/top_", action, ".png", sep="")
  
  p <- ggplot(data=itemStatsPerAction)
  p <- p + aes(x=sortLevels(item, total), y=total/1000)
  p <- p + barChart + coord_flip()
  p <- p + labs(x="Item", y=paste("Times", action, "(in thousands)", sep=" "))
  p <- p + ggtitle(paste("Top", action, "items", sep=" "))
  
  cat(paste("Saving", filename, "\n"))
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)
  
}; rm(action, itemStatsPerAction)

#------------------------------------------------------#
#### Dealing with entitiy stats. Kind of a big one. ####
#------------------------------------------------------#
cat("Generating entity stat plots \n")

# Get columns for killEntity and KilledBy categories respectively
killEntity      <- grep("killEntity", names(playerstats))
killedByEntity  <- grep("entityKilledBy", names(playerstats))

# Get a list of mob names from those columns
killEntityMobs      <- sub("killEntity.", "", names(playerstats[killEntity]))
killedByEntityMobs  <- sub("entityKilledBy.", "", names(playerstats[killedByEntity]))

# Substitute mob names with more familiar names
for(i in 1:nrow(strings.mobs)){
  
  killedByEntityMobs  <- sub(strings.mobs$id[i], strings.mobs$name[i], killedByEntityMobs)
  killEntityMobs      <- sub(strings.mobs$id[i], strings.mobs$name[i], killEntityMobs)

}; rm(i);

#--------------------------------------------#
#### Generate graphs for killEntity stats ####
#--------------------------------------------#


for(i in 1:length(killEntity)){

  filename <- paste("Plots/mobs/", names(playerstats[killEntity[i]]),".png", sep="")
  
  p <- ggplot(data=playerstats) 
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[,killEntity[i]], mean, order=T), y=playerstats[,killEntity[i]], mean, order=T)
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
  p <- p + xLable + labs(y="Kills", title=paste("Kills of:",killEntityMobs[i]))
  
  cat(paste("Saving", filename, "\n"))
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)

}; rm(i, p, filename)

#------------------------------------------------#
#### Generate graphs for entityKilledBy stats ####
#------------------------------------------------#

for(i in 1:length(killedByEntity)){
  
  filename <- paste("Plots/mobs/", names(playerstats[killedByEntity[i]]),".png", sep="")
  
  p <- ggplot(data=playerstats) 
  p <- p + aes(fill=joinStatus, x=reorder(player, playerstats[,killedByEntity[i]], mean, order=T), 
                                y=playerstats[,killedByEntity[i]])
  p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
  p <- p + xLable + labs(y="Deaths", title=paste("Killed by:",killedByEntityMobs[i]))
  
  cat(paste("Saving", filename, "\n"))
  ggsave(plot=p, file=filename, height=plotHeight, width=plotWidth)
  
}; rm(i, p, filename)


#----------------------------------------------#
#### Generate top killed / deaths by charts ####
#----------------------------------------------#

# Kills per mob #
mobsKilled <- data.frame(killedMob =  killEntityMobs,
                         nKills    =  unlist(colwise(sum)(playerstats[killEntity]), use.names=F))

p <- ggplot(data=mobsKilled) 
p <- p + aes(x=reorder(killedMob, nKills, mean, order=T), y=nKills)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + labs(x="Mobs", y="Kills", title="Killed Mobs")
ggsave(plot=p, file="Plots/mobs/Kills_byMob.png", height=plotHeight, width=plotWidth)

# … Let's filter out Endermen #
mobsKilledFiltered <- mobsKilled[mobsKilled$killedMob != "Enderman",]

p <- ggplot(data=mobsKilledFiltered) 
p <- p + aes(x=reorder(killedMob, nKills, mean, order=T), y=nKills)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + labs(x="Mobs", y="Kills", title="Killed Mobs (except Endermen)")
ggsave(plot=p, file="Plots/mobs/Kills_byMob_except_Endermen.png", height=plotHeight, width=plotWidth)

#### Deaths by mob ####
mobsKilledBy <- data.frame(killedByMob  = killedByEntityMobs,
                           nKilledBy    = unlist(colwise(sum)(playerstats[killedByEntity]), use.names=F))

p <- ggplot(data=mobsKilledBy) 
p <- p + aes(x=reorder(killedByMob, nKilledBy, mean, order=T), y=nKilledBy)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + labs(x="Mobs", y="Deaths", title="Deaths by Mob")
ggsave(plot=p, file="Plots/mobs/Deaths_byMob.png", height=plotHeight, width=plotWidth)

## Classify by hostile and friendly mobs ##
killFriendly  <- c("killEntity.Bat", "killEntity.Sheep", "killEntity.Pig", "killEntity.Chicken", "killEntity.Cow", 
                  "killEntity.EntityHorse", "killEntity.Squid", "killEntity.MushroomCow", "killEntity.Villager", 
                  "killEntity.Ozelot", "killEntity.Wolf")
killHostile   <- c("killEntity.PigZombie", "killEntity.Blaze", "killEntity.Enderman", "killEntity.Spider", 
                  "killEntity.Zombie", "killEntity.Skeleton", "killEntity.Creeper", "killEntity.Silverfish", 
                  "killEntity.Witch", "killEntity.Slime", "killEntity.CaveSpider", "killEntity.MushroomCow", 
                  "killEntity.LavaSlime", "killEntity.Ghast", "killEntity.Endermite")

for(i in 1:nrow(playerstats)){

  playerstats$killFriendly[i]   <- sum(playerstats[i, killFriendly])
  playerstats$killHostile[i]    <- sum(playerstats[i, killHostile])

}; rm(i);

## Generate graphs for that ##

# Friendly Mobs #
p <- ggplot(data=playerstats) 
p <- p + aes(fill=joinStatus, x=reorder(player, killFriendly, mean, order=T), y=killFriendly)
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + xLable + labs(y="Kills", title="Total Friendly Mobs Killed")
ggsave(plot=p, file="Plots/mobs/KillFriendlies.png", height=plotHeight, width=plotWidth)

# Hostile Mobs #
p <- ggplot(data=playerstats) 
p <- p + aes(fill=joinStatus, x=reorder(player, killHostile, mean, order=T), y=killHostile)
p <- p + barChart + statusFillScale + coord_flip() + scale_y_discrete(breaks= pretty_breaks())
p <- p + xLable + labs(y="Kills", title="Total Hostile Mobs Killed")
ggsave(plot=p, file="Plots/mobs/KillHostiles.png", height=plotHeight, width=plotWidth)

rm(killedByEntity, killedByEntityMobs, killEntity, killEntityMobs)
