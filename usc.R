#### Stats for USC6 ####
source("functions.R")
source("options.R")
library(wurstmineR)
require(plyr)

#### General preparations ####

# Set plot base dir
basedir      <- "usc6"
plotlocation <- paste0(basedir, "/plots/")
datadir      <- "usc6/stats/"

if (!file.exists(plotlocation)){
  dir.create(plotlocation, recursive = TRUE)
}

# Teams
teams       <- list()
teams[[1]]  <- list("members" = c("Fenhl", "Jemus42", "Farthen08", "niklasmeyer"),
                    "color"   = colors.Minecraft[["Aqua"]],
                    "name"    = "AnderZEL")
teams[[2]]  <- list("members" = c("papierschiff", "l3viathan2142", "m4dm41ik", "felis_blue"),
                    "color"   = colors.Minecraft[["Light Purple"]],
                    "name"    = "All Teh Dmianods")

# Defining team color scales ##
teamColors        <- sapply(teams, "[[", "color")
names(teamColors) <- sapply(teams, "[[", "name")
teamFillScale     <- scale_fill_manual(   name = "Team", values = teamColors)
teamColourScale   <- scale_colour_manual( name = "Team", values = teamColors)

#### Actualy doing stuff ####
playerstats <- data.frame()
for(file in dir(datadir)){
  stat <- readStatsFile(paste0(datadir, file))
  playerstats <<- rbind.fill(stat, playerstats)
}; rm(stat)

playerstats$player <- sapply(playerstats$UUID, getNameFromUUID)
playerstats[is.na(playerstats)] <- 0

items        <- playerstats[c(grep("Item.minecraft", names(playerstats)), 
                              grep("mineBlock.minecraft", names(playerstats)))]
items$player <- playerstats$player

generalstats        <- playerstats[names(playerstats) %in% strings.general$id]
generalstats$player <- playerstats$player

achievements        <- playerstats[names(playerstats) %in% strings.achievements$id]
achievements$player <- playerstats$player

entities        <- playerstats[c(grep("killEntity", names(playerstats)), 
                                 grep("KilledBy", names(playerstats)))]
entities$player <- playerstats$player

itemStats <- arrange(getItemStats(items),   desc(total))
mobStats  <- arrange(getMobStats(entities), desc(total))

# Team assignment
playerstats$team[playerstats$player %in% teams[[1]]$members] <- teams[[1]]$name
playerstats$team[playerstats$player %in% teams[[2]]$members] <- teams[[2]]$name
playerstats$team <- factor(playerstats$team, 
                           levels = sapply(teams, "[[", "name"), ordered = T)

write.csv(arrange(itemStats, leadingPlayers, desc(total)), file = paste0(basedir, "/itemStats.csv"))
write.csv(arrange(mobStats,  leadingPlayers, desc(total)), file = paste0(basedir, "/mobStats.csv"))

#----------------------------------------#
#### Plotting item stats as they come ####
#----------------------------------------#
message("Generating item stats plots")

for(i in 1:length(itemStats$stat)){
  
  stat        <- itemStats$stat[i]
  action      <- itemStats$action[i]
  itemName    <- itemStats$item[i]
  subdir      <- "items/"
  filename    <- paste0(plotlocation, subdir, stat, ".png")
  title       <- paste0("Times item was ", action, ": ", itemName)
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = team, 
               x    = sortLevels(player, playerstats[[stat]]), 
               y    = playerstats[[stat]])
  p <- p + barChart + teamFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
  p <- p + labs(x   = "Player", y = paste("Times", action), title = title)
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(p, i, title, filename, stat, action, itemName)

## Now to look at the different item actions ##
message("Generating item actions plots")

# subset for each action, get top 20 items for each action, and plot them
for(action in unique(itemStats$action)){
  itemStatsPerAction <- itemStats[itemStats$action == action,]
  itemStatsPerAction <- arrange(itemStatsPerAction, desc(total))
  filename           <- paste0(plotlocation, "items/top_", action, ".png")
  
  p <- ggplot(data  = itemStatsPerAction)
  p <- p + aes(x    = sortLevels(item, total), 
               y    = total)
  p <- p + barChart + coord_flip()
  p <- p + labs(x   = "Item", y = paste("Times", action))
  p <- p + ggtitle(paste("Top", action, "items"))
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight * 1.5, width = plotWidth)
}; rm(action, itemStatsPerAction)

#------------------------------------------------------#
#### Dealing with entitiy stats. Kind of a big one. ####
#------------------------------------------------------#
message("Generating entity stat plots")

for(i in 1:length(mobStats$stat)){
  
  stat       <- mobStats$stat[i]
  action     <- mobStats$action[i]
  mobName    <- mobStats$mob[i]
  filename   <- paste0(plotlocation, "mobs/", stat, ".png")
  
  if (action == "killed"){
    title    <- paste0("Times mob “", mobName, "” was killed")
    ylab     <- "Kills"
  } else if (action == "killed by"){
    title    <- paste0("Deaths caused by mob “", mobName, "”")
    ylab     <- "Caused Deaths"
  }
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = team, 
               x    = sortLevels(player, playerstats[[stat]]), 
               y    = playerstats[[stat]])
  p <- p + barChart + teamFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
  p <- p + labs(x   = "Player", y = ylab, title = title)
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(p, i, title, filename, stat, action, mobName)

#### Generate top killed / deaths by charts ####

# Kills per mob #
mobsKilled <- mobStats[mobStats$action == "killed", ]

p <- ggplot(data = mobsKilled)
p <- p + aes(x = sortLevels(mob, total), y = total)
p <- p + barChart + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
p <- p + labs(x = "Mobs", y = "Kills", title = "Killed Mobs")
ggsave(plot = p, file = paste0(plotlocation, "mobs/Kills_byMob.png"), height = plotHeight, width = plotWidth)
rm(p, mobsKilled)

#-------------------------------------------------------------------------------#
#### General stats barcharts and versions scaled by online time // Statspage ####
#-------------------------------------------------------------------------------#

message("Generating general stats plots")

strings.general <- strings.general[strings.general$id %in% names(generalstats), ]
strings.general[grep("damage", strings.general$id), "scale"] <- rep(2, 2)
strings.general[grep("damage", strings.general$id), "unit"]  <- rep("Hearts", 2)
strings.general[grep("OneCm",  strings.general$id), "scale"] <- rep(100, 2)
strings.general[grep("OneCm",  strings.general$id), "unit"]  <- rep("Meters", 2)


for(i in 1:nrow(strings.general)){
  
  stat      <- strings.general$id[i]
  statScale <- strings.general$scale[i]
  statName  <- strings.general$name[i]
  statUnit  <- strings.general$unit[i]
  filename  <- paste0(plotlocation, "generalstats/", stat,".png")
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = team, 
               x    = sortLevels(player, playerstats[[stat]]), 
               y    = playerstats[[stat]] / statScale)
  p <- p + barChart + teamFillScale + coord_flip()
  p <- p + scale_y_discrete(breaks = pretty_breaks()) 
  p <- p + xLable + labs(y = statUnit, title = statName)
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(i, filename, p, stat, statScale, statUnit, statName)

#-------------------------#
#### Achievement plots ####
#-------------------------#
message("Generating achievement plots")
strings.achievements <- strings.achievements[strings.achievements$id %in% names(achievements), ]
for(i in 1:nrow(strings.achievements)){
  
  ID          <- strings.achievements$id[i]
  name        <- strings.achievements$displayname[i]
  description <- strings.achievements$description[i]
  
  if(ID == "exploreAllBiomes"){ next };
  
  filename <- paste(plotlocation, "achievements/", ID,".png", sep="")
  
  p <- ggplot(data  = playerstats)
  p <- p + aes(fill = team, 
               x    = sortLevels(player, playerstats[[ID]]), 
               y    = playerstats[[ID]]) 
  p <- p + barChart + teamFillScale + coord_flip() + scale_y_discrete(breaks = pretty_breaks())
  p <- p + xLable   + ylab("Times Achieved")
  if(nchar(description, type  = "width") > 44){
    p <- p + theme(plot.title = element_text(size = 12))
  }
  p <- p + ggtitle(paste("Achievement:", name, "\n", description))
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)
  
}; rm(i, p, filename, name, ID, description)
