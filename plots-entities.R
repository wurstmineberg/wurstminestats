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

