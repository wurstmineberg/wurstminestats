## Plots based on achievement stats

# Refresh data if older than 6 hours
if((as.numeric(format(Sys.time(), "%s")) - as.numeric(now))/60/60 > 6){
  source("dataPrep.R");
}

source("functions.R")

# Cakes baked
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,bakeCake), y=bakeCake)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Cakes", title="Cakes baked per player")
ggsave(file="Plots/achievements/CakesBaked.png", height=plotHeight, width=plotWidth)

# Cows killed
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,killCow), y=killCow)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Cows", title="Cows killed per player")
ggsave(file="Plots/achievements/CowsKilled.png", height=plotHeight, width=plotWidth)

# Cow kill to cow breed ratio
cowRatio <- as.numeric(sub("NaN", "0", playerstats$breedCow/(playerstats$killCow)))
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,cowRatio), y=cowRatio)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Breeds per kills", title="Cow Breed to Cow Kill Ratio")
ggsave(file="Plots/achievements/CowsRatio.png", height=plotHeight, width=plotWidth)
rm(cowRatio);

# Hoes made
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,buildHoe), y=buildHoe)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Hoes", title="Hoes made per player")
ggsave(file="Plots/achievements/HoesBuilt.png", height=plotHeight, width=plotWidth)

# Withers killed
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,killWither), y=killWither)) + 
  barChart + legendTitle + coord_flip() +
  xLable + labs(y="Withers", title="Withers killed per player")
ggsave(file="Plots/achievements/WithersKilled.png", height=plotHeight, width=plotWidth)

# Number of opened inventories per played hour
ggplot(data=playerstats, aes(fill=joinStatus, x=reorder(player,openInventory/playOneHour), y=openInventory/playOneHour)) + 
  barChart + legendTitle + coord_flip() + scale_y_discrete(breaks= pretty_breaks()) +
  xLable + labs(y="Inventories Opened per Hour", title="How Often Players Opened Their Inventories per Hour")
ggsave(file="Plots/achievements/OpenInventory_weighted.png", height=plotHeight, width=plotWidth)