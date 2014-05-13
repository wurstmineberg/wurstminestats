#! /usr/bin/Rscript
#### Get some tweets out for the dataz ====
# Load cached data
load("cache/workspace.RData")
source("options.R")
source("functions.R")
library("wurstmineR")

# Make twitteR work
load("cache/twitcred.RData")
registerTwitterOAuth(twitCred)

# Get random stats out, only one now.
diceroll <- round(runif(1,1,4))
if (diceroll == 1){
  tweet(statOfTheDay(category = sample(c("general", "items", "mobs"), 1)))
} else if (diceroll == 2){
  tweet(randomItemStat())
} else if (diceroll == 3){
  tweet(randomMobStat())
} else if (diceroll == 4){
  tweet(randomAchievement("random"))
}

# Special tweets
if (wday(now(), T, F) == "Sunday" && hour(now()) == 22){
  Sys.sleep(30)
  tweet(mostActiveDay(7))
}
if (hour(now()) == 22){
  Sys.sleep(30)
  tweet(dailyActivity(1))
}
if (!is.null(getBirthdayNotification(birthdays)) && hour(now()) == 20){
  Sys.sleep(60)
  tweet(getBirthdayNotification(birthdays))
}