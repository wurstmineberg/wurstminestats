#! /usr/bin/Rscript
#### Get some tweets out for the dataz ====
# Load cached data
load("cache/workspace.RData")
source("options.R")
source("functions.R")

# Make twitteR work
if(!exists("twitCred")){
  load("cache/twitcred.RData")
  print(ifelse(registerTwitterOAuth(twitCred), "Tweets should work", "Something broke"))
}

# Get random stats out
tweet(statOfTheDay(category = sample(c("general", "items", "mobs"), 1)))
Sys.sleep(60)
tweet(randomItemStat())
Sys.sleep(60)
tweet(randomMobStat())
Sys.sleep(60)
tweet(randomAchievement("random"))

# Special tweets
if (wday(now(), T, F) == "Sunday" && hour(now()) %in% 20:24){
  Sys.sleep(30)
  tweet(mostActiveDay(7))
}
if (hour(now()) %in% 20:24){
  Sys.sleep(30)
  tweet(dailyActivity(1))
}
if (!is.null(getBirthdayNotification(birthdays))){
  Sys.sleep(60)
  tweet(getBirthdayNotification(birthdays))
}