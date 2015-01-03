#! /usr/bin/Rscript

#### Get some tweets out for the dataz ====
# Load cached data
suppressPackageStartupMessages(library("wurstmineR"))
load("cache/workspace.RData")
source("options.R")
source("functions.R")

# Make twitteR work
if (!(file.exists("cache"))){
  dir.create("cache")
} else if (!(file.exists("cache/twitcred.RData"))){
  stop("Twitter credentials file is not present!")
} else {
  load("cache/twitcred.RData")
  if (registerTwitterOAuth(twitCred)){
    message("twitteR should work.")
  }
}

# Get random stats out, only one now.
diceroll <- round(runif(1, min = 1, max = 4))
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
if (wday(now(), T, F) == "Sunday" && hour(now()) >= 21){
  Sys.sleep(30)
  tweet(mostActiveDay(7))
}
if (hour(now()) == 22){
  Sys.sleep(30)
  tweet(dailyActivity(1))
}

# if (!is.null(getBirthdayNotification(birthdays)) && hour(now()) == 20){
#   Sys.sleep(60)
#   tweet(getBirthdayNotification(birthdays))
# }
