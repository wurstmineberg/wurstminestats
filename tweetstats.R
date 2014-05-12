#### Get some tweets out for the dataz ====
# Make twitteR work
if(!exists("twitCred")){
  load("cache/twitcred.RData")
  print(ifelse(registerTwitterOAuth(twitCred), "Tweets should work", "Something broke"))
}
# Get tweets out
tweet(statOfTheDay())
Sys.sleep(30)
tweet(randomItemStat())
Sys.sleep(30)
tweet(randomMobStat())
Sys.sleep(30)
tweet(randomAchievement("random"))
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