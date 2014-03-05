wurstminestats
==============

Plotting the Wurstmineberg Minecraft Server

![Whitelist growth](https://raw.github.com/wurstmineberg/wurstminestats/master/Plots/WhitelistGrowth.png)  

## What this is
Primarily it's me playing around with [R](http://www.r-project.org).  
Besides that, this will be an addition to the [wurstmineberg](http://wurstminebger.de) player statistics page at our [stats page](http://wurstminebger.de/stats). 
The plots relevant to the stats page are in [Plots/statspage](https://github.com/jemus42/wurstminestats/tree/master/Plots/statspage), which will probably be up on the website some time in the near future, when all the plots are done.  
There are also some general more or less informative plots in [Plots](https://github.com/jemus42/wurstminestats/tree/master/Plots).

To make it easier to view the plots while they're being made, the "Plots" directory is also mirrored to our image site at [i.wurstmineberg.de](http://i.wurstmineberg.de/wurstminestats/).

## Dependencies
Besides a recent version of R (duh), there are some things you need for this to be portable to any other Minecraft server. Let's go over the foreign data sources in this briefly.  

### Data sources
* http://api.wurstmineberg.de/server/playerstats/general.json
* http://api.wurstmineberg.de/server/playerstats/achievement.json
* http://api.wurstmineberg.de/server/playerstats/item.json
* http://api.wurstmineberg.de/server/playerstats/entity.json  

These are [Minecraft API](http://api.wurstmineberg.de) endpoints, which expose the actual player statistics we're interested in visualising. If you have this set up for your own server, you're pretty much good to go. Almost.

### General information sources
* http://wurstmineberg.de/assets/serverstatus/people.json  

This is our people database, which holds metadata about the people on our server, such as join date, nickname (ID), minecraft username and what have you. This is required to sort and classify the datasets. It is required in many dataset transformations, because I substitute minecraft usernames with their nicknames (IDs), sort them by join dates and group by their join status, so… If you don't want to set up a people.json, you'd need to get rid of these parts manually.

* http://wurstmineberg.de/static/json/achievements.json

Just some substitution-friendly strings to make achievements more human readable.  

* http://wurstmineberg.de/static/json/strings.json

Similar to achievements.json, this provides names and classification for player stats, as well as some mob name substitutions.

## Contribution
* If you're familiar with R or [ggplot2](http://docs.ggplot2.org/), I appreciate any hints on how to do something better, cleaner, more efficient or just more visually pleasing. 
* Requests for graphs are appreciated, and if I figure out how to make them happen, I will
* Visual errors in graphs can be reported via the issue tracker, you know, to keep track
* Errors with the data itself are more or less out of my reach.

