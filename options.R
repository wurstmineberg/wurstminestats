# Defining some global options which may or may not prove useful.
# Because I pretend somebody else will want to use this 
# clusterfuck at some point and I'm a nice person.

#### Data sources ####

## Strings & other descriptive JSON stuff ##
options(url.strings.general         = "http://assets.wurstmineberg.de/json/strings.json")
options(url.strings.achievements    = "http://assets.wurstmineberg.de/json/achievements.json")
options(url.strings.items           = "http://assets.wurstmineberg.de/json/items.json")

# In contrast to the above files, the next one is server specific, because people.
options(url.strings.people          = "http://wurstmineberg.de/assets/serverstatus/people.json")

## API-URLs ##
options(url.stats.general           = "http://api.wurstmineberg.de/server/playerstats/general.json")
options(url.stats.achievements      = "http://api.wurstmineberg.de/server/playerstats/achievement.json")
options(url.stats.entities          = "http://api.wurstmineberg.de/server/playerstats/entity.json")
options(url.stats.items             = "http://api.wurstmineberg.de/server/playerstats/item.json")

options(url.general.deaths.latest   = "http://api.wurstmineberg.de/server/deaths/latest.json")
options(url.general.sessions        = "http://api.wurstmineberg.de/server/sessions/overview.json")
