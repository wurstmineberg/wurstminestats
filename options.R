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

#### Dependencies ####
# List of libraries required
libraries <- c("jsonlite",
              "ggplot2",
              "scales",
              "grid",
              "plyr",
              "dplyr",
              "RColorBrewer",
              "RCurl",
              "httr",
              "lubridate",
              "shiny",
              "httpuv",
              "markdown",
              "gdata",
              "googleVis")

# If dependencies are missing, install them
for(dep in libraries){
  if(!require(dep, character.only=T)){
   # print(dep)
    install.packages(dep, character.only=T)
  }
}

if(!require("rCharts")){
  if(!require("devtools")){
  install.packages("devtools")
  } 
  require("devtools")
  install_github("rCharts", "ramnathv")
}

#------------------------------------------------------------#
##### Define some variables for ggplot2 layout and labels ####
#------------------------------------------------------------#

playerTheme <- theme(legend.position  = "right",
                     legend.key.size   = unit(.4, "cm"),
                     legend.text       = element_text(size = rel(.8)))

plotWidth <- 6; plotHeight <- 4;
barChart  <- geom_bar(colour="black", width=.7, stat="identity")
xLable    <- xlab("Player")

# Define colour scale to keep status/people colours static
statusColours         <- brewer.pal(9,"Set1")
statusFillScale       <- scale_fill_manual(   name = "Join Status", values = statusColours)
statusColourScale     <- scale_colour_manual( name = "Join Status", values = statusColours)

#activePeople          <- fixPeopleColors(activePeople, 0.95)
legendPeople          <- scale_fill_manual(name = "People", values = activePeople$color)
