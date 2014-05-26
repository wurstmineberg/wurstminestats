# Defining some global options which may or may not prove useful.
# Because I pretend somebody else will want to use this 
# clusterfuck at some point and I'm a nice person.

#### Data sources ####

## Strings & other descriptive JSON stuff ##
options(url.strings.general         = "http://assets.wurstmineberg.de/json/strings.json")
options(url.strings.mobs            = "http://assets.wurstmineberg.de/json/mobs.json")
options(url.strings.achievements    = "http://assets.wurstmineberg.de/json/achievements.json")
options(url.strings.items           = "http://assets.wurstmineberg.de/json/items.json")
options(url.strings.biomes          = "http://assets.wurstmineberg.de/json/biomes.json")
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
# Loading some libraries but with suppressed messages because they're too noisy
suppressPackageStartupMessages(library("googleVis"))

# Loading all the things manually
library("ggplot2")
library("scales")           # For datetime scales on plots
library("grid")             # for unit() in ggplot theme() functions
library("RColorBrewer")     # Because colours
library("rCharts")          # For interactive jsified plotting glory (http://ramnathv.github.io/rCharts/), install via install_github("rCharts", "ramnathv")
library("lubridate")        # Time is hell, people.
library("jsonlite")
library("stringr")          # Only used for the random stat functions
library("twitteR")          # Used to tweet said random stats, not required for plotting etc.
library("plyr")             # Data transformations are hard.
library("dplyr")            # ^

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
statusColours       <- brewer.pal(9,"Set1")
statusColours       <- c("founding"    = statusColours[1], 
                          "later"      = statusColours[2], 
                          "postfreeze" = statusColours[3], 
                          "invited"    = statusColours[4],
                          "former"     = statusColours[5])
statusFillScale     <- scale_fill_manual(   name = "Join Status", values = statusColours)
statusColourScale   <- scale_colour_manual( name = "Join Status", values = statusColours)

#activePeople          <- fixPeopleColors(activePeople, 0.95)
legendPeople          <- scale_fill_manual(name = "People", values = activePeople$color)

#### Defining a color scale for the colors used by Minecraft ####
# Source: http://minecraft.gamepedia.com/Formatting_codes#Color_codes

colors.Minecraft <- c("Black"      = "#000000",
                    "Dark Blue"    = "#0000AA",
                    "Dark Green"   = "#00AA00",
                    "Dark Aqua"    = "#00AAAA",
                    "Dark Red"     = "#AA0000",
                    "Dark Purple"  = "#AA00AA",
                    "Gold"         = "#FFAA00",
                    "Gray"         = "#AAAAAA",
                    "Dark Gray"    = "#555555",
                    "Blue"         = "#5555FF",
                    "Green"        = "#55FF55",
                    "Aqua"         = "#55FFFF",
                    "Red"          = "#FF5555",
                    "Light Purple" = "#FF55FF",
                    "Yellow"       = "#FFFF55",
                    "White"        = "#FFFFFF")
