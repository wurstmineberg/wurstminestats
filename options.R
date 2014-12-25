# Defining some global options which may or may not prove useful.
# Because I pretend somebody else will want to use this 
# clusterfuck at some point and I'm a nice person.

#### Data sources ####
message("Setting global options")
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
options(url.general.deaths          = "http://api.wurstmineberg.de/server/deaths/overview.json")
options(url.general.sessions        = "http://api.wurstmineberg.de/server/sessions/overview.json")

#### Dependencies ####
message("Loading required packages")
# Loading all the things manually
library("ggplot2", quietly = TRUE)
library("scales", quietly = TRUE)           # For datetime scales on plots
library("grid", quietly = TRUE)             # for unit() in ggplot theme() functions
library("RColorBrewer", quietly = TRUE)     # Because colours
library("lubridate", quietly = TRUE)        # Time is hell, people.
library("jsonlite", quietly = TRUE)         # Because JSON. I don't know if RJSONIO or rjson would wourk, too. Sorry.
library("stringr", quietly = TRUE)          # Only used for the random stat functions
library("twitteR", quietly = TRUE)          # Used to tweet said random stats, not required for plotting etc.
library("plyr", quietly = TRUE)             # Data transformations are hard.
library("dplyr", quietly = TRUE)            # ^

##### Directories for plot locations ####
message("Checking if plot directories are present")
plotdirs <- c("Plots", "Plots/items", "Plots/items/mined", "Plots/items/crafted", "Plots/items/broken",
              "Plots/items/used", "Plots/achievements", "Plots/statspage", "Plots/statspage/scaled",
              "Plots/sessions")

for (i in plotdirs){
  if (!file.exists(i)){
    message("Seems directory ", i, " is missing, trying to create")
    dir.create(i)
  }
}

#------------------------------------------------------------#
##### Define some variables for ggplot2 layout and labels ####
#------------------------------------------------------------#
message("Defining some ggplot2 theme elements")
playerTheme <- theme(legend.position  = "right",
                     legend.key.size   = unit(.4, "cm"),
                     legend.text       = element_text(size = rel(.8)))

plotWidth <- 6; plotHeight <- 5;
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

colors.Minecraft <- c("Black"        = "#000000",
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

colors.Dimensions <- c("Overworld" = "#00CF1F",
                       "Nether"    = "#DB310B",
                       "End"       = "#8800AD")
