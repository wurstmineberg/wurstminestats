# Defining some global options which may or may not prove useful.

#### Data sources ####
message("Setting global options")
## Strings & other descriptive JSON stuff ##

urls <- jsonlite::fromJSON("config/urls.json")

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
#library("plyr", quietly = TRUE)             # Data transformations are hard.
library("dplyr", quietly = TRUE)            # ^

##### Directories for plot locations ####
message("Checking if plot directories are present")
plotdirs <- c("output", "output/items", "output/items/mined", "output/items/crafted", "output/items/broken",
              "output/items/used", "output/achievements", "output/general", "output/general/scaled",
              "output/sessions", "output/entities", "output/items/picked_up", "output/items/dropped",
              "output/wcec")

for (dir in plotdirs){
  if (!file.exists(dir)){
    message("Seems directory ", dir, " is missing, trying to create")
    dir.create(dir)
  }
}; rm(dir)

#------------------------------------------------------------#
##### Define some variables for ggplot2 layout and labels ####
#------------------------------------------------------------#
message("Defining some ggplot2 theme elements")
playerTheme <- theme(legend.position = "right",
                     legend.key.size = unit(.4, "cm"),
                     legend.text     = element_text(size = rel(.8)))

plotWidth <- 6; plotHeight <- 5.5;
barChart  <- geom_bar(colour = "black", width = .7, stat = "identity")
xLable    <- xlab("Person")

# Define colour scale to keep status/people colours static
statusColours       <- brewer.pal(9,"Set1")
statusColours       <- c("founding"    = statusColours[1], 
                          "later"      = statusColours[2], 
                          "postfreeze" = statusColours[3], 
                          "invited"    = statusColours[4],
                          "former"     = statusColours[5])
statusFillScale     <- scale_fill_manual  (name = "Join Status", values = statusColours)
statusColourScale   <- scale_colour_manual(name = "Join Status", values = statusColours)

#activePeople       <- fixPeopleColors(activePeople, 0.95)
legendPeople        <- scale_fill_manual(name = "People", values = people_active$color)

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

colors.Dimensions   <- c("Overworld" = "#00CF1F",
                         "Nether"    = "#DB310B",
                         "End"       = "#8800AD")
