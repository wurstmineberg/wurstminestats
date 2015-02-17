#! /usr/bin/Rscript
## Headless / IDE-less executable script to autogenerate all the plots

# This is the biggie
source("dataPrep.R")        # Loads all libraries, calls functions.R, which is needed for pretty much everything

# These are not, but they all require dataPrep.R
source("plots-sessions.R")
source("plots-meta.R")
source("plots-generalstats.R")
source("plots-achievements.R")
rmarkdown::render("misc-stats.Rmd", output_dir = "Plots/")

if (hour(now()) < 4){
  source("plots-entities.R")
}
if (wday(now(), T, F) == "Sunday" && hour(now()) > 20){
  source("plots-items.R")
}

# Sooner or later, I want a giant logfile.
#writePlayerstatsLog()
#write.csv(itemStats, "data/itemStats.csv")
#write.csv(playerSessions, "data/playerSessions.csv")
