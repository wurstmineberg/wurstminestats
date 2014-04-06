#! /usr/bin/Rscript
## Headless / IDE-less executable script to autogenerate all the plots

# This is the biggie
source("dataPrep.R")        # Loads all libraries, calls functions.R, which is needed for pretty much everything

# These are not, but they all require dataPrep.R
source("drawPlots.R")       # Generates all the plots in ./Plots. All of them.

# Sooner or later, I want a giant logfile.
writePlayerstatsLog()
write.csv(itemStats, "data/itemStats.csv")
write.csv(playerSessions, "data/playerSessions.csv")
