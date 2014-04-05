#! /usr/bin/Rscript
## Headless / IDE-less executable script to autogenerate all the plots

# This is the biggie
source("dataPrep.R")        # Loads all libraries, calls functions.R, which is needed for pretty much everything

# These are not, but they all require dataPrep.R
source("metaStats.R")       # 
source("achievements.R")    # 
source("generalStats.R")    # 
source("entityStats.R")     # Adds killFriendly and killHostile columns to playerstats
source("itemStats.R")       # 
source("sessions.R")        # Does not interfere or rely on playerstats dataframe, only needs activePeople

# Sooner or later, I want a giant logfile.
writePlayerstatsLog()
write.csv(itemStats, "data/itemStats.csv")
