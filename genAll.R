#! /usr/bin/Rscript
## Headless / IDE-less executable script to autogenerate all the plots

# These are biggies and are required for everything else to work
source("dataPrep.R")
source("functions.R")

# These are not
source("metaStats.R")
source("achievements.R")
source("generalStats.R")
source("entityStats.R")
source("itemStats.R")
source("sessions.R")
