# Setting some important default packages so they're already loaded on startup

options(defaultPackages = c(getOption("defaultPackages"), 
                            "jsonlite",
                            "ggplot2",
                            "scales",
                            "grid",
                            "plyr",
                            "RColorBrewer",
                            "httr",
                            "lubridate",
                            "grDevices",
                            "shiny",
                            "httpuv",
                            "markdown")
)

# Loading some other libraries but with suppressed messages because they're too noisy
suppressMessages(library(RCurl))
suppressPackageStartupMessages(library("gdata"))
suppressPackageStartupMessages(library("googleVis"))
