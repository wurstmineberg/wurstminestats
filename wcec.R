#! /usr/bin/env Rscript

##### Accumulating WCEC runs ####
library(rvest)
library(dplyr)
library(ggplot2)

centers <- html_table(html("http://wiki.wurstmineberg.de/Portal_Game#Centers"))

names(centers) <- factor(seq_along(centers))
centers <- plyr::ldply(centers, identity, .id = "Center")
centers <- centers %>% rename(Death_Count = `Death Count`, num = `#`) %>%
                       mutate(Death_Count = suppressWarnings(as.numeric(Death_Count)),
                              Name = factor(Name)) %>%
                       filter(!is.na(Death_Count)) 

##### Plots #####
plotdir <- "output/wcec"
if(!file.exists(plotdir)){
  dir.create(plotdir)
}

bestlist <- centers %>% group_by(Name) %>% summarize(dc = sum(Death_Count)) %>% arrange(dc)

p <- centers %>% mutate(Name = factor(Name, levels = as.character(bestlist$Name), ordered = TRUE)) %>%
       ggplot(data = ., aes(x = Name, y = Death_Count, fill = Center)) +
       geom_bar(stat = "identity", colour = "black") +
       labs(title = "WCEC Deaths per Person", x = "Person", y = "Death Count")
ggsave(plot = p, filename = paste0(plotdir, "/total_deaths_person.png"), width = 12, height = 6)

p <- ggplot(data = centers, aes(x = Center, y = Death_Count)) +
       geom_bar(stat = "identity", position = "stack") +
       labs(title = "WCEC Deaths per Center", x = "Center", y = "Death Count")
ggsave(plot = p, filename = paste0(plotdir, "/total_deaths_center.png"), width = 12, height = 6)

p <- centers %>% group_by(Center) %>% 
      summarize(runs = n(), deaths = sum(Death_Count)) %>%
      mutate(deaths_per_run = deaths/runs) %>%
      ggplot(data = ., aes(x = Center, y = deaths_per_run)) +
      geom_bar(stat = "identity") +
      labs(title = "WCEC Deaths per Run per Center", x = "Center", y = "Deaths per Run")
ggsave(plot = p, filename = paste0(plotdir, "/total_deaths_per_run_center.png"), width = 12, height = 6)

p <- centers %>% group_by(Name) %>% 
      summarize(runs = n(), deaths = sum(Death_Count)) %>%
      mutate(deaths_per_run = deaths/runs) %>%
      ggplot(data = ., aes(x = reorder(Name, deaths_per_run), y = deaths_per_run, fill = factor(runs))) +
      geom_bar(stat = "identity", colour = "black") +
      labs(title = "WCEC Deaths per Run per Person", x = "Person", y = "Deaths per Run", fill = "# Runs")
ggsave(plot = p, filename = paste0(plotdir, "/total_deaths_per_run_person.png"), width = 12, height = 6)
