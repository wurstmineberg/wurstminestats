##### Accumulating WCEC runs ####
library(rvest)

centers <- html_table(html("http://wiki.wurstmineberg.de/Portal_Game#Centers"))

for (i in seq_along(centers)){
  centers[[i]]["Center"]        <- i
  centers[[i]]                  <- centers[[i]][!(centers[[i]]["#"] == "?"), ]
  centers[[i]][["Death_Count"]] <- as.numeric(centers[[i]][["Death Count"]])
}

centers        <- plyr::ldply(centers, identity)
centers$Name   <- as.factor(centers$Name)
centers$Center <- as.factor(centers$Center)
centers$num    <- as.numeric(centers[["#"]])

##### Plots ####
plotdir <- "Plots/wcec"
if(!file.exists(plotdir)){
  dir.create(plotdir)
}

p <- ggplot(data = centers, aes(x = reorder(Name, Death_Count), y = Death_Count, fill = Center)) +
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
