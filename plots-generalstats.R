#------------------------------------------------#
#### General stats barcharts and // Statspage ####
#------------------------------------------------#
message("Generating general stats plots")

statscales <- prettify_stats(stats$general)

for(i in seq_len(nrow(strings$general))){
  
  stat      <- strings$general$id[i]
  statScale <- statscales$scale[i]
  statName  <- strings$general$name[i]
  statUnit  <- statscales$unit[i]
  filename  <- paste0("output/general/", stat, ".png")
  
  p <- ggplot(data  = stats$general)
  p <- p + aes(x    = sortLevels(person, stats$general[[stat]]), 
               y    = stats$general[[stat]] / statScale)
  p <- p + barChart + statusFillScale + coord_flip()
  p <- p + scale_y_discrete(breaks = pretty_breaks()) 
  p <- p + xLable + labs(y = statUnit, title = statName)
  
  message("Saving ", filename)
  ggsave(plot = p, file = filename, height = plotHeight, width = plotWidth)

}; rm(i, filename, p, stat, statScale, statUnit, statName, statscales)

