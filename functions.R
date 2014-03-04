## Defining some functions and variables used by other scripts

# Define general legend/guide for all players
playerTheme <- theme(legend.position="right",
                    legend.key.size = unit(.4, "cm"),
                    legend.text = element_text(size = rel(.8)));

# Define some variables for bar chart layout and labels
plotWidth <- 6; plotHeight <- 4;
barChart <- geom_bar(colour="black", width=.7, stat="identity")
xLable <- xlab("Player")

# Define colour scale to keep status colours static
statusColours <- brewer.pal(9,"Set1")
names(statusColours) <- levels(playerstats$joinStatus)
legendTitle <- scale_fill_manual(name = "Join Status", values = statusColours)
