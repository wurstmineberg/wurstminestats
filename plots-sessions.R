#----------------#
#### Sessions ####
#----------------#
message("Generating session plots")

#### Plotting playedPerDay
playedPerDay$year <- lubridate::year(playedPerDay$date)
playedPerDay_15   <- playedPerDay[playedPerDay$year == "2015", ]
playedPerDay_14   <- playedPerDay[playedPerDay$year == "2014", ]
playedPerDay_13   <- playedPerDay[playedPerDay$year == "2013", ]

# 2015
p <- ggplot(data = playedPerDay_15)
p <- p + aes(x = date, y = timePlayed/60)
p <- p + geom_area(alpha = 0.7) + geom_point() + geom_path(alpha = .8)
p <- p + geom_hline(yintercept = mean(playedPerDay_15$timePlayed/60), alpha = .5)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%B-%d"),
                          breaks = date_breaks("weeks"),
                          minor_breaks = "days")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Played Hours", x = "Date", title = "Total Time Played per Day 2015 (UTC)")
ggsave(p, file = "Plots/sessions/playTime_2015.png", height = 6, width = 12)

## 2014
p <- ggplot(data = playedPerDay_14)
p <- p + aes(x = date, y = timePlayed/60)
p <- p + geom_area(alpha = 0.7) + geom_point() + geom_path(alpha = .8)
p <- p + geom_hline(yintercept = mean(playedPerDay_14$timePlayed/60), alpha = .5)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%B-%d"),
                          breaks = date_breaks("weeks"),
                          minor_breaks = "days")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Played Hours", x = "Date", title = "Total Time Played per Day 2014 (UTC)")
ggsave(p, file = "Plots/sessions/playTime_2014.png", height = 6, width = 12)

## 2013
p <- ggplot(data = playedPerDay_13)
p <- p + aes(x = date, y = timePlayed/60)
p <- p + geom_area(alpha = 0.7) + geom_point() + geom_path(alpha = .8)
p <- p + geom_hline(yintercept = mean(playedPerDay_13$timePlayed/60), alpha = .5)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%B-%d"),
                          breaks = date_breaks("weeks"),
                          minor_breaks = "days")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Played Hours", x = "Date", title = "Total Time Played per Day 2013 (UTC)")
ggsave(p, file = "Plots/sessions/playTime_2013.png", height = 6, width = 12)

#### Testing "played per weekday"
fillColours   <- people$color[people$name %in% playedPerWeekday$person]

p <- ggplot(data = arrange(playedPerWeekday, person))
p <- p + aes(x = wday, y = timePlayed/60, fill = person)
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + geom_hline(yintercept = avgPerWeekday/60, alpha = .5)
p <- p + labs(y = "Played Hours", x = "Weekdays", title = "Total Time Played per Day of Week")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
ggsave(p, file = "Plots/sessions/playTime_weekdays.png", height = 6, width = 12)

#### Plotting playedPerPerson 
playedPerPerson_15 <- playedPerPerson[year(playedPerPerson$date) == "2015", ]
fillColours        <- people$color[people$name %in% playedPerPerson_15$person]

p <- ggplot(data = arrange(playedPerPerson_15, person), aes(x = format(date, "%d"), y = timePlayed/60, fill = person))
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"), breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks = pretty_breaks()) + playerTheme
p <- p + labs(y = "Played Hours", x = "Day of Month (UTC)", title = "Time Played per Day: 2015")
p <- p + scale_fill_manual(name = "People", values = fillColours)
#p <- p + facet_grid(month ~ ., scales = "free_y")
p <- p + facet_wrap(~month, ncol = 2, scales = "free_y")
ggsave(p, file = "Plots/sessions/playTime_perPerson_2015.png", height = 8, width = 12)
rm(p, fillColours)

# 2014
playedPerPerson_14 <- playedPerPerson[year(playedPerPerson$date) == "2014", ]
fillColours        <- people$color[people$name %in% playedPerPerson_14$person]

p <- ggplot(data = arrange(playedPerPerson_14, person), aes(x = format(date, "%d"), y = timePlayed/60, fill = person))
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"), breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks = pretty_breaks()) + playerTheme
p <- p + labs(y = "Played Hours", x = "Day of Month (UTC)", title = "Time Played per Day: 2014")
p <- p + scale_fill_manual(name = "People", values = fillColours)
#p <- p + facet_grid(month ~ ., scales = "free_y")
p <- p + facet_wrap(~month, ncol = 2, scales = "free_y")
ggsave(p, file = "Plots/sessions/playTime_perPerson_2014.png", height = 8, width = 12)
rm(p, fillColours)

# For 2013
playedPerPerson_13 <- playedPerPerson[year(playedPerPerson$date) == "2013", ]
fillColours        <- people$color[people$name %in% playedPerPerson_13$person]

p <- ggplot(data = arrange(playedPerPerson_13, person), aes(x = format(date, "%d"), y = timePlayed/60, fill = person))
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"), breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks = pretty_breaks()) + playerTheme
p <- p + labs(y = "Played Hours", x = "Day of Month (UTC)", title = "Time Played per Day: 2013")
p <- p + scale_fill_manual(name = "People", values = fillColours)
#p <- p + facet_grid(month ~ ., scales = "free_y")
p <- p + facet_wrap(~month, ncol = 2, scales = "free_y")
ggsave(p, file = "Plots/sessions/playTime_perPerson_2013.png", height = 8, width = 12)
rm(p, fillColours)

#### Plotting playedPerMonth 
fillColours   <- people$color[people$name %in% playedPerMonth$person]

p <- ggplot(data = arrange(playedPerMonth, person))
p <- p + aes(x = month, y = timePlayed/60, fill = person)
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + geom_hline(yintercept = avgPerMonth/60, alpha = .5)
p <- p + labs(y = "Played Hours", x = "Months", title = "Total Time Played per Month")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
ggsave(p, file = "Plots/sessions/playTime_months.png", height = 6, width = 12)

#### JoinTime hours histogram
fillColours   <- people$color[people$id %in% playerSessions$person]

p <- ggplot(data = arrange(playerSessions, person))
p <- p + aes(x = hour(playerSessions$joinTime), fill = person)
p <- p + geom_histogram(colour = "black", binwidth = .7)
p <- p + labs(y = "Frequency", x = "Hour of Day (UTC)", title = "Join Time Frequencies")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + scale_x_discrete(limits = seq(0, 23, by = 1))
p <- p + scale_y_continuous(breaks = pretty_breaks())
ggsave(p, file = "Plots/sessions/joinTime_hours.png", height = 6, width = 12)
rm(p)

# Now density because lol
p <- ggplot(data = arrange(playerSessions, person))
p <- p + aes(x = hour(playerSessions$joinTime), fill = person)
p <- p + geom_density(colour = "black", position = "stack")
p <- p + labs(y = "Density", x = "Hour of Day (UTC)", title = "Join Time Frequencies")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + scale_x_discrete(limits = seq(0, 23, by = 1))
p <- p + scale_y_continuous(breaks = pretty_breaks())
ggsave(p, file = "Plots/sessions/joinTime_hours_d.png", height = 6, width = 12)

# LeaveTime hours histogram because it's only right
p <- ggplot(data = arrange(playerSessions, person))
p <- p + aes(x = hour(playerSessions$leaveTime), fill = person)
p <- p + geom_histogram(colour = "black", binwidth = .7)
p <- p + labs(y = "Frequency", x = "Hour of Day (UTC)", title = "Leave Time Frequencies")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + scale_x_discrete(limits = seq(0, 23, by = 1))
p <- p + scale_y_continuous(breaks = pretty_breaks())
ggsave(p, file = "Plots/sessions/leaveTime_hours.png", height = 6, width = 12)

p <- ggplot(data = arrange(playerSessions, person))
p <- p + aes(x = hour(playerSessions$leaveTime), fill = person)
p <- p + geom_density(colour = "black", position = "stack")
p <- p + labs(y = "Density", x = "Hour of Day (UTC)", title = "Leave Time Frequencies")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + scale_x_discrete(limits = seq(0, 23, by = 1))
p <- p + scale_y_continuous(breaks = pretty_breaks())
ggsave(p, file = "Plots/sessions/leaveTime_hours_d.png", height = 6, width = 12)
rm(fillColours)

#### Played per year
fillColours   <- people$color[people$id %in% playerSessions$person]

p <- ggplot(data = arrange(playedPerYear, person))
p <- p + aes(x = as.factor(year), y = playedMinutes/60, fill = person)
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + labs(y = "Played Hours", x = "Year", title = "Total Time Played Year")
ggsave(p, file = "Plots/sessions/playedPerYear.png", height = 6, width = 7)

#### Played per month… per year
p <- ggplot(data = arrange(playedPerMonthYear, person))
p <- p + aes(x = month, y = playedMinutes/60, fill = person)
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + labs(y = "Played Hours", x = "Year", title = "Total Time Played Month")
p <- p + facet_grid(~ year)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave(p, file = "Plots/sessions/playedPerMonthYear.png", height = 6, width = 11)

#### LastSeen
legend <- data.frame(value = c(mean(lastseen$daysSince),
                               median(lastseen$daysSince),
                               365),
                     name  = c("Mean", "Median", "1 Year Mark"))

p <- ggplot(data = lastseen, aes(x = sortLevels(person, desc(daysSince)), 
                                 y = daysSince,
                                 fill = person))
p <- p + barChart + playerTheme
p <- p + geom_hline(data = legend, show_guide = T, aes(yintercept = value, linetype = name))
p <- p + scale_fill_manual(name = "People", values = fillColours, guide = "none") + coord_flip()
p <- p + labs(title = "Days Since Last Activity", x = "Person", y = "Days") 
p <- p + scale_linetype_discrete(name = "Markers")
ggsave(p, file = "Plots/sessions/lastSeen.png", height = 6, width = 9)

# Now z-scaled because z-scaling is funnne
p <- ggplot(data = lastseen, aes(x = sortLevels(person, desc(scale(daysSince))), 
                                 y = scale(daysSince),
                                 fill = person))
p <- p + barChart + playerTheme
p <- p + scale_fill_manual(name = "People", values = fillColours, guide = "none") + coord_flip()
p <- p + labs(title = "Days Since Last Activity", x = "Person", y = "Scaled Days ((x-m)/sd)") 
ggsave(p, file = "Plots/sessions/lastSeen_z.png", height = 6, width = 9)