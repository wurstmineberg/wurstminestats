#----------------#
#### Sessions ####
#----------------#
message("Generating session plots")

# Plotting playedPerDay
p <- ggplot(data = playedPerDay)
p <- p + aes(x = date, y = timePlayed/60)
p <- p + geom_area(alpha = 0.7) + geom_point() + geom_path(alpha = .8)
p <- p + geom_hline(yintercept = mean(playedPerDay$timePlayed/60), alpha = .5)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                          breaks = date_breaks("weeks"),
                          minor_breaks = "days")
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + labs(y = "Played Hours", x = "Day", title = "Total Time Played per Day (UTC)")
ggsave(p, file = "Plots/sessions/playTime.png", height = 6, width = 12)

# Testing "played per weekday"
fillColours   <- activePeople$color[activePeople$name %in% playedPerWeekday$person]

p <- ggplot(data = arrange(playedPerWeekday, person))
p <- p + aes(x = wday, y = timePlayed/60, fill = person)
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + geom_hline(yintercept = avgPerWeekday/60, alpha = .5)
p <- p + labs(y = "Played Hours", x = "Weekdays", title = "Total Time Played per Day of Week")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
ggsave(p, file = "Plots/sessions/playTime_weekdays.png", height = 6, width = 12)
rm(avgPerWeekday)

# Plotting playedPerPerson 
fillColours <- activePeople$color[activePeople$name %in% playedPerPerson$person]

p <- ggplot(data = arrange(playedPerPerson, person), aes(x = format(date, "%d"), y = timePlayed/60, fill = person))
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"), breaks = date_breaks("days"))
p <- p + scale_y_continuous(breaks = pretty_breaks()) + playerTheme
p <- p + labs(y = "Played Hours", x = "Day of Month (UTC)", title = "Time Played per Day: 2014")
p <- p + scale_fill_manual(name = "People", values = fillColours)
p <- p + facet_grid(month ~ .)
monthNum <- length(unique(playedPerPerson$month))
ggsave(p, file = "Plots/sessions/playTime_perPerson.png", height = (monthNum*2), width = 12)
rm(p, fillColours, monthNum)

# Plotting playedPerMonth 
fillColours   <- activePeople$color[activePeople$name %in% playedPerMonth$person]

p <- ggplot(data = arrange(playedPerMonth, person))
p <- p + aes(x = month, y = timePlayed/60, fill = person)
p <- p + geom_bar(position = "stack", stat = "identity", colour = "black")
p <- p + geom_hline(yintercept = avgPerMonth/60, alpha = .5)
p <- p + labs(y = "Played Hours", x = "Months", title = "Total Time Played per Month")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
ggsave(p, file = "Plots/sessions/playTime_months.png", height = 6, width = 12)
rm(p)
# JoinTime hours histogram
fillColours   <- activePeople$color[activePeople$name %in% playerSessions$person]

p <- ggplot(data = arrange(playerSessions, person))
p <- p + aes(x = hour(playerSessions$joinTime), fill = person)
p <- p + geom_histogram(colour = "black", binwidth = .7)
p <- p + labs(y = "Frequency", x = "Hour of Day (UTC)", title = "Join Time Frequencies")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + scale_x_discrete(limits = seq(0, 23, by = 1))
p <- p + scale_y_continuous(breaks = pretty_breaks())
ggsave(p, file = "Plots/sessions/joinTime_hours.png", height = 6, width = 12)
rm(p)
# LeaveTime hours histogram because it's only right
p <- ggplot(data = arrange(playerSessions, person))
p <- p + aes(x = hour(playerSessions$leaveTime), fill = person)
p <- p + geom_histogram(colour = "black", binwidth = .7)
p <- p + labs(y = "Frequency", x = "Hour of Day (UTC)", title = "Leave Time Frequencies")
p <- p + scale_fill_manual(name = "People", values = fillColours) + playerTheme
p <- p + scale_x_discrete(limits = seq(0, 23, by = 1))
p <- p + scale_y_continuous(breaks = pretty_breaks())
ggsave(p, file = "Plots/sessions/leaveTime_hours.png", height = 6, width = 12)
rm(fillColours)
