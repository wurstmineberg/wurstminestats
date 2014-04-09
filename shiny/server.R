# server.R

require(shiny)
source("../dataPrep.R")
require(rCharts)

shinyServer(function(input, output) {
  
  #### Handle table outouts ####
  filterDate <- reactive({
    date2 <- as.POSIXct(input$dates[2], tz="UTC")
    date1 <- as.POSIXct(input$dates[1], tz="UTC")
    peopleDate <- activePeople[activePeople$joinDate >= date1 & activePeople$joinDate <= date2, ]
    return(peopleDate)
  })
  
  filterCol <- reactive({
    peopleCol <- filterDate()
    if(input$onlycol){
      peopleCol <- peopleCol[!is.na(peopleCol$color),]
    } 
    return(peopleCol)
  })
  output$birthdays <- renderText({
    paste("The next server birthday is ", birthdays$nextPerson, "'s on ", birthdays$nextDate, ".", sep='')
  })
    
  output$tablePeople <- renderDataTable({
    people <- filterCol()
    people[input$columnSelect]
  }, options = list(bPaginate = FALSE))
  
  output$tableSessions <- renderDataTable({
    date2 <- as.POSIXct(input$sessionDates[2], tz="UTC")
    date1 <- as.POSIXct(input$sessionDates[1], tz="UTC")
    playedPerPerson[playedPerPerson$date >= date1 & playedPerPerson$date <= date2, ]
  })

  output$tableDeaths <- renderDataTable({
    deaths
  })

  output$tableItemStats <- renderDataTable({
    itemStats
  })

  output$tableGeneralStats <- renderDataTable({
    generalstats
  })

  output$tableAchievements <- renderDataTable({
   achievements
  })

  output$tableEntities <- renderDataTable({
   entities
  })
  
  #### Plot stuff ####

  filterPlotPeople <- reactive({
    
    playedPerPerson       <- filterDatePlot()
    tempPeople            <- input$columnSelectPlot
    selectedRows          <- playedPerPerson$person %in% tempPeople
    playedPerPersonPeople <- playedPerPerson[selectedRows, ]
    
    return(playedPerPersonPeople)
    })
  
  output$sessionPlot <- renderPlot({
    date2plot <- as.POSIXct(input$datesPlot[2], tz="UTC")
    date1plot <- as.POSIXct(input$datesPlot[1], tz="UTC")

    playedPerPerson <- playedPerPerson[playedPerPerson$date >= date1plot & playedPerPerson$date <= date2plot, ]
    playedPerDay    <- playedPerDay[playedPerDay$date >= date1plot & playedPerDay$date <= date2plot, ]

    if(input$date.scope == "Daily"){

      fillColours <- activePeople$color[activePeople$name %in% playedPerPerson$person]
      p <- ggplot(data=playedPerPerson, aes(x=date, y=timePlayed/60))
      p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"), breaks = date_breaks("days"))
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

    } else if (input$date.scope == "Weekdays"){
      fillColours <- activePeople$color[activePeople$name %in% playedPerWeekday$person]
      p <- ggplot(data=playedPerWeekday, aes(x=date, y=timePlayed/60, fill=person))
      p <- p + geom_hline(yintercept = avgPerWeekday/60, alpha=.5)
    }
    if(input$line.or.bar == "Line"){

      p <- ggplot(data=playedPerDay, aes(x=date, y=timePlayed/60))
      p <- p + geom_area(alpha=0.7) + geom_point() + geom_path(alpha=.8)
      p <- p + geom_hline(yintercept = mean(playedPerDay$timePlayed/60), alpha=.5)

    } else if(input$line.or.bar == "Bar"){
      p <- p + geom_bar(position="stack", stat="identity", colour="black", aes(fill=person))
      p <- p + playerTheme
    }
    p <- p + scale_y_continuous(breaks=pretty_breaks())
    p <- p + labs(y="Played Hours", x="Day", title=element_blank())
    p <- p + scale_fill_manual(name="People", values=fillColours)
    p <- p + scale_color_manual(name="People", values=fillColours)
  
    print(p)
  })
  
  output$sessionPlot2 <- renderChart({
    playTimes <- rPlot(x = list(var = "date", sort = "date"), y = "timePlayed", 
                       color = 'person', data = playedPerPerson, type = 'line')
    playTimes$addParams(dom = 'sessionPlot2')
    return(playTimes)
  })
})
