# server.R

source("helpers.R")
require(rCharts)

shinyServer(function(input, output) {
  
  #### Handle table outouts ####
  filterDate <- reactive({
    date2 <- as.POSIXct(input$dates[2], tz="UTC")
    date1 <- as.POSIXct(input$dates[1], tz="UTC")
    peopleDate <- activePeople[activePeople$joinDate > date1 & activePeople$joinDate < date2, ]
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
    paste("The next server birthday is ", birthdays$nextPerson, "'s on ", birthdays$nextDate, sep='')
  })
    
  output$tablePeople <- renderDataTable({
    people <- filterCol()
    people[input$columnSelect]
  }, options = list(bPaginate = FALSE))
  
  output$tableSessions <- renderDataTable({
    playedPerPerson
  })
  
  #### Plot stuff ####
  filterDatePlot <- reactive({
    date2plot <- as.POSIXct(input$datesPlot[2], tz="UTC")
    date1plot <- as.POSIXct(input$datesPlot[1], tz="UTC")
    playedPerPersonDate <- playedPerPerson[playedPerPerson$date > date1plot & playedPerPerson$date < date2plot, ]
    return(playedPerPersonDate)
  })
  
  output$sessionPlot <- renderPlot({
    playedPerPerson <- filterDatePlot()
    
    fillColours <- activePeople$color[activePeople$name %in% playedPerPerson$person]
    
    p <- ggplot(data=playedPerPerson, aes(x=date, y=timePlayed/60, fill=person))
    p <- p + geom_bar(position="stack", stat="identity", colour="black")
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    p <- p + scale_x_datetime(labels = date_format("%y-%m-%d"),
                              breaks = date_breaks("days"))
    p <- p + scale_y_continuous(breaks=pretty_breaks()) + playerTheme
    p <- p + labs(y="Played Hours", x="Day", title=element_blank())
    p <- p + scale_fill_manual(name="People", values=fillColours)
    print(p)
  })
  
  output$sessionPlot2 <- renderChart({
    playTimes <- rPlot(x = list(var = "date", sort = "date"), y = "timePlayed", 
                       color = 'person', data = playedPerPerson, type = 'line')
    playTimes$addParams(dom = 'sessionPlot2')
    return(playTimes)
  })
  
})
