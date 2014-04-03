# server.R

source("helpers.R")

shinyServer(function(input, output) {
  
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
  })
  
  output$tableSessions <- renderDataTable({
    playedPerPerson
  })
  
})
