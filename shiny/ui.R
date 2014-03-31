library(shiny)
columns <- names(activePeople)
shinyUI(fluidPage(
  titlePanel("Wurstminedata", "Where data goes to rot"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This is a demo displaying server members"),
      br(),
      helpText("You can choose the whitelisting period with the sliders below."),
    
      dateRangeInput("dates", 
        h3("Date range"),
        start = "2012-10-30", 
        end = as.character(Sys.Date())),
      
      checkboxInput("onlycol", label = "Show only those with fav color set", value = FALSE),
      
      checkboxGroupInput("columnSelect", 
                         label = h3("Select Colums"), 
                         choices = columns,
                         selected = c("name", "joinDate", "joinStatus", "serverAge", "color", "invitedBy"))
   
    #  actionButton("get", "Nothing yet")
      
    ),
    
    mainPanel(
      h3(textOutput("birthdays")), br(),
      tabsetPanel(
        tabPanel("People", dataTableOutput("table1"))
      )
      
    )
  )
))