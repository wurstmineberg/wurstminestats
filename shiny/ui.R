library(shiny)

shinyUI(fluidPage(
  titlePanel("Wurstminedata", p("Where data goes to rot")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This is a demo displaying server members"),
      br(),
      helpText("You can choose the whitelisting period with the sliders below."),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2012-10-30", 
        end = as.character(Sys.Date()))#,
   
    #  actionButton("get", "Nothing yet")
      
    ),
    
    mainPanel(
      h3(textOutput("birthdays")), br(),
      tabsetPanel(
        tabPanel("People", dataTableOutput("table1")), 
        tabPanel("Sessions", "NYI"), 
        tabPanel("Deaths", "NYI")
      )
      
    )
  )
))