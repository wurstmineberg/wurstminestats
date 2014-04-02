library(shiny)
columns <- names(activePeople)
shinyUI(navbarPage("Wurstminedata",
  tabPanel("Data",
    fluidPage(
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
          
        ), # closes sidebarPanel
        
        mainPanel(
          h3(textOutput("birthdays")), br(),
          tabsetPanel(
            tabPanel("People", dataTableOutput("table1"))
          )
        )
      ) # Closes sidebarLayout
    ) # Closes fluidPage
  ),

    tabPanel("About", 
      fluidPage(
          p("I should buy a goat")
        )
    )

))
