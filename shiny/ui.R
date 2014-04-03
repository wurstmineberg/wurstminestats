library(shiny)

columns       <- names(activePeople)
columnsPreset <- c("name", "joinDate", "joinStatus", "color", "invitedBy")

shinyUI(navbarPage("Wurstminedata",
  tabPanel("Data",
    fluidPage(
      sidebarLayout(

        sidebarPanel(
          h3("People filters"),
          helpText("You can choose the whitelisting period with the sliders below."),
        
          dateRangeInput("dates", 
            h4("Whitelisting Period"),
            start = "2012-10-30", 
            end = as.character(Sys.Date())),
          
          checkboxInput("onlycol", label = "Show only those with fav color set", value = FALSE),
          
          checkboxGroupInput("columnSelect", 
                             label = h4("Colums to Display"), 
                             choices = columns,
                             selected = columnsPreset)
       
        #  actionButton("get", "Nothing yet")
          
        ), # closes sidebarPanel
        
        mainPanel(
          h3("For your consideration:"),
            p(textOutput("birthdays")), br(),
          tabsetPanel(
            tabPanel("People", dataTableOutput("tablePeople")),
            tabPanel("Sessions", dataTableOutput("tableSessions"))
        ))
  ))),

    tabPanel("Plots",
      fluidPage(sidebarLayout(
        sidebarPanel(
          h3("Plot controls")),
        mainPanel()
    ))),

    tabPanel("About", 
      fluidPage(
          includeMarkdown("about.md")
        )
    )

))
