library(shiny)
require(rCharts)
require(RCurl)
options(RCHART_LIB = 'polycharts')

columns       <- names(activePeople)
columnsPreset <- c("name", "joinDate", "joinStatus", "color", "invitedBy")

shinyUI(navbarPage("Wurstminedata",
  tabPanel("Data",
    fluidPage(
      ## Debug only
      h1("Warning: Early dev shit"),
      p("This is as early as can be, nothing is cleaned up yet and most shit is dysfunctional."),
      p("It's only online for the 'hey, $function now works, look' and 'any hints as to how to make $stuff less sucky?' things."),
      ## /Debug only
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
                             selected = columnsPreset)), # closes sidebarPanel
        
        mainPanel(
          h3("For your consideration:"),
            p(textOutput("birthdays")), br(),
          tabsetPanel(
            tabPanel("People", dataTableOutput("tablePeople")),
            tabPanel("Sessions", dataTableOutput("tableSessions"))
        )))
    )),

    tabPanel("Plots",
      fluidPage(sidebarLayout(
        sidebarPanel(
          h3("Plot controls"),
          dateRangeInput("datesPlot", 
               h4("Select Time Period"),
               start = "2014-02-19", 
               end = as.character(Sys.Date())),
          checkboxGroupInput("columnSelectPlot", 
                             label = h4("People to Display (NYI)"), 
                             choices = activePeople$name[activePeople$name %in% playedPerPerson$person],
                             selected = activePeople$name[activePeople$name %in% playedPerPerson$person])
          ),
        mainPanel(tabsetPanel(
          tabPanel("Played Time – ggplot2 legacy)", plotOutput("sessionPlot")),
          tabPanel("Played Time – rCharts (Test)", showOutput("sessionPlot2", "polycharts")))
    )))),

    tabPanel("About", 
      fluidPage(
          includeMarkdown("about.md")
        )
    )
))