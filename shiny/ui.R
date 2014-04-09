library(shiny)
require(rCharts)
options(RCHART_LIB = 'polycharts')

columns        <- names(activePeople)
columnsPreset  <- c("name", "joinDate", "joinStatus", "color", "invitedBy")
sessionsPeople <- activePeople$name[activePeople$name %in% playedPerPerson$person]

# current server age total
wurstminebergAge <- round(as.numeric(difftime(Sys.time(), activePeople$joinDate[1], units ="days")))

shinyUI(navbarPage(title="Wurstminedata", id="nav", inverse=TRUE,
  tabPanel(title="Data", value="data",
      pageWithSidebar(

        headerPanel("Warning: Early dev shit"),

        sidebarPanel(
          conditionalPanel(
            condition="input.dataTab == 'dataPeople'",
            h3("People Filters"),
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
            ),
          conditionalPanel(
            condition="input.dataTab == 'dataSessions'",
            h3("Session Filters"),
              dateRangeInput("sessionDates", h4("Date range"),
                start = "2014-02-19", 
                end = as.character(Sys.Date())
              )
          )
        ), # closes sidebarPanel
        
        mainPanel(
          h3("For your consideration:"),
            p(textOutput("birthdays")),
            p(paste("The server is currently ", wurstminebergAge, " days old.", sep="")),
            br(),
          tabsetPanel(id="dataTab",
            tabPanel("People",                value="dataPeople",           dataTableOutput("tablePeople")),
            tabPanel("Sessions",              value="dataSessions",         dataTableOutput("tableSessions")),
            tabPanel("Deaths",                value="dataDeaths",           dataTableOutput("tableDeaths")),
            tabPanel("Item Stats (overview)", value="dataItemStats",        dataTableOutput("tableItemStats")),
            tabPanel("General Stats",         value="dataGeneralStats",     dataTableOutput("tableGeneralStats")),
            tabPanel("Achievements",          value="dataAchievements",     dataTableOutput("tableAchievements")),
            tabPanel("Entities",              value="dataEntities",         dataTableOutput("tableEntities"))

            )
        )
      )
  ),

  tabPanel("Plots", value="plots",
    pageWithSidebar(
      headerPanel("These are plots. They are mostly broken as of now, sorry."),
      sidebarPanel(
        h3("Plot controls"),
        dateRangeInput("datesPlot", h4("Select Time Period"),
             start = as.character(Sys.Date()-14), 
             end   = as.character(Sys.Date())
        ),
        selectInput("line.or.bar", "Chart type", choices =c("Line", "Bar"), selected = "Bar"),
        selectInput("date.scope", "Date scope", choices =c("Daily", "Weekdays"), selected = "Daily"),
        checkboxGroupInput("columnSelectPlot", 
                           label    = h4("People to Display"), 
                           choices  = sessionsPeople,
                           selected = sessionsPeople)
        ),
      mainPanel(tabsetPanel(
        tabPanel("Played Time – ggplot2", plotOutput("sessionPlot")),
        tabPanel("Played Time – rCharts (NYI)", showOutput("sessionPlot2", "polycharts"))
      ))
    )
  ),

    tabPanel("About", value="about",
      fluidPage(
          includeMarkdown("about.md")
        )
    )
))
