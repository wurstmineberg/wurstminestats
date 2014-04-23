library(shiny)
require(rCharts)
#options(RCHART_LIB = 'polycharts')

columns        <- names(activePeople)
columnsPreset  <- c("name", "joinDate", "joinStatus", "color", "invitedBy")
sessionsPeople <- activePeople$name[activePeople$name %in% playedPerPerson$person]

# current server age total
wurstminebergAge <- round(as.numeric(difftime(Sys.time(), activePeople$joinDate[1], units ="days")))

shinyUI(navbarPage(title = "Wurstminedata", id = "nav", inverse = TRUE,
  tabPanel(title = "Data", value = "data",
      pageWithSidebar(

        headerPanel("Warning: Early dev shit"),

        sidebarPanel(
          conditionalPanel(
            condition = "input.dataTab == 'dataPeople' ",
            h3("People Filters"),
            helpText("You can choose the whitelisting period with the sliders below."),
          
            dateRangeInput("dates", 
              h4("Whitelisting Period"),
              start = "2012-10-30", 
              end = as.character(Sys.Date())),
            
            checkboxInput("onlycol", label = "Show only those with fav color set", value = FALSE),
            
            selectInput("columnSelect", 
                               label = h4("Colums to Display"), 
                               choices = columns,
                               selected = columnsPreset,
                               multiple= TRUE,
                               selectize = TRUE)
            ),
          conditionalPanel(
            condition = "input.dataTab == 'dataSessions' ",
            h3("Session Filters"),
              dateRangeInput("sessionDates", h4("Date range"),
                start = "2014-02-19", 
                end = as.character(Sys.Date())
              )
          ),
          conditionalPanel(
            condition = "input.dataTab == 'dataDeaths' ",
            h3("Latest deaths"),
            helpText("These are the latest deaths only, since there currently is no API endpoint for the complete death log")
          ),
          conditionalPanel(
            condition = "input.dataTab == 'dataItemStats' ",
            h3("Item stats per item/action"),
            helpText("You can filter the dataset with the search boxes on the right, besides that I don't really know what to offer you.")
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
          conditionalPanel(
            condition = "input.plotTab == 'plotSessions' ",

              dateRangeInput("datesPlot", h4("Select Time Period"),
                            start = as.character(Sys.Date()-14), 
                            end   = as.character(Sys.Date())
              ),
              selectInput("line.or.bar", "Chart type", choices =c("Line", "Bar"), selected = "Bar"),
              selectInput("date.scope", "Date scope", choices =c("Daily", "Weekdays"), selected = "Daily")
          ),
          conditionalPanel(
            condition = "input.plotTab == 'plotGeneralStats' ",
              selectInput("selectGeneralStat",
                          label = h4("Select a statistic"),
                          choices = strings.general$name,
                          selected = strings.general$name[1],
                          multiple = FALSE,
                          selectize= TRUE)
          ),
          selectInput("columnSelectPlot", 
                             label    = h4("People to Display"), 
                             choices  = sessionsPeople,
                             selected = sessionsPeople,
                             multiple = TRUE,
                             selectize = TRUE)
        ),
      mainPanel(tabsetPanel(id = "plotTab",
        tabPanel("Played Time – ggplot2",       value = "plotSessions",     plotOutput("sessionPlot")),
        tabPanel("Played Time – rCharts (NYI)", value = "pltoSessions",     showOutput("sessionPlot2", "polycharts")),
        tabPanel("Played Time – gvis (WIP)",    value = "plotSessions",     htmlOutput("plot.session.gvis")),
        tabPanel("General Stats",               value = "plotGeneralStats", showOutput("plot.generalStats", "NVD3"))
      ))
    )
  ),

    tabPanel("About", value="about",
      fluidPage(
          includeMarkdown("about.md")
        )
    )
))
