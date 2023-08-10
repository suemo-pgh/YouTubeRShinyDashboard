#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This dashboard was created through DATAcated Circle's class as an introduction to RShiny.


library(shinydashboard)
library(plotly)
library(ggplot2)
library(readxl)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Story by Data - YouTube Traffic Source Analysis", titleWidth = '600'),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    fluidRow(
      
      tags$h1('YouTube video views for 90 days (April 9 - July 7, 2020'),
      HTML('<img src="logo.png", height="50px" style="float:right"/>')
    ),
    
    fluidRow(
      column( width = 6,
        box(valueBoxOutput("total_views", width = 12), width=NULL),
        box(title = "The majority of views are from the following traffic sources", plotlyOutput("piechart"), width = NULL),
        print("Data source: Export from YouTube Analytics - Story by Data channel")
      ),
      
      column( width = 6,
        box(title = "View Trends From Previous Days",
            plotlyOutput("linechart"),
            width = NULL),
        box(title = "Average views per day of the week",
            plotlyOutput("barchart"),
            width = NULL) 
      )
    )
  
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  my_data <- read_excel("Story by Data YouTube.xlsx")
  
  total_view <- sum(my_data$Views)
  
  output$total_views <- renderValueBox({
  valueBox(value = total_view, "Total Views For Past 90 Days", color = "red", icon = icon("youtube"))
  })
  
  # Line chart section
  
  colors = c("#1be022", "#a4a4ab", "#a4a4ab", "#a4a4ab", "#a4a4ab","#a4a4ab", "#00a306")
  output$linechart <- renderPlotly({ggplot(my_data, aes(x=Date,y=Views,color=`Traffic source`)) +
    geom_line() +
    theme(legend.position = "none") +
    geom_smooth(method="lm", linetype="dashed") +
    scale_color_manual(values=colors)
    
  })
  
  #PIE CHART
  #aggregation
  totals <- aggregate(my_data$Views, by=list(Category=my_data$'Traffic source'), FUN=sum)
  
  output$piechart <- renderPlotly({plot_ly(totals, values = ~x, labels = ~Category, type = 'pie', showlegend=FALSE, 
                                           hole=0.6, textinfo='label+percent',
                                           marker=list(colors=
                                                         c(
                                                           "External" = "#1be022",
                                                           "Notification" = "#a4a4ab",
                                                           "Playlist" = "#a4a4ab",
                                                           "Subscriber" = "#a4a4ab",
                                                           "Other" = "#a4a4ab",
                                                           "YouTube Related" = "#a4a4ab",
                                                           "YouTube Search" = "#00a306"
                                                         ),
                                                       line = list(colors = "#FFF", width=2)))
  })
  
  #Bar Chart
  
  my_data$day <- weekdays(as.Date(my_data$Date))
  
  my_data$day <- factor(my_data$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  output$barchart <- renderPlotly({ ggplot(my_data, aes(x=day, fill=day)) + geom_histogram(stat="count") +
    scale_fill_manual(values=c("#808080", "#808080", "#808080", "#808080", "#808080", "#cc0000", "#cc0000"))
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
