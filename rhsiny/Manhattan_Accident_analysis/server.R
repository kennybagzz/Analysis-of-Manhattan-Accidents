
library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output) {
  output$plot1 <- renderPlot({accidents_per_yr_graph
  })
  output$plot2 <- renderPlot({accidents_per_month_graph
  })
  output$plot3 <- renderPlot({accidents_per_day_graph
  })
  output$plot4 <- renderPlot({accidents_by_hour_graph
  })
}
