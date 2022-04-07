library(leaflet)
library(shiny)
library(shinydashboard)
library(leafpop)
library(rsconnect)

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
    
  output$CaseOccurrencePlot <- renderPlot({
    if (input$Case1 == "Fatalities"){
      x = accidents_per_year_killed
      }
    else if (input$Case1 == "Injuries"){
      x = accidents_per_year_injured
    }
    else if (input$Case1 == "Property Damage"){
      x = accidents_per_year_property
    }
    else if (input$Case1 == "All"){
      x = idc
    }
    x
    })
  
  output$CaseProportionPlot <- renderPlot({
    if (input$Case2 == "Fatalities"){
      x = accidents_per_year_killed_proportions
    }
    else if (input$Case2 == "Injuries"){
      x = accidents_per_year_injured_proportion
    }
    else if (input$Case2 == "Property Damage"){
      x = accidents_per_year_property_proportion
    }
    else if (input$Case2 == "All"){
      x = idc_proportions
    }
    x
  })
  
  output$CasualtyProportionPlot <- renderPlot({
    if (input$Case3 == "Fatalities"){
      x = graph_people_killed_yr
    }
    else if (input$Case3 == "Injuries"){
      x = graph_people_injured_yr
    }
    x
  })
  
  output$myMap <- renderLeaflet({
    leaflet(filter(accidents_group_by_latlong, accidents_per_week >0.99))  %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lat = 40.766676,  lng = -73.971321,zoom = 12)%>%
      addCircles(lng = ~middle_longitude, lat = ~middle_latitude, weight = 1,
                 radius = 120.7, group = 'markers')
    
  })
  


}
