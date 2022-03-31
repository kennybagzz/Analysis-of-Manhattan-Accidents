library(leaflet)
library(shiny)
library(shinydashboard)

dashboardPage(skin = "green",
  dashboardHeader(title = 'Manhattan Accidents'),
  
  
  dashboardSidebar(
    sidebarMenu(id = 'sideBarMenu',
              menuItem("Introduction", tabName = "intro",icon = icon("map")),
              menuItem("Occurrences", tabName = "occurrence", icon = icon("map"),
                  menuSubItem("Accidents per Year",tabName = "plot1", icon = icon("chart-line")),
                  menuSubItem("Accidents per Month",tabName = "plot2", icon = icon("chart-line")),
                  menuSubItem("Accidents per Day",tabName = "plot3", icon = icon("chart-line"))),
              menuItem("Severity",tabName = "severity", icon = icon("chart-bar")),
              menuItem("Location",tabName = "location", icon = icon("chart-bar"))
              )),
  
dashboardBody(
  tabItems(
    
    tabItem(tabName = "plot1",
            fluidRow(box(
              plotOutput("plot1",height = 650),
              width = 12))),
    tabItem(tabName = "plot2",
            fluidRow(box(
              plotOutput("plot2",height = 650),
              width = 12))),
    tabItem(tabName = "plot3",
            fluidRow(box(
              plotOutput("plot3",height = 650),
              width = 12))),
    tabItem(tabName = "plot4",
            fluidRow(box(
              plotOutput("plot4",height = 650),
              width = 12))),
    tabItem(tabName = 'severity',
            tabsetPanel(
              tabPanel("Casualties per Year", fluidRow(
                column(width =12, selectizeInput(inputId = "Case3",
                                                 label = "Case",
                                                 choices = c("Fatalities", "Injuries")
                )
                )
              ),
              fluidRow(
                column(
                  width = 8,
                  plotOutput("CasualtyPlot")))),
              tabPanel("Occurrences per Year", fluidRow(
                column(width =12, selectizeInput(inputId = "Case1",
                                                 label = "Casualty",
                                                 choices = c("All", "Fatalities", "Injuries", "Property Damage")
                )
                )
              ),
              
              fluidRow(
                column(
                  width = 8,
                  plotOutput("CaseOccurrencePlot")))),
            
              tabPanel("Proportions per Year", fluidRow(
                column(width =12, selectizeInput(inputId = "Case2",
                                                 label = "Case",
                                                 choices = c("All", "Fatalities", "Injuries", "Property Damage")
                )
                )
              ), fluidRow(
                column(
                  width = 8,
                  plotOutput("CaseProportionPlot")))
              ))),
    tabItem(tabName = 'location',
            tabsetPanel(
            tabPanel("Location",
            
                     fluidRow(box(
                       leafletOutput("myMap",
                                     height = 650),
                       width = 12))),
          tabPanel("Heatmap",
           
           fluidRow(box(
             leafletOutput("Accident Hotzone",
                           height = 650),
             width = 12)))))
            )))
    
  


