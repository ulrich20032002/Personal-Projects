library(shiny)
library(plotly)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(shinydashboard)

setwd("C:\\Users\\meleu\\OneDrive\\Documents\\Senan\\Analyst Kingdom\\Top 5 Economies in Africa")

#reading file
africatop5 <- read.csv("Outputs\\Africatop5Subset.csv", check.names = FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "Economies in Africa"),
  dashboardSidebar(
    
    sliderInput(inputId = "Year",
                label = "Choose a timeframe",
                min = min(africatop5$Year), 
                max = max(africatop5$Year),
                value = c(min(africatop5$Year),max(africatop5$Year)), 
                ticks = TRUE,
                round = TRUE,
                step = 1),
    
    selectizeInput("countries",
                   "Select Country:",
                   choices = unique(africatop5$`Country Name`),
                   selected = "Nigeria",
                   multiple = TRUE
    )
  ),
  dashboardBody(
    h1("Top 5 Economies in Africa"),
    plotlyOutput("top5bygdp"))) #To make space for the output
#we tell the app how to build thwe plot, and that's something we do with the server function

#Tell the server how to assemble your inputs into your outputs
#3 Rules for server
#1) Save objects to display output with output$ + name using for output in fluidPage
#2) Use render+the type of output. In the render function, write code that builds the object
#3) Access input values with input$
#Create reactivity by using Inputs to build rendered outputs

server <- function(input, output) {
  
  output$top5bygdp <- renderPlotly({
    # title <- "100 random normal values"
    
    gdptop5 <- africatop5 %>%
      select(Year, `GDP (constant 2010 US$)`, `Country Name`) %>%
      group_by(Year, `Country Name`) %>%
      summarize(GDP = sum(`GDP (constant 2010 US$)`)) %>%
      filter(Year >= as.numeric(input$Year[1]),
             Year <= as.numeric(input$Year[2]),
             `Country Name` %in% input$countries)
    
    plot_ly(gdptop5, 
            x = ~Year,
            y = ~GDP,
            color = ~`Country Name`,
            type = 'scatter',
            mode = 'lines+markers')
  })
}

shinyApp(ui=ui, server=server)