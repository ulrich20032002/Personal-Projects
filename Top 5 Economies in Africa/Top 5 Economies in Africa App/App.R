library(shiny)
library(plotly)
library(readxl)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(repmis)

africatop5url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRBcd5gG2H5Rdw4yrgWkYKegK_RaSgoGM-5f4bpvJt3c7IkC_4CyGvkj-XXSCoDgRZKfU8wu9mElHyh/pub?output=csv"

africatop5 <- read.csv(url(africatop5url), check.names = FALSE)


ui <- dashboardPage(
  dashboardHeader(title = "African Economies"),
  dashboardSidebar(
    
    sliderInput(inputId = "Year",
              label = "Choose a timeframe",
              min = min(africatop5$Year), 
              max = max(africatop5$Year),
              value = c(min(africatop5$Year),max(africatop5$Year)), 
              ticks = TRUE,
              round = TRUE,
              step = 1,
              sep = ""),
    
    selectizeInput("countries",
                   "Select Country:",
                   choices = unique(africatop5$`Country Name`),
                   selected = unique(africatop5$`Country Name`),
                   multiple = TRUE
                   )
    ),
    dashboardBody(
      h1("African Economies: Top 5 Powerhouses | 1995 - 2015"),
      box(title = "Top 5 African Economies by GDP (constant 2010 US$)", status = "primary", plotlyOutput("top5bygdp", height = 300)),
      box(title = "Top 5 African Economies by GDP Per Capita (constant 2010 US$)", status = "primary", plotlyOutput("top5bygdppercapita", height = 300)),
      box(title = "Top 5 African Economies by Consumption (2016 US$)", status = "primary", plotlyOutput("top5byhconsumption", height = 300)),
      box(title = "Top 5 African Economies by Gross Domestic Savings (2016 US$)", status = "primary", plotlyOutput("top5bygrossdomesticsavings", height = 300))

      )) 

server <- function(input, output) {

  
  output$top5bygdp <- renderPlotly({
    
    
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
            mode = 'line+markers')
    })
  
  output$top5bygdppercapita <- renderPlotly({
    
    
    gdppercapitatop5 <- africatop5 %>%
      select(Year, `GDP per capita (constant 2010 US$)`, `Country Name`) %>%
      group_by(Year, `Country Name`) %>%
      summarize(`GDP Per Capita` = sum(`GDP per capita (constant 2010 US$)`)) %>%
      filter(Year >= as.numeric(input$Year[1]),
             Year <= as.numeric(input$Year[2]),
             `Country Name` %in% input$countries)
    
    plot_ly(gdppercapitatop5, 
            x = ~Year,
            y = ~`GDP Per Capita`,
            color = ~`Country Name`,
            type = 'scatter',
            mode = 'line+markers') 
    
  })
    
    output$top5byhconsumption <- renderPlotly({
      
      
      hconsumptiontop5 <- africatop5 %>%
        select(Year, `Household final consumption expenditure (current US$)`, `Country Name`) %>%
        group_by(Year, `Country Name`) %>%
        summarize(`Household Consumption Expenditure` = sum(`Household final consumption expenditure (current US$)`)) %>%
        filter(Year >= as.numeric(input$Year[1]),
               Year <= as.numeric(input$Year[2]),
               `Country Name` %in% input$countries)
      
      plot_ly(hconsumptiontop5, 
              x = ~Year,
              y = ~`Household Consumption Expenditure`,
              color = ~`Country Name`,
              type = 'scatter',
              mode = 'line+markers')
  })
    
    
    output$top5bygrossdomesticsavings <- renderPlotly({
     
      
      grossdomesticsavingstop5 <- africatop5 %>%
        select(Year, `Gross domestic savings (current US$)`, `Country Name`) %>%
        group_by(Year, `Country Name`) %>%
        summarize(`Gross domestic savings` = sum(`Gross domestic savings (current US$)`)) %>%
        filter(Year >= as.numeric(input$Year[1]),
               Year <= as.numeric(input$Year[2]),
               `Country Name` %in% input$countries)
      
      plot_ly(grossdomesticsavingstop5, 
              x = ~Year,
              y = ~`Gross domestic savings`,
              color = ~`Country Name`,
              type = 'scatter',
              mode = 'line+markers')
    })
    
    
}

shinyApp(ui=ui, server=server)
