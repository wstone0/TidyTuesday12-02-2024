library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)

A64 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-12-03/A64_traffic.csv')
A64 <- A64 %>% mutate(Day = day(`Report Date`))
A64 <- A64 %>% mutate(Hour = hour(hms(`Time Period Ending`)))

site_names <- A64 %>% pull(Name) %>% unique()
days <- A64 %>% pull(Day) %>% unique()

ui <- fluidPage(

    titlePanel("A64 Road Traffic Data (May 2021)"),

    sidebarLayout(
        sidebarPanel(
            selectInput("site_names",
                        "Choose a site:",
                        choices = site_names),
            sliderInput("days",
                        "Day of the month:",
                        min = 1,
                        max = 31,
                        value = 1)
        ),

        mainPanel(
           plotlyOutput("speedPlot"),
           plotlyOutput("volumePlot")
        )
    )
)

server <- function(input, output) {

    output$speedPlot <- renderPlotly({
        p <- A64 %>% 
          filter(Name == input$site_names) %>% 
          filter(Day == input$days) %>% 
          group_by(Hour) %>% 
          summarize(avg_speed = mean(`Avg mph`)) %>% 
          ggplot(aes(x = Hour, y = avg_speed)) +
          labs(
            title = "Average Vehicle Speed by Hour",
            x = "Hour of the Day",
            y = "Average Speed (mph)") +
          geom_col() +
          theme_classic()
        ggplotly(p)
    })
    
    output$volumePlot <- renderPlotly({
      p <- A64 %>% 
        filter(Name == input$site_names) %>% 
        filter(Day == input$days) %>% 
        group_by(Hour) %>% 
        summarize(volume = mean(`Total Volume`)) %>% 
        ggplot(aes(x = Hour, y = volume)) +
        geom_col() +
        labs(
          title = "Vehicle Volume by Hour",
          x = "Hour of the Day",
          y = "Volume") +
        theme_classic()
      ggplotly(p)
    })
}

shinyApp(ui = ui, server = server)
