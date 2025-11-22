#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(ggthemes)

data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")
metro_names <- data_by_dist |> pull(metro_name) |> unique()


# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    titlePanel("Diversity by District"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("city_name","City Name:", metro_names),
            sliderInput("span_parameter", "Span Parameter:",
              min = 0,
              max = 1,
              value = 0.5)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("diverse_scatterplot"),
           leafletOutput("diverse_map"),
           plotlyOutput("distr_bar")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$diverse_scatterplot <- renderPlotly({
      p <- data_by_dist %>%
        filter(metro_name  == input$city_name) %>%
      ggplot(aes(x = entropy, y = distmiles)) +
        geom_point(aes(key = tract_id)) +
        geom_smooth(se = FALSE, method = "loess", span = input$span_parameter, color = "red") +
        theme_minimal()

      ggplotly(p, source = "plotly_scatterplot") %>%
        event_register("plotly_selected")
    })

   output$diverse_map <- renderLeaflet({
     pal <- colorNumeric(c("#FFEEEE", "#FF0000","#800000"), domain=data_by_dist$entropy)
     m <- data_by_dist %>%
       filter(metro_name  == input$city_name) %>%
       leaflet() |>
       addProviderTiles("CartoDB.Positron") |>
       addPolygons(opacity = 0, fillColor = pal(data_by_dist$entropy), fillOpacity = 0.05, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
   })

   output$distr_bar <- renderPlotly({

   })
}

# Run the application
shinyApp(ui = ui, server = server)
