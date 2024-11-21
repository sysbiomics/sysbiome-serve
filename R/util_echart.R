# library(dplyr)
# library(echarts4r)
# library(glue)
# library(shiny)
# mock_data <- tibble::tribble(
#   ~level1  ,   ~level2     , ~level3           , ~sales,
#   "Asia"   ,    "China"    ,   "Shanghai"      ,  32L,
#   "Asia"   ,    "China"    ,   "Beijing"       ,  86L,
#   "Asia"   ,    "China"    ,   "Chongqing"     ,  30L,
#   "Asia"   ,    "India"    ,   "Mumbai"        ,  92L,
#   "Asia"   ,    "India"    ,   "Kolkata"       ,  75L,
#   "Asia"   ,    "India"    ,   "Chennai"       ,  99L,
#   "America",    "USA"      ,   "New York"      ,   6L,
#   "America",    "USA"      ,   "Chicago"       ,  12L,
#   "America",    "Argentina",   "Buenos Aires"  ,  54L,
#   "America",    "Argentina",   "Rosario"       ,  36L,
#   "America",    "Brasil"   ,   "Sao Paulo"     ,   2L,
#   "America",    "Brasil"   ,   "Rio de Janeiro",  64L,
#   "Europe" ,    "Spain"    ,   "Madrid"        ,  54L,
#   "Europe" ,    "Spain"    ,   "Barcelona"     ,  46L,
#   "Europe" ,    "Spain"    ,   "Sevilla"       ,  67L,
#   "Europe" ,    "Italy"    ,   "Rome"          ,  22L,
#   "Europe" ,    "France"   ,   "Paris"         ,  42L,
#   "Europe" ,    "France"   ,   "Marseille"     ,  91L
# )

# # Prepare data for chart
# chart_data <- mock_data |>
#   group_by(level = level1) |>
#   summarise(total = sum(sales))
# # Create chart
# plot_sales_data <- function(chart_data, chart_title, chart_color) { 
#     chart_data |> 
#       e_chart(x = level) |>
#       e_bar(total, name = chart_title, color = chart_color)
# }
# ui <- fluidPage(
#   h1("Drill Down in Shiny"),
#   echarts4rOutput("chart")
# )
# # Define server
# server <- function(input, output) {
#   output$chart <- renderEcharts4r({
#     # Prepare data for chart
#     chart_data <- mock_data |>
#       group_by(level = level1) |>
#       summarise(total = sum(sales))
    
#     # Create chart
#     plot_sales_data(
#       chart_data = chart_data,
#       chart_title = "Sales by Continent",
#       chart_color = "#5470C6"
#     )
#   })
# }
# shinyApp(ui, server)


library(shiny)
library(echarts4r)

# Sample data for Sankey chart
data <- data.frame(
  source = c("A", "A", "B", "C", "C"),
  target = c("B", "C", "C", "D", "E"),
  value = c(5, 10, 15, 20, 25)
)

# Shiny UI
ui <- fluidPage(
  titlePanel("Sankey Diagram Example with echart4r"),
  echarts4rOutput("sankey_chart") # Output for the Sankey diagram
)

# Shiny server logic
server <- function(input, output) {
  
  output$sankey_chart <- renderEcharts4r({
    data |>
      e_charts() |>
      e_sankey(source, target, value) |>
      e_tooltip(trigger = "item") # Add tooltips to show info on hover
  })
}

# Run the Shiny app
shinyApp(ui, server)