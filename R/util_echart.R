# library(dplyr)
# library(echarts4r)
# library(glue)
# library(shiny)

# mock_data <- tibble::tribble(
#     ~level1, ~level2, ~level3, ~sales,
#     "Asia", "China", "Shanghai", 32L,
#     "Asia", "China", "Beijing", 86L,
#     "Asia", "China", "Chongqing", 30L,
#     "Asia", "India", "Mumbai", 92L,
#     "Asia", "India", "Kolkata", 75L,
#     "Asia", "India", "Chennai", 99L,
#     "America", "USA", "New York", 6L,
#     "America", "USA", "Chicago", 12L,
#     "America", "Argentina", "Buenos Aires", 54L,
#     "America", "Argentina", "Rosario", 36L,
#     "America", "Brasil", "Sao Paulo", 2L,
#     "America", "Brasil", "Rio de Janeiro", 64L,
#     "Europe", "Spain", "Madrid", 54L,
#     "Europe", "Spain", "Barcelona", 46L,
#     "Europe", "Spain", "Sevilla", 67L,
#     "Europe", "Italy", "Rome", 22L,
#     "Europe", "France", "Paris", 42L,
#     "Europe", "France", "Marseille", 91L
# )

# ui <- fluidPage(
#     h1("Drill Down in Shiny"),
#     echarts4rOutput("chart")
# )
# # Define server
# server <- function(input, output) {



#     plot_sales_data <- function(chart_data, chart_title, chart_color, chart_drill_to) {
#         sales_chart <- chart_data |>
#             e_chart(x = level) |>
#             e_bar(total, name = chart_title, color = chart_color)
#         # Adding the click observer only when drill_to is passed
#         if (!is.null(chart_drill_to)) {
#             sales_chart <- sales_chart |>
#                 e_on(
#                     query = "series.bar",
#                     # Set input values
#                     handler = glue::glue(
#                         "function(params){
#                         console.log(params)
#              Shiny.setInputValue(
#               'custom_bar_click',
#               {clicked_level: '<<chart_drill_to>>', drilled_place: params.name}, {priority: 'event'}
#              );
#            }",
#                         .open = "<<", .close = ">>"
#                     ),
#                     event = "click"
#                 )
#         }
#         return(sales_chart)
#     }

#     output$chart <- renderEcharts4r({
#     # Our custom input value that we send from the bar click
#     print(input$custom_bar_click)
#     if (is.null(input$custom_bar_click)) {
#       # Prepare data for chart
#       chart_data <- mock_data |>
#         group_by(level = level1) |>
#         summarise(total = sum(sales))

#       # Create chart
#       plot_sales_data(
#         chart_data = chart_data,
#         chart_title = "Sales by Continent",
#         chart_color = "#5470C6",
#         chart_drill_to = "level2"
#       )
#     } else if(input$custom_bar_click$clicked_level == "level2") {
#       # Prepare data for chart
#       chart_data <- mock_data |>
#         filter(level1 == input$custom_bar_click$drilled_place) |>
#         group_by(level = level2) |>
#         summarise(total = sum(sales))

#       # Create chart
#       plot_sales_data(
#         chart_data = chart_data,
#         chart_title = glue::glue(
#           "Sales by Country (Filtered for {input$custom_bar_click$drilled_place})"
#         ),
#         chart_color = "#91CC75",
#         chart_drill_to = "level3"
#       )
#     } else if(input$custom_bar_click$clicked_level == "level3") {
#       # Prepare data for chart
#       chart_data <- mock_data |>
#         filter(level2 == input$custom_bar_click$drilled_place) |>
#         group_by(level = level3) |>
#         summarise(total = sum(sales))

#       # Create chart
#       plot_sales_data(
#         chart_data = chart_data,
#         chart_title = glue::glue(
#           "Sales by City (Filtered for {input$custom_bar_click$drilled_place})"
#         ),
#         chart_color = "#FAC858",
#         chart_drill_to = NULL
#       )
#     }
#   })
# }
# shinyApp(ui, server)

# library(shiny)
# library(ggplot2)

# # UI
# ui <- fluidPage(
#   titlePanel("mtcars ggplot2 Example"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("xvar", "X-axis Variable", choices = names(mtcars), selected = "wt"),
#       selectInput("yvar", "Y-axis Variable", choices = names(mtcars), selected = "mpg")
#     ),
#     mainPanel(
#       plotOutput("scatterPlot", height = "60vh") # Adjust the height here
#     )
#   )
# )

# # Server
# server <- function(input, output) {
#   output$scatterPlot <- renderPlot(
#     {
#       ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar)) +
#         geom_point() +
#         labs(title = "Scatter Plot", x = input$xvar, y = input$yvar)
#     },
#     res = 150
#   )
# }

# # Run the app
# shinyApp(ui, server)

# library(forcats)
# library(readr)
# library(tidyr)
# library(dplyr)
# library(echarts4r)

# ko_rawdat <- read_tsv("/sysmiome/public_data/chula/picrust2/KO_pred_metagenome_unstrat_descrip.tsv") %>%
#     dplyr::rename_with(.cols = 1, ~"KO_ID")
# fpath <- system.file("ko_mapfiles", "ko00001.tsv.gz", package = "sysmiome.serve")
# ko_map <- read_tsv(fpath)

# ko_datfull <- ko_rawdat %>%
#     dplyr::select(-description) %>%
#     tidyr::pivot_longer(-c(KO_ID)) %>%
#     group_by(KO_ID) %>%
#     summarize(gene_count = sum(value)) %>%
#     left_join(ko_map, c("KO_ID" = "KO"))

# ko_sankey <- ko_datfull %>%
#     # Create connections between level1 and level2
#     transmute(source = level1, target = level2, value = gene_count) %>%
#     bind_rows(
#         # Add connections between level2 and level3
#         ko_datfull %>% transmute(source = level2, target = level3, value = gene_count)
#     )

# # Filtered time. Since the data is too much to display. I would get only top 20 of second and top 30 of the third
# aggregated_data <- ko_datfull %>%
#     group_by(level1, level2, level3) %>%
#     summarise(total_gene_count = sum(gene_count), .groups = "drop")

# # Step 2: Lump level2 and level3 into others categories
# aggregated_data <- aggregated_data %>%
#     dplyr::filter(level1 == "Metabolism") %>%
#     mutate(level2 = forcats::fct_lump_n(level2, n = 20, w = total_gene_count, other_level = "Other")) %>%
#     dplyr::filter(level2 != "Other") %>%

# aggregated_sankey <- aggregated_data %>%
#     # Create connections between level1 and level2
#     transmute(source = level1, target = level2, value = total_gene_count) %>%
#     bind_rows(
#         # Add connections between level2 and level3
#         aggregated_data %>% transmute(source = level2, target = level3, value = total_gene_count)
#     )

# aggregated_sankey |>
#   e_charts() |>
#   e_sankey(source, target, value)