#' @param id, project_id
#' @import shiny echarts4r
#' @importFrom DT DTOutput
#' @noRd
ui_function <- function(id = "ID_TAXA_MODULE") {
    ns <- NS(id)

    testPlot <- sidebarLayout(
        sidebarPanel(
            width = 2,
            tagList()
        ),
        mainPanel(
            width = 10,
            tagList(
                echarts4rOutput(ns("sankey_chart"))
            )
        )
    )

    ecPlot <- tagList(
        DTOutput(ns("ecTable"))
    )

    koPlot <- tagList(
        echarts4rOutput(ns("ko_sankey"), height = "80vh"), # Output for the Sankey diagram
        DTOutput(ns("koTable"))
    )

    ptPlot <- tagList(
        DTOutput(ns("ptTable"))
    )


    tabsetPanel(
        tabPanel("EC", ecPlot),
        tabPanel("KO", koPlot),
        tabPanel("Pathway", ptPlot),
        tabPanel("test", testPlot),
    )
}

#' @import microbiome echarts4r
#' @importFrom microbiome meta
#' @importFrom DT renderDT datatable
#' @noRd
sv_function <- function(id = "ID_FUNCTION_MODULE", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {
            renderTab <- function(data) {
                DT::datatable(
                    data,
                    options = list(
                        pageLength = 15, # Number of rows per page
                        lengthMenu = c(15, 25, 50, 100, 200), # Options for number of rows per page
                        searching = TRUE, # Enable searching
                        scrollX = TRUE, # Enable horizontal scrolling
                        scrollY = 400, # Vertical scroll height
                        deferRender = TRUE # Delay table rendering until needed
                    )
                )
            }

            output$ko_sankey <- renderEcharts4r({
                data <- project_obj()$get_ko_sankey()

                # data <- data.frame(
                #     source = c("A", "A", "B", "C", "C"),
                #     target = c("B", "C", "C", "D", "E"),
                #     value = c(5, 10, 15, 20, 25)
                # )
                data |>
                    e_charts() |>
                    e_sankey(source, target, value) |>
                    e_tooltip(trigger = "item") # Add tooltips to show info on hover
            })

            output$koTable <- renderDT({
                # For table, I think
                ko_rawdat <- project_obj()$get_kofunc_data()
                renderTab(ko_rawdat)
            })

            output$ecTable <- renderDT({
                data <- project_obj()$get_ecfunc_data()
                renderTab(data)
            })

            output$ptTable <- renderDT({
                data <- project_obj()$get_ptfunc_data()
                renderTab(data)
            })

            output$sankey_chart <- renderEcharts4r({
                data <- data.frame(
                    source = c("A", "A", "B", "C", "C"),
                    target = c("B", "C", "C", "D", "E"),
                    value = c(5, 10, 15, 20, 25)
                )
                data |>
                    e_charts() |>
                    e_sankey(source, target, value) |>
                    e_tooltip(trigger = "item") # Add tooltips to show info on hover
            })
        }
    )
}
