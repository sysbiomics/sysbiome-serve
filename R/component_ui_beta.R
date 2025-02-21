# #' @param id, project_id
# #' @import shiny
# #' @noRd
# ui_beta <- function(id = "ID_BETA_MODULE") {
#     ns <- NS(id)

#     sidebarLayout(
#         sidebarPanel(
#             width = 2,
#             selectInput(
#                 inputId = ns("beta_measure"),
#                 label = "Type of analysis",
#                 choices = c(
#                     "PCOA" = "pcoa",
#                     "NMDS" = "nmds"
#                 ),
#                 selected = c("PCOA"),
#                 multiple = FALSE
#             ),
#             selectInput(
#                 inputId = ns("color_measure"),
#                 label = "Colour by",
#                 choices = c("Select an option"),
#                 multiple = FALSE
#             )
#         ),
#         mainPanel(
#             width = 10,
#             tagList(
#                 div(
#                     style = "min-height: 300px;", # set a fixed height for the plotOutput
#                     shinycssloaders::withSpinner(
#                         plotOutput(ns("plot_beta"), height = "60vh")
#                     ),
#                 ),
#                 selectInput(
#                     inputId = ns("export_beta"),
#                     label = "Export plot as",
#                     choices = c(
#                         "PNG" = "png",
#                         "PDF" = "pdf"
#                     ),
#                 ),
#                 downloadButton(
#                     outputId = ns("export_beta_plot"),
#                     label = "Export plot"
#                 ),
#             )
#         )
#     )
# }

# sv_beta <- function(id = "ID_BETA_MODULE", project_obj) {
#     moduleServer(
#         id,
#         function(input, output, session) {

#             NS <- session$ns

#             dbe <- reactive({
#                 x_choices <- setdiff(colnames(project_obj()$get_filtered_metadata()$meta), "ID_sample")
#                 x_choices <- c("Select an option", x_choices)

#                 updateSelectInput(
#                     session = session,
#                     inputId = "color_measure",
#                     choices = x_choices,
#                     selected = NULL
#                 )

#                 project_obj()$get_beta_data()
#             }) |>
#                 bindCache(project_obj()) |>
#                 bindEvent(project_obj())

#             output$plot_beta <- renderPlot({
#                 dat_beta <- dbe()

#                 plotType <- input$beta_measure
#                 colourBy <- input$color_measure

#                 if (colourBy == "" || colourBy == "Select an option") {
#                     # Show a placeholder message
#                     plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#                     text(1, 1, "Please select a colour first.", cex = 1.5, col = "red")
#                     return()
#                 }

#                 dat <- dat_beta %>%
#                     purrr::pluck(plotType) %>%
#                     left_join(dat_beta$meta, by = "ID_sample")

#                 x_axis <- colnames(dat)[2]
#                 y_axis <- colnames(dat)[3]

#                 ggplot(data = dat, aes(x = .data[[x_axis]], y = .data[[y_axis]], color = factor(.data[[colourBy]]))) +
#                     geom_point(size = 4) +
#                     stat_ellipse(linetype = 2)
#             })

#             output$export_beta_plot <- downloadHandler(
#                 filename = function() {
#                     paste0("beta_diversity_plot.", input$export_beta)
#                 },
#                 content = function(file) {
#                     ggsave(file, plot = ggplot2::last_plot())
#                 }
#             )
#         }
#     )
# }


ui_beta <- function(id = "ID_BETA_MODULE") {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = ns("beta_measure"),
                label = "Type of analysis",
                choices = c(
                    "PCOA" = "pcoa",
                    "NMDS" = "nmds"
                ),
                selected = c("PCOA"),
                multiple = FALSE
            ),
            ui_select_metadata(ns("color_measure"))  # Correctly namespaced
        ),
        mainPanel(
            width = 10,
            tagList(
                div(
                    style = "min-height: 300px;", # set a fixed height for the plotOutput
                    shinycssloaders::withSpinner(
                        plotOutput(ns("plot_beta"), height = "60vh")
                    ),
                ),
                selectInput(
                    inputId = ns("export_beta"),
                    label = "Export plot as",
                    choices = c(
                        "PNG" = "png",
                        "PDF" = "pdf"
                    ),
                ),
                downloadButton(
                    outputId = ns("export_beta_plot"),
                    label = "Export plot"
                ),
                downloadButton(
                    outputId = ns("export_beta_data"), # New download button for data
                    label = "Export Data"
                ),
            )
        )
    )
}


sv_beta <- function(id = "ID_BETA_MODULE", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {

            color_measure <- sv_select_metadata("color_measure", project_obj, "Colour by")

            dbe <- reactive({
                project_obj()$get_beta_data()
            }) |>
                bindCache(project_obj()) |>
                bindEvent(project_obj())

            output$plot_beta <- renderPlot({
                dat_beta <- dbe()

                plotType <- input$beta_measure
                colourBy <- color_measure()  # Reactive value

                if (colourBy == "" || colourBy == "Select an option") {
                    # Show a placeholder message
                    plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
                    text(1, 1, "Please select a colour first.", cex = 1.5, col = "red")
                    return()
                }

                dat <- dat_beta %>%
                    purrr::pluck(plotType) %>%
                    left_join(dat_beta$meta, by = "ID_sample")

                x_axis <- colnames(dat)[2]
                y_axis <- colnames(dat)[3]

                ggplot(data = dat, aes(x = .data[[x_axis]], y = .data[[y_axis]], color = factor(.data[[colourBy]]))) +
                    geom_point(size = 4) +
                    stat_ellipse(linetype = 2)
            })

            output$export_beta_plot <- downloadHandler(
                filename = function() {
                    paste0("beta_diversity_plot.", input$export_beta)
                },
                content = function(file) {
                    ggsave(file, plot = ggplot2::last_plot())
                }
            )

            output$export_beta_data <- downloadHandler(
                filename = function() {
                    plotType <- input$beta_measure # Get the selected plot type
                    paste0(tolower(plotType), "_beta_diversity_data.tsv") # Prepend filename
                },
                content = function(file) {
                    dat_beta <- dbe()
                    plotType <- input$beta_measure
                    dat_to_export <- dat_beta %>% purrr::pluck(plotType) %>% left_join(dat_beta$meta, by = "ID_sample")
                    readr::write_tsv(dat_to_export, file)
                }
            )
        }
    )
}
