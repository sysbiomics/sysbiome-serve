#' @param id, project_id
#' @import shiny
#' @noRd
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
            selectInput(
                inputId = ns("color_measure"),
                label = "Colour by",
                choices = NULL,
                multiple = FALSE
            )
        ),
        mainPanel(
            width = 10,
            tagList(
                div(
                    shinycssloaders::withSpinner(
                        plotOutput(ns("plot_beta"))
                    ),
                ),
                selectInput(
                    inputId = ns("export_data"),
                    label = "Export plot as",
                    choices = c(
                        "PNG" = "png",
                        "PDF" = "pdf"
                    ),
                ),
                actionButton(
                    inputId = ns("export_data_button"),
                    label = "Export"
                ),
            )
        )
    )
}

sv_beta <- function(id = "ID_BETA_MODULE", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {
            dbe <- reactive({
                x_choices <- setdiff(colnames(project_obj()$get_filtered_metadata()$meta), "ID_sample")
                x_choices <- c("", x_choices)

                updateSelectInput(
                    session = session,
                    inputId = "color_measure",
                    choices = x_choices,
                    selected = NULL
                )

                project_obj()$get_beta_data()
            }) |>
                bindCache(project_obj()) |>
                bindEvent(project_obj())


            # observe({
            #     req(project_obj())

            #     xchoices <- setdiff(colnames(project_obj()$get_filtered_metadata()$meta), "ID_sample")
            #     xchoices <- c("", xchoices) # Add an empty choice as the first option

            #     updateSelectInput(
            #         session = session,
            #         inputId = "color_measure",
            #         choices = xchoices,
            #         selected = NULL
            #     )
            # })

            output$plot_beta <- renderPlot(
                {
                    dat_beta <- dbe()

                    plotType <- input$beta_measure
                    colourBy <- input$color_measure

                    dat <- dat_beta %>%
                        purrr::pluck(plotType) %>%
                        left_join(dat_beta$meta, by = "ID_sample")

                    x_axis <- colnames(dat)[2]
                    y_axis <- colnames(dat)[3]

                    ggplot(data = dat, aes(x = .data[[x_axis]], y = .data[[y_axis]], color = factor(.data[[colourBy]]))) +
                        geom_point(size = 4) +
                        stat_ellipse(linetype = 2)
                },
                width = 600,
            )
        }
    )
}
