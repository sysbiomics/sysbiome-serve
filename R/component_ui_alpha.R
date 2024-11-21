#' @param id, project_id
#' @import shiny
#' @noRd
ui_alpha <- function(id = "ID_ALPHA_MODULE") {
    ns <- NS(id)

    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = ns("alphadiv_measure"),
                label = "Measure(s)",
                choices = c(
                    "Shannon" = "diversity_shannon",
                    "Simpson" = "diversity_gini_simpson",
                    "invSimpson" = "diversity_inverse_simpson",
                    "Chao1 (richness)" = "chao1",
                    "ObservedOTUs" = "observed"
                ),
                selected = c("diversity_shannon", "diversity_gini_simpson", "diversity_inverse_simpson"),
                multiple = TRUE
            ),
            selectInput(
                inputId = ns("xlab_selection"),
                label = "X lab",
                choices = NULL,
                multiple = FALSE
            ),
            selectInput(
                inputId = ns("group_selection"),
                label = "group",
                choices = NULL,
                multiple = FALSE
            ),
        ),
        mainPanel(
            width = 10,
            tagList(
                div(
                    style = "height: 500px;", # set a fixed height for the plotOutput
                    shinycssloaders::withSpinner(
                        plotOutput(ns("plot_alpha")),
                    )
                ),
                fluidRow(
                    column(
                        width = 4,
                        selectInput(
                            inputId = ns("export_data"),
                            label = "Export plot as",
                            choices = c(
                                "PNG" = "png",
                                "PDF" = "pdf"
                            ),
                        ),
                    ),
                    column(
                        width = 4,
                        br(), # Align with the export data selection
                        downloadButton(
                            outputId = ns("download_plot"),
                            label = "Export"
                        ),
                    ),
                ),
                tableOutput(ns("table_alpha"))
            )
        )
    )
}

#' @param id, project_id
#' @import shiny ggplot2 dplyr
#' @noRd
sv_alpha <- function(id = "ID_ALPHA_MODULE", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {
            adat <- reactive({
                xchoices <- setdiff(colnames(project_obj()$get_filtered_metadata()$meta), "ID_sample")
                xchoices <- c("Select an option", xchoices) # Add an empty choice as the first option

                updateSelectInput(
                    session = session,
                    inputId = "xlab_selection",
                    choices = xchoices,
                    selected = NULL
                )

                updateSelectInput(
                    session = session,
                    inputId = "group_selection",
                    choices = xchoices,
                    selected = NULL
                )

                project_obj()$get_alpha_data()
            }) |>
                bindCache(project_obj()) |>
                bindEvent(project_obj())

            # Update input2 choices dynamically based on input1 selection
            observeEvent(input$xlab_selection, {
                xchoices <- setdiff(colnames(project_obj()$get_filtered_metadata()$meta), "ID_sample")
                selected1 <- input$xlab_selection
                updated_choices <- if (selected1 != "Select an option") setdiff(xchoices, selected1) else xchoices
                updateSelectInput(session, "group_selection", choices = updated_choices, selected = "Select an option")
            })

            output$plot_alpha <- renderPlot(
                {
                    dat_alpha <- adat()

                    xlab <- input$xlab_selection
                    group <- input$group_selection
                    measures <- input$alphadiv_measure

                    if (is.null(xlab) || xlab == "Select an option") {
                        # Show a placeholder message
                        plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
                        text(1, 1, "Please select an X lab first.", cex = 1.5, col = "red")
                        return()
                    }

                    .dat_alpha <- dat_alpha$alpha %>%
                        tidyr::pivot_longer(-ID_sample, names_to = "alpha_measure") %>%
                        dplyr::filter(alpha_measure %in% measures)

                    .dat_meta <- dat_alpha$meta

                    if (is.null(group) || group == "Select an option") {
                        .dat_alpha %>%
                            dplyr::left_join(.dat_meta, by = "ID_sample") %>%
                            ggplot(aes(x = factor(.data[[xlab]]), y = value)) +
                            geom_boxplot() +
                            facet_wrap(~alpha_measure, scales = "free_y")
                    } else {
                        .dat_alpha %>%
                            dplyr::left_join(.dat_meta, by = "ID_sample") %>%
                            ggplot(aes(x = factor(.data[[xlab]]), y = value)) +
                            geom_boxplot() +
                            facet_wrap(vars(interaction(.data[[group]], .data[["alpha_measure"]])), scales = "free_y")
                    }
                },
                res = 96
            )
        }
    )
}
