# #' @param id, project_id
# #' @import shiny
# #' @noRd
# ui_alpha <- function(id = "ID_ALPHA_MODULE") {
#     ns <- NS(id)

#     sidebarLayout(
#         sidebarPanel(
#             width = 2,
#             selectInput(
#                 inputId = ns("alphadiv_measure"),
#                 label = "Measure(s)",
#                 choices = c(
#                     "Shannon" = "diversity_shannon",
#                     "Simpson" = "diversity_gini_simpson",
#                     "invSimpson" = "diversity_inverse_simpson",
#                     "Chao1 (richness)" = "chao1",
#                     "ObservedOTUs" = "observed"
#                 ),
#                 selected = c("diversity_shannon", "diversity_gini_simpson", "diversity_inverse_simpson"),
#                 multiple = TRUE
#             ),
#             selectInput(
#                 inputId = ns("xlab_selection"),
#                 label = "X lab",
#                 choices = c("Select an option"),
#                 multiple = FALSE
#             ),
#             selectInput(
#                 inputId = ns("group_selection"),
#                 label = "group",
#                 choices = NULL,
#                 multiple = FALSE
#             ),
#         ),
#         mainPanel(
#             width = 10,
#             tagList(
#                 div(
#                     style = "min-height: 300px;", # set a fixed height for the plotOutput
#                     shinycssloaders::withSpinner(
#                         plotOutput(ns("plot_alpha"), height = "60vh"),
#                     )
#                 ),
#                 selectInput(
#                     inputId = ns("export_alpha_plot"),
#                     label = "Export plot as",
#                     choices = c(
#                         "PNG" = "png",
#                         "PDF" = "pdf"
#                     ),
#                 ),
#                 downloadButton(
#                     outputId = ns("download_alpha_plot"),
#                     label = "Export plot"
#                 ),
#                 downloadButton(
#                     outputId = ns("download_alpha_table"),
#                     label = "Export Table"
#                 ),
#                 tableOutput(ns("table_alpha"))
#             )
#         )
#     )
# }

# #' @param id, project_id
# #' @import shiny ggplot2 dplyr
# #' @noRd
# sv_alpha <- function(id = "ID_ALPHA_MODULE", project_obj) {
#     moduleServer(
#         id,
#         function(input, output, session) {

#             adat <- reactive({
#                 xchoices <- setdiff(colnames(project_obj()$get_cat_metadata()$meta), "ID_sample")
#                 xchoices <- c(PLACE_HOLDER_CHOICE, xchoices) # Add an empty choice as the first option

#                 updateSelectInput(
#                     session = session,
#                     inputId = "xlab_selection",
#                     choices = xchoices,
#                     selected = PLACE_HOLDER_CHOICE
#                 )

#                 updateSelectInput(
#                     session = session,
#                     inputId = "group_selection",
#                     choices = xchoices,
#                     selected = NULL
#                 )

#                 project_obj()$get_alpha_data()
#             }) |>
#                 bindCache(project_obj()) |>
#                 bindEvent(project_obj())

#             # Update input2 choices dynamically based on input1 selection
#             observeEvent(input$xlab_selection, {
#                 xchoices <- setdiff(colnames(project_obj()$get_cat_metadata()$meta), "ID_sample")
#                 selected1 <- input$xlab_selection
#                 updated_choices <- if (selected1 != "Select an option") setdiff(xchoices, selected1) else xchoices
#                 updateSelectInput(session, "group_selection", choices = updated_choices, selected = "Select an option")
#             })

#             output$plot_alpha <- renderPlot(
#                 {
#                     dat_alpha <- adat()

#                     xlab <- input$xlab_selection
#                     group <- input$group_selection
#                     measures <- input$alphadiv_measure

#                     if (! is_choice_valid(xlab)) {
#                         # Show a placeholder message
#                         plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#                         text(1, 1, "Please select an X lab first.", cex = 1.5, col = "red")
#                         return()
#                     }

#                     .dat_alpha <- dat_alpha$alpha %>%
#                         tidyr::pivot_longer(-ID_sample, names_to = "alpha_measure") %>%
#                         dplyr::filter(alpha_measure %in% measures)

#                     .dat_meta <- dat_alpha$meta

#                     if (is.null(group) || group == "Select an option") {
#                         .dat_alpha %>%
#                             dplyr::left_join(.dat_meta, by = "ID_sample") %>%
#                             ggplot(aes(x = factor(.data[[xlab]]), y = value)) +
#                             geom_boxplot() +
#                             facet_wrap(~alpha_measure, scales = "free_y")
#                     } else {
#                         .dat_alpha %>%
#                             dplyr::left_join(.dat_meta, by = "ID_sample") %>%
#                             ggplot(aes(x = factor(.data[[xlab]]), y = value)) +
#                             geom_boxplot() +
#                             facet_grid(rows = vars(.data[["alpha_measure"]]), cols = vars(.data[[group]]), scales = "free_y")
#                     }
#                 },
#                 res = 96
#             )

#             # Generate a downloadable plot
#             output$download_alpha_plot <- downloadHandler(
#                 filename = function() {
#                     paste0("alpha_diversity_plot.", input$export_alpha_plot)
#                 },
#                 content = function(file) {
#                     ggsave(file, plot = ggplot2::last_plot())
#                 }
#             )

#             # Generate a downloadable table
#             output$download_alpha_table <- downloadHandler(
#                 filename = function() {
#                     "alpha_diversity_table.tsv"
#                 },
#                 content = function(file) {
#                     dat_alpha <- adat()
#                     readr::write_tsv(dat_alpha$alpha, file)
#                 }
#             )
#         }
#     )
# }

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
            ui_select_metadata(ns("xlab_selection")), # Use the dynamic select module here
            ui_select_metadata(ns("group_selection")) # And here
        ),
        mainPanel(
            width = 10,
            tagList(
                div(
                    style = "min-height: 300px;", # set a fixed height for the plotOutput
                    shinycssloaders::withSpinner(
                        plotOutput(ns("plot_alpha"), height = "60vh"),
                    )
                ),
                selectInput(
                    inputId = ns("export_alpha_plot"),
                    label = "Export plot as",
                    choices = c(
                        "PNG" = "png",
                        "PDF" = "pdf"
                    ),
                ),
                downloadButton(
                    outputId = ns("download_alpha_plot"),
                    label = "Export plot"
                ),
                downloadButton(
                    outputId = ns("download_alpha_table"),
                    label = "Export Table"
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

            xlab_selection <- sv_select_metadata("xlab_selection", project_obj, "X lab")
            group_selection <- sv_select_metadata("group_selection", project_obj, "Group")

            # Update group_selection choices (with placeholder)
            observeEvent(xlab_selection(), { # React to xlab_selection changes
                xchoices <- setdiff(colnames(project_obj()$get_cat_metadata()$meta), "ID_sample")
                selected_xlab <- xlab_selection()

                updated_choices <- if (selected_xlab != "Select an option") {
                    c("Select an option", setdiff(xchoices, selected_xlab)) # Include placeholder
                } else {
                    c("Select an option", xchoices) # Include placeholder
                }

                updateSelectInput(session, "group_selection-select_metadata", choices = updated_choices, selected = "Select an option")
            })

            adat <- reactive({
                project_obj()$get_alpha_data()
            }) |>
                bindCache(project_obj()) |>
                bindEvent(project_obj()) # React to project_obj changes


            output$plot_alpha <- renderPlot(
                {
                    dat_alpha <- adat()

                    xlab <- xlab_selection()
                    group <- group_selection()
                    measures <- input$alphadiv_measure

                    if (is.null(xlab) || xlab == "Select an option") {
                        plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
                        text(1, 1, "Please select an X lab first.", cex = 1.5, col = "red")
                        return()
                    }

                    .dat_alpha <- dat_alpha$alpha %>%
                        tidyr::pivot_longer(-ID_sample, names_to = "alpha_measure") %>%
                        dplyr::filter(alpha_measure %in% measures)

                    .dat_meta <- dat_alpha$meta

                    gg_plot <- .dat_alpha %>%
                        dplyr::left_join(.dat_meta, by = "ID_sample") %>%
                        ggplot(aes(x = factor(.data[[xlab]]), y = value)) +
                        geom_boxplot() +
                        facet_wrap(~alpha_measure, scales = "free_y")


                    if (!is.null(group) && group != "Select an option") {
                        gg_plot <- gg_plot + facet_grid(rows = vars(.data[["alpha_measure"]]), cols = vars(.data[[group]]), scales = "free_y")
                    }

                    gg_plot + theme_bw()
                },
                res = 96
            )


            # Generate a downloadable plot
            output$download_alpha_plot <- downloadHandler(
                filename = function() {
                    paste0("alpha_diversity_plot.", input$export_alpha_plot)
                },
                content = function(file) {
                    ggsave(file, plot = ggplot2::last_plot())
                }
            )

            # Generate a downloadable table
            output$download_alpha_table <- downloadHandler(
                filename = function() {
                    "alpha_diversity_table.tsv"
                },
                content = function(file) {
                    dat_alpha <- adat()
                    readr::write_tsv(dat_alpha$alpha, file)
                }
            )
        }
    )
}
