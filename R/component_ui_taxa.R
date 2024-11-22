ui_taxa <- function(id = "ID_TAXA_MODULE") {
    ns <- NS(id)

    # Check if phyloseq has rank as upper-case or lower case then...
    taxbarplot <- sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = ns("tax_level"),
                label = "Type of analysis",
                choices = c(
                    "Phylum" = "Phylum",
                    "Order" = "Order",
                    "Family" = "Family"
                ),
                selected = c("Phylum"),
                multiple = FALSE
            ),
            selectInput(
                inputId = ns("tax_group"),
                label = "Group",
                choices = NULL,
                multiple = FALSE
            )
        ),
        mainPanel(
            width = 10,
            fluidPage(
                div(
                    style = "min-height: 300px;", # set a fixed height for the plotOutput
                    shinycssloaders::withSpinner(
                        plotOutput(ns("plot_bar"), height = "60vh")
                    )
                )
            )
        )
    )

    kronaplot <- sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = ns("select_group"),
                label = "Group",
                choices = NULL,
                multiple = FALSE
            )
        ),
        mainPanel(
            width = 10,
            actionButton(ns("generate_html"), "Generate HTML"),
            downloadButton(ns("download_html"), "Download HTML"),
            htmlOutput(ns("kronaframe"))
        )
    )

    testplot <- div("Hello")

    tabsetPanel(
        tabPanel("Barplot", taxbarplot),
        tabPanel("Kronaplot", kronaplot),
        tabPanel("testPlot", testplot),
    )
}

#' @import microbiome phyloseq
#' @importFrom microbiome meta
#' @noRd
sv_taxa <- function(id = "ID_TAXA_MODULE", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {
            observe({
                x_choices <- setdiff(colnames(project_obj()$get_filtered_metadata()$meta), "ID_sample")
                x_choices <- c("", x_choices)

                # print(x_choices)

                updateSelectInput(
                    session = session,
                    inputId = "tax_group",
                    choices = x_choices,
                    selected = NULL
                )

                updateSelectInput(
                    session = session,
                    inputId = "select_group",
                    choices = x_choices,
                    selected = NULL
                )
            })

            ## Setting downloadable krona
            temp_file_path <- reactiveVal(NULL) # Reactive value to store the temp file path

            output$download_html <- downloadHandler(
                filename = function() {
                    paste("krona_plot", Sys.Date(), ".html", sep = "")
                },
                content = function(file) {
                    req(temp_file_path()) # Ensure a file path is available
                    file.copy(temp_file_path(), file)
                }
            )

            # Generate krona
            observeEvent(input$generate_html, {
                dat_ps <- project_obj()$get_taxa_data()$ps

                grouping <- input$select_group

                if (is.null(grouping) || grouping == "") { # if no grouping
                    # Save to a temporary HTML file
                    temp_file <- tempfile(fileext = ".html")
                    krona(dat_ps, temp_file)

                    temp_file_path(temp_file)

                    # Make the temp file available as a URL in Shiny
                    temp_url <- paste("tmp", basename(temp_file), sep = "/")
                    addResourcePath("tmp", dirname(temp_file))

                    logger::log_info("Create HTML successfully")

                    output$kronaframe <- renderUI({
                        tags$iframe(src = temp_url, height = 800, width = "70%")
                    })

                    # Update the iframe src attribute to the new HTML file
                    updateQueryString("?generated=true", mode = "push")
                    session$sendCustomMessage(type = "updateIframe", message = temp_url)
                } else { # If has group.  # Filter ps into each subset of group

                    metadata <- sample_data(dat_ps)
                    group_names <- unique(metadata[[grouping]])

                    tmp_list <- list()
                    for (group_name in group_names) {
                        # Subset phyloseq object

                        var_bool <- phyloseq::sample_data(dat_ps)[[grouping]] == group_name
                        subset_physeq <- phyloseq::prune_samples(var_bool, dat_ps)

                        # Perform operations on the subset
                        temp_file <- tempfile(fileext = ".html")
                        # Interim text cannot use space...
                        group_name_s <- gsub("[[:space:]]", "_", group_name)
                        krona(subset_physeq, temp_file, name = group_name_s)
                        tmp_list[[group_name]] <- temp_file
                    }

                    temp_file <- tempfile(fileext = ".html")
                    temp_file_path(temp_file)
                    merge_krona(unlist(tmp_list), temp_file)
                    temp_url <- paste("tmp", basename(temp_file), sep = "/")
                    addResourcePath("tmp", dirname(temp_file))

                    logger::log_info("Create HTML successfully")

                    output$kronaframe <- renderUI({
                        tags$iframe(src = temp_url, height = 800, width = "70%")
                    })

                    # Update the iframe src attribute to the new HTML file
                    updateQueryString("?generated=true", mode = "push")
                    session$sendCustomMessage(type = "updateIframe", message = temp_url)
                }
            })

            observe({
                session$sendCustomMessage(
                    type = "updateIframe",
                    message = ""
                )
            })


            output$plot_bar <- renderPlot({
                req(project_obj())

                dat_ps <- project_obj()$get_taxa_data()$ps

                tlevel <- input$tax_level
                tax_group <- input$tax_group

                if (tax_group != "") {
                    dat_ps %>%
                        aggregate_taxa(tlevel) %>%
                        microbiome::transform("compositional") %>%
                        FastMelt(c(tax_group)) %>%
                        ggplot() +
                        aes(x = ID_sample, y = abn, fill = .data[[tlevel]]) +
                        facet_wrap(~ .data[[tax_group]], scales = "free_x") +
                        geom_bar(stat = "identity") +
                        scale_x_discrete(guide = guide_axis(angle = 45))
                } else {
                    dat_ps %>%
                        aggregate_taxa(tlevel) %>%
                        microbiome::transform("compositional") %>%
                        FastMelt() %>%
                        ggplot() +
                        aes(x = ID_sample, y = abn, fill = .data[[tlevel]]) +
                        geom_bar(stat = "identity") +
                        scale_x_discrete(guide = guide_axis(angle = 45))
                }
            })
        }
    )
}
