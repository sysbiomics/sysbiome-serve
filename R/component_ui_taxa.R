# From https://graphicdesign.stackexchange.com/questions/3682/where-can-i-find-a-large-palette-set-of-contrasting-colors-for-coloring-many-d
a1_pallete <- c(
    "#F0A0FF", "#0075DC", "#993F00", "#4C005C", "#191919", "#005C31",
    "#2BCE48", "#FFCC99", "#808080", "#94FFB5", "#8F7C00", "#9DCC00",
    "#C20088", "#003380", "#FFA405", "#FFA8BB", "#426600", "#FF0010",
    "#5EF1F2", "#00998F", "#E0FF66", "#740AFF", "#990000", "#FFFF80",
    "#FFE100", "#FF5005"
)

# Where did I get this?
big_pallete <- c(
    "#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
    "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
    "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
    "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
    "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
    "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
    "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
    "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
    "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
    "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
    "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
    "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
    "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C"
)

# For rendering minus in MDPI
label_trueminus <- function(.x) {
    ifelse(sign(.x) == -1, paste0("\u2212", abs(.x)), .x)
}

# Reorder unknown into last position
relvl <- function(fcts) {
    if ("Unspecified" %in% levels(fcts)) {
        fcts <- forcats::fct_relevel(fcts, "Unspecified", after = 0L)
    }
    if ("Uncultured" %in% levels(fcts)) {
        fcts <- forcats::fct_relevel(fcts, "Uncultured", after = 0L)
    }
    if ("Other" %in% levels(fcts)) {
        fcts <- forcats::fct_relevel(fcts, "Other", after = 0L)
    }
    fcts
}

#
# / Functions
#

#
# Plot individual samples each
#
indplot <- function(ps, taxa_level, lump_n, lgd) {
    taxdat <- aggregate_taxa(ps, taxa_level) %>%
        microbiome::transform("compositional") %>%
        FastMelt(colnames(phyloseq::sample_data(ps))) %>%
        mutate("{taxa_level}" := case_when(
            .data[[taxa_level]] == "" ~ "Unspecified",
            stringr::str_ends(.data[[taxa_level]], "__") ~ "Unspecified",
            .data[[taxa_level]] == "uncultured" ~ "Uncultured",
            TRUE ~ .data[[taxa_level]]
        )) %>%
        mutate("{taxa_level}" := forcats::fct_lump_n(.data[[taxa_level]], lump_n, w = abn)) %>%
        mutate("{taxa_level}" := forcats::fct_reorder(.data[[taxa_level]], abn)) %>%
        mutate("{taxa_level}" := relvl(.data[[taxa_level]])) %>%
        mutate(abn = abn * 100)

    taxdat
}


# Barplot Module UI
ui_barplot <- function(id = "ID_BAR_COMP") {
    ns <- NS(id)

    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = ns("tax_level"),
                label = "Taxa level",
                choices = c(
                    "Phylum" = "Phylum",
                    "Class" = "Class",
                    "Order" = "Order",
                    "Family" = "Family",
                    "Genus" = "Genus"
                ),
                selected = c("Phylum"),
                multiple = FALSE
            ),
            ui_select_metadata(ns("tax_group")) # Use the new component here
        ),
        mainPanel(
            width = 10,
            fluidPage(
                div(
                    style = "min-height: 300px;",
                    shinycssloaders::withSpinner(
                        plotOutput(ns("plot_bar"), height = "60vh")
                    )
                )
            )
        )
    )
}

# Barplot Module Server
sv_barplot <- function(id = "ID_BAR_COMP", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {

            tax_group_reactive <- sv_select_metadata(id = "tax_group", project_obj, "Group")

            output$plot_bar <- renderPlot({
                req(project_obj())
                dat_ps <- project_obj()$get_taxa_data()$ps
                tlevel <- input$tax_level
                tax_group <- tax_group_reactive()

                if (!is_choice_valid(tlevel)) {
                    plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
                    text(1, 1, "Please select taxonomy level first.", cex = 1.5, col = "red")
                    return()
                }

                ggbase <- indplot(dat_ps, tlevel, 20, tlevel) %>%
                        ggplot() +
                        aes(x = ID_sample, y = abn, fill = .data[[tlevel]]) +
                        geom_bar(stat = "identity") +
                        ylab("Relative Abundance (%)") +
                        scale_fill_manual(values = a1_pallete) +
                        scale_x_discrete(guide = guide_axis(angle = 60))

                if (is_choice_valid(tax_group)) {
                    ggbase +
                        facet_wrap(~ .data[[tax_group]], scales = "free_x") +
                        theme(axis.text.x = element_text(size = 8)) + theme_bw() + scale_y_continuous(limits = c(0, 100), expand = c(0, 0))
                } else {
                    ggbase +
                        theme(axis.text.x = element_text(size = 8))  + theme_bw() + scale_y_continuous(limits = c(0, 100), expand = c(0, 0))
                }
            }) |> bindCache(project_obj(), input$tax_level, tax_group_reactive()) # Cache with the reactive value
        }
    )
}

# Krona Module UI
ui_krona <- function(id = "ID_KRONA_COMP") {
    ns <- NS(id)

    sidebarLayout(
        sidebarPanel(
            width = 2,
            ui_select_metadata(ns("krona_group")) # Use the new component here
        ),
        mainPanel(
            width = 10,
            actionButton(ns("generate_html"), "Generate HTML"),
            downloadButton(ns("download_html"), "Download HTML"),
            htmlOutput(ns("kronaframe"))
        )
    )
}

# Krona Module Server
sv_krona <- function(id = "ID_KRONA_COMP", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {
            temp_file_path <- reactiveVal(NULL)

            krona_group_reactive <- sv_select_metadata("krona_group", project_obj, "Group")

            output$download_html <- downloadHandler(
                filename = function() {
                    paste("krona_plot", Sys.Date(), ".html", sep = "")
                },
                content = function(file) {
                    req(temp_file_path())
                    file.copy(temp_file_path(), file)
                }
            )

            observeEvent(input$generate_html, {
                dat_ps <- project_obj()$get_taxa_data()$ps
                grouping <- krona_group_reactive() # Use reactive value here
                hidePageSpinner()

                if (!is_choice_valid(grouping)) {

                    showPageSpinner(caption = (div(strong("Generating KRONA"), br(), em("Please wait"))))

                    temp_file <- tempfile(fileext = ".html")
                    krona(dat_ps, temp_file)
                    temp_file_path(temp_file)

                    temp_url <- paste("tmp", basename(temp_file), sep = "/")
                    addResourcePath("tmp", dirname(temp_file))

                    hidePageSpinner()

                    output$kronaframe <- renderUI({
                        tags$iframe(src = temp_url, height = 800, width = "70%")
                    })

                    updateQueryString("?generated=true", mode = "push")
                    session$sendCustomMessage(type = "updateIframe", message = temp_url)
                } else {

                    showPageSpinner(caption = (div(strong("Generating KRONA"), br(), em("Please wait"))))

                    metadata <- sample_data(dat_ps)
                    group_names <- unique(metadata[[grouping]])

                    tmp_list <- list()
                    for (group_name in group_names) {
                        var_bool <- phyloseq::sample_data(dat_ps)[[grouping]] == group_name
                        subset_physeq <- phyloseq::prune_samples(var_bool, dat_ps)

                        temp_file <- tempfile(fileext = ".html")
                        group_name_s <- gsub("[[:space:]]", "_", group_name)
                        krona(subset_physeq, temp_file, name = group_name_s)
                        tmp_list[[group_name]] <- temp_file
                    }

                    temp_file <- tempfile(fileext = ".html")
                    temp_file_path(temp_file)
                    merge_krona(unlist(tmp_list), temp_file)
                    temp_url <- paste("tmp", basename(temp_file), sep = "/")
                    addResourcePath("tmp", dirname(temp_file))

                    hidePageSpinner()

                    output$kronaframe <- renderUI({
                        tags$iframe(src = temp_url, height = 800, width = "70%")
                    })

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
        }
    )
}

# Correlation Module UI
ui_correlation <- function(id = "ID_CORR_COMP") {
    ns <- NS(id)

    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = ns("cor_tax_level"),
                label = "Taxa level",
                choices = NULL,
                multiple = FALSE
            ),
        ),
        mainPanel(
            width = 10,
            fluidPage(
                div(
                    style = "min-height: 300px;",
                    shinycssloaders::withSpinner(
                        plotOutput(ns("plot_correlation"), height = "60vh"),
                    )
                ),
                selectInput(
                    inputId = ns("export_corr_type"),
                    label = "Export plot as",
                    choices = c(
                        "PNG" = "png",
                        "PDF" = "pdf"
                    ),
                ),
                downloadButton(ns("export_heatmap_correlation"), "Download heatmap"),
                downloadButton(ns("export_matrix_correlation"), "Download correlation matrix"),
            )
        )
    )
}

# Correlation module server
#' @import Hmisc openxlsx
sv_correlation <- function(id = "ID_CORR_COMP", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {

            observeEvent(project_obj(), {
                taxa_names <- c("Select an option", project_obj()$get_taxa_name())
                updateSelectInput(
                    session = session,
                    inputId = "cor_tax_level",
                    choices = taxa_names,
                    selected = "Select an option"
                )
            })

            cur_cor <- reactive({
                cor_tax_lvl <- input$cor_tax_level
                if (!is_choice_valid(cor_tax_lvl)){
                    return(NULL)
                }
                project_obj()$get_correlation_data(cor_tax_lvl)
            }) |>
                bindCache(input$cor_tax_level) |>
                bindEvent(input$cor_tax_level)

            cur_heat <- reactive({
                cor_tax_lvl <- input$cor_tax_level
                if (!is_choice_valid(cor_tax_lvl)){
                    return(NULL)
                }
                project_obj()$get_taxa_data()$ps %>%
                    tax_agg(rank = input$cor_tax_level) %>%
                    cor_heatmap(
                        taxa = tax_top(., n = 20),
                        seriation_method = "Identity",
                        tax_anno = NULL,
                        cor = "spearman",
                    )
            }) |>
                bindCache(input$cor_tax_level) |>
                bindEvent(input$cor_tax_level)

            output$plot_correlation <- renderPlot(
                {
                    corr_dat <- cur_cor()
                    if (is.null(corr_dat)) {
                        # Show a placeholder message
                        plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
                        text(1, 1, "Please select a taxonomy level first.", cex = 1.5, col = "red")
                        return()
                    }

                    cur_heat()

                },

                # second argument.
                res = 96
            )

            # Generate a downloadable plot
            output$export_heatmap_correlation <- downloadHandler(
                filename = function() {
                    tax_level <- input$cor_tax_level
                    graphic_type <- input$export_corr_type # PNG/PDF
                    name <- paste0(tax_level, "_correlation.", graphic_type)
                    name
                },
                content = function(file) {
                    heatmap_obj <- cur_heat()
                    if (is.null(heatmap_obj)) return()

                    width <- 10
                    height <- 8
                    res <- 300

                    if (input$export_corr_type == "png") {
                        png(file, width = width, height = height, units = "in", res = res)
                    } else if (input$export_corr_type == "pdf") {
                        pdf(file, width = width, height = height)
                    }

                    ComplexHeatmap::draw(heatmap_obj)
                    dev.off()
                }
            )
            output$export_matrix_correlation <- downloadHandler(
                filename = function() {
                    tax_level <- input$cor_tax_level
                    paste0(tax_level, "_", "correlation.tsv")
                },
                content = function(file) {
                    output <- cur_cor() %>%
                        rownames_to_column(var = "FeatureID") %>%
                        as_tibble()

                    readr::write_tsv(output, file)
                }
            )
        }
    )
}


# Main Taxa Module UI
ui_taxa <- function(id = "ID_TAXA_MODULE") {
    ns <- NS(id)

    tabsetPanel(
        tabPanel("Barplot", ui_barplot(ns("barplot"))),
        tabPanel("Kronaplot", ui_krona(ns("krona"))),
        tabPanel("Correlation", ui_correlation(ns("correlation")))
    )
}

# Main Taxa Module Server
sv_taxa <- function(id = "ID_TAXA_MODULE", project_obj) {
    moduleServer(
        id,
        function(input, output, session) {
            sv_barplot("barplot", project_obj)
            sv_krona("krona", project_obj)
            sv_correlation("correlation", project_obj)
        }
    )
}