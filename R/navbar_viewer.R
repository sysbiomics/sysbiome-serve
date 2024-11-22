ui_viewer <- function(id = "OUTER_UID") {
    ns <- NS(id)

    tagList(
        project_selection_ui("project_selector"),
        tabsetPanel(
            id = ns("mainTabs"),
            tabPanel("Overview", ui_overview(id = ns("overview"))),
            tabPanel("Alpha Diversity", ui_alpha(id = ns("alpha"))),
            tabPanel("Beta Diversity", ui_beta(id = ns("beta"))),
            tabPanel("Taxa analysis", ui_taxa(id = ns("taxa"))),
            tabPanel("Functional analysis", ui_function(id = ns("function"))),
        )
    )
}

#' @import echarts4r
ui_server <- function(id = "OUTER_UID", project_obj) {
    moduleServer(id, function(input, output, session) {
        # Separate reactive values for each type of data
        # Load data when project changes
        observe({
            req(project_obj())
            log_mssg <- glue::glue("Changing project_id to: {project_obj()$get_project_id()}")
            log_info(log_mssg)
        })

        sv_overview("overview", project_obj)
        sv_alpha("alpha", project_obj)
        sv_beta("beta", project_obj)
        sv_taxa("taxa", project_obj)
        sv_function("function", project_obj)
    })
}
