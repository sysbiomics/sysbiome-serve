#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
main_ui <- function() {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic
        fluidPage(
            shinycssloaders::withSpinner(
                navbarPage(
                    title = "SYSMIOME",
                    tabPanel("Home", homePage),
                    tabPanel("New Project", ui_new_project()),
                    tabPanel("Viewer", ui_viewer()),
                    tabPanel("Monitors", ui_monitor()),
                    tabPanel("Public Datasets", aboutPage),
                    tabPanel("About", aboutPage)
                )
            )
        )
    )
}

app_ui <- main_ui


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
    add_resource_path(
        "www",
        app_sys("app/www")
    )

    tags$head(
        favicon(),
        bundle_resources(
            path = app_sys("app/www"),
            app_title = "Sysmiome"
        )
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert()
    )
}
