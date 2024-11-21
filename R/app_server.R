#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny shinycssloaders shiny.router glue
#' @noRd
app_server <- function(input, output, session) {

    log_info("Starting SYSMIOME")

    # Hard code the base folder
    session$userData$BASEFOLDER <- "/sysmiome"

    tower <- Tower$new("/sysmiome")
    project_id <- reactiveVal("demo")
    project_id <- project_selection_server("project_selector", tower)

    project_obj <- reactive({
        return(tower$get_project(project_id()))
    })

    shiny::isolate({
        hostname <- session$clientData$url_hostname
        pathname <- session$clientData$url_pathname
    })

    ui_server(project_obj = project_obj)

    sv_new_project(tower = tower)
    sv_monitor(tower = tower)

    # Define HTTP API endpoint
    ggplot_url_svg <- session$registerDataObj(
        name = "example_plot_svg",
        data = list(),
        filterFunc = function(data, req) {
            if (req$REQUEST_METHOD == "GET") {
                params <- parseQueryString(req$QUERY_STRING)
                gg <- generate_ggplot(params$title)
                tmp_path <- paste0(tempfile(), ".svg")
                ggsave(tmp_path, plot = gg)
                #' Reading a plot svg file as binary and passing it to the response
                readBin(tmp_path, "raw", 100000000) %>%
                    httpResponse(200, "image/svg+xml", .)
            }
        }
    )

    #' Inform React about HTTP API endpoints through Websocket
    #' Otherwise React won't be able to use it
    #' This because API endpiont need a session id.
    session$sendCustomMessage("urls", {
        list(
            ggplot_url_svg = ggplot_url_svg
        )
    })

    # uri <- paste0(
    #   "http://", session$request$HTTP_HOST, "/session/",
    #   session$token, "/dataobj/", namekk
    # )
    # print(uri)
}
