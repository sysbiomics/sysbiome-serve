# Sample JSON data
json_data <- '{
    "id": "1",
    "owner": "SYSMIOME-USER-1",
    "name": "Project 1",
    "run_command": {
        "parameter": "nextflow run main.nf -profile docker",
        "dataset": "public/nextflow-datasets/rnaseq"
    },
    "status": "finished"
}'

#' @import shiny
ui_overview <- function(id = "ID_OVERVIEW_VIEWER") {
    ns <- NS(id)
    tagList(
        titlePanel("Project Overview"),
        tableOutput(ns("json_table")),
        downloadButton(
            outputId = ns("download_data"),
            label = "Download data"
        ),
    )
}

#' @import jsonlite
#' @noRd
sv_overview <- function(id = "ID_OVERVIEW_VIEWER", project_obj) {
    moduleServer(id, function(input, output, session) {
        parsed_data <- reactive({
            jsonlite::fromJSON(json_data)
        })

        output$download_data <- downloadHandler(
            filename = function() {
                "phyloseq.RDS"
            },
            content = function(con) {
                proj_loc <- "/sysmiome/public_data/chula_03_phase"
                file_loc <- file.path(proj_loc, "/phyloseq/complete/phyloseq.RDS")
                file.copy(file_loc, con)
            }
        )

        output$json_table <- renderTable(
            {
                jsonDat <- project_obj()$get_project_overview()
                data <- parsed_data()
                data_frame <- data.frame(
                    Key = c("ID", "Owner", "Name", "Run Command (Parameter)", "Run Command (Dataset)", "Status"),
                    Value = c(
                        data$id,
                        data$owner,
                        data$name,
                        data$run_command$parameter,
                        data$run_command$dataset,
                        data$status
                    )
                )
                data_frame
            },
            align = "l",
            width = "100%",
            bordered = TRUE
        )
    })
}
