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
        div(
            tableOutput(ns("json_table")),
            style = "font-size:150%"
        ),
        downloadButton(
            outputId = ns("download_data"),
            label = "Download data"
        ),
    )
}

#' @import jsonlite
#' @noRd
sv_overview <- function(id = "ID_OVERVIEW_VIEWER", project_obj_reactive) {
    moduleServer(id, function(input, output, session) {
        parsed_data <- reactive({
            jsondat <- jsonlite::fromJSON(project_obj_reactive()$get_project_overview())
            # Add ASV, number of samples, and metadata.
            ps_list <- project_obj_reactive()$get_taxa_data()
            ps_dat <- ps_list$ps
            metadata <- ps_list$meta
            jsondat$`Samples (n)` <- nsamples(ps_dat)
            jsondat$`Metadata (n)` <- ncol(metadata)

            jsondat
        }) |> bindEvent(project_obj_reactive())

        output$download_data <- downloadHandler(
            filename = function() {
                paste0(project_obj_reactive()$get_project_id(), ".zip")
            },
            content = function(file) {
                # Create a temporary directory
                temp_dir <- tempdir()
                zip_file <- file.path(temp_dir, "temp.zip")

                # Get the project directory
                proj_loc <- project_obj_reactive()$folder_path

                # Create zip file using R's built-in zip function
                withCallingHandlers(
                    {
                        zip::zip(
                            zipfile = zip_file,
                            files = list.files(proj_loc, recursive = TRUE, full.names = TRUE),
                            root = proj_loc
                        )

                        # Copy the zip file to the download location
                        file.copy(zip_file, file)

                        # Clean up
                        unlink(zip_file)
                    },
                    error = function(e) {
                        # Log error and provide feedback
                        message("Error creating zip file: ", e$message)
                        stop(e)
                    }
                )
            },
            contentType = "application/zip"
        )

        output$json_table <- renderTable(
            {
                data <- parsed_data()

                if (length(data) == 0) {
                    return(data.frame(Name = integer(0), Value = character(0)))
                }

                flattened <- unlist(data, recursive = TRUE)

                # Convert to data frame
                df <- data.frame(
                    Name = names(flattened),
                    Value = as.character(flattened),
                    stringsAsFactors = FALSE
                )

                df
            },
            align = "l",
            width = "100%",
            bordered = TRUE
        )
    })
}
