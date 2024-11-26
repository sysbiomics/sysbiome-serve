pipeline_list <- c(
  "16S rRNA" = "sysbiomics/minbreeze",
  "ITS" = "sysbiomics/itsbreeze"
)

ui_new_project <- function(id = "ID_NEW_MODULE") {
  ns <- NS(id)

  # collapse_compo(
  #   list(header = "Upload fastq", content = h1("Content 1")),
  #   list(header = "Upload fastq", content = h1("Content 2")),
  #   list(header = "Upload fastq", content = h1("Content 3") )
  # )

# ui <- fluidPage(
#     collapse_compo(
#         list(header = "Box 1 Header", content = "Content of Box 1"),
#         list(header = "Box 2 Header", content = "Content of Box 2"),
#         list(header = "Box 3 Header", content = "Content of Box 3")
#     )

  sidebarLayout(
    sidebarPanel(
      fluidPage(
        textInput(
          inputId = ns("project_name"),
          label = "Project name",
          placeholder = "My project"
        ),
        textInput(
          inputId = ns("submitter_email"),
          label = "Email",
          placeholder = "YourEmail@gmail.com"
        ),
        fileInput(
          inputId = ns("file_metadata"),
          label = "Project metadata",
          accept = c(
            ".csv",
            ".tsv"
          )
        ),
        fileInput(ns("files_fastq"),
          "Raw sequences (FASTQ)",
          multiple = TRUE,
          accept = c(
            ".fq.gz",
            ".fastq.gz",
            "text/gzip",
            "application/zip",
            "application/x-zip-compressed",
            "application/x-compressed",
            "multipart/x-zip"
          )
        ),
        fluidRow(
          radioButtons(
            ns("pipeline"), "Data type:",
            c(
              "16S rRNA" = "bact",
              "ITS" = "its"
            )
          ),
        ),
        fluidRow(
          actionButton(
            inputId = ns("submit"),
            label = "Submit"
          ),
          actionButton(
            inputId = ns("project_sample"),
            label = "Example inputs"
          ),
          actionButton(
            inputId = ns("submit"),
            label = "Debug"
          ),
        )
      )
    ),
    mainPanel(
      h1("Metadata:"),
      tableOutput(ns("summary_metadata")),
      h1("Input files:"),
      tableOutput(ns("summary_fastq"))
      # DT::DTOutput(ns("summary_fastq"))
    )
  )
}

# Making sure that metadata and fastq files are valid
# These two inputs are uploaded from shiny so it has
# name size type datapath column where
# name = original file name
# size = file size
# type = file type
# datapath = temporary file location
validate_input <- function(filemeta, filesfastqs) {
  log_debug("Validating metadata file")
}

sv_new_project <- function(id = "ID_NEW_MODULE", tower) {
  moduleServer(
    id,
    function(input, output, session) {
      # Show metadata in table
      observeEvent(input$file_metadata, {
        log_info("Metadata file uploaded")
        output$summary_metadata <- renderTable({
          readr::read_csv(input$file_metadata$datapath, show_col_types = FALSE)
        })
      })

      # Log upload file and show their stat in table
      observeEvent(input$files_fastq, {
        log_info("Fastq files uploaded")
        output$summary_fastq <- renderTable({
          input$files_fastq
        })
      })

      # Observe submit button
      observeEvent(input$submit, {
        log_info("Submit button pressed")

        #
        # Validate inputs
        #

        # input exists
        if (is.null(input$file_metadata)) {
          log_error("No metadata file upload")
          showModal(modalDialog(
            title = "Error: no metadata file",
            p("Please upload metadata file before submission"),
            easyClose = TRUE,
            size = "l"
          ))
          return()
        }

        if (is.null(input$files_fastq)) {
          log_error("No fastq files selected")
          showModal(modalDialog(
            title = "Error: no fastq file",
            p("Please upload fastq file before submission"),
            easyClose = TRUE,
            size = "l"
          ))
          return()
        }

        # Validate metadata format
        input$file_metadata


        # metadata is in concordant with upload data

        validate(
          need(grepl("^[a-zA-Z0-9 ()]+$", input$project_name), "Invalid project name")
        )

        # Create new project using Tower
        tryCatch(
          {
            project_id <- tower$create_new_project(
              project_name = input$project_name,
              submitter_email = input$submitter_email,
              metadata_file = input$file_metadata,
              fastq_files = input$files_fastq,
              pipeline = input$pipeline
            )

            # Notify user that it is uploaded
            url <- glue::glue("https://sysmiome/project/{project_id}")
            msg <- glue::glue("Your project has been uploaded. You can access it at: {url}")

            showModal(modalDialog(
              title = "Uploaded",
              msg,
              size = "l"
            ))
          },
          error = function(e) {
            log_error(paste("Error creating project:", e$message))
            showModal(modalDialog(
              title = "Error",
              paste("An error occurred while creating the project:", e$message),
              easyClose = TRUE,
              size = "l"
            ))
          }
        )
      })


      # # Observe submit button
      # observeEvent(input$submit, {
      #   # Validate all inputs
      #   log_info("Submit button pressed}")

      #   # If no metadata uploaded
      #   if (is.null(input$file_metadata)) {
      #     log_error("No metadata file upload")
      #     showModal(modalDialog(
      #       title = "Error: no metadata file",
      #       p("Please upload metadata file before submission"),
      #       easyClose = TRUE,
      #       size = "l"
      #     ))
      #     return()
      #   }

      #   if (is.null(input$files_fastq)) {
      #     log_error("No fastq files selected")
      #     showModal(modalDialog(
      #       title = "Error: no fastq file",
      #       p("Please upload fastq file before submission"),
      #       easyClose = TRUE,
      #       size = "l"
      #     ))
      #     return()
      #   }

      #   project_name <- input$project_name
      #   submitter_email <- input$submitter_email
      #   pipeline <- input$pipeline

      #   # validate input
      #   validate(
      #     # Check if project name doesn' have ","
      #     need(grepl("^[a-zA-Z0-9 ()]+$", project_name))
      #   )

      #   # Validate data
      #   validate_input(input$file_metadata, input$fastq_files)

      #   # Create new projects
      #   log_info("Creating new project")
      #   random_str <- paste(sample(c(0:9, letters, LETTERS), 20, replace = TRUE), collapse = "")
      #   sha256_hash <- digest::digest(random_str, algo = "sha256", serialize = FALSE)
      #   # Add project name together with hash (only first 16 characters)
      #   project_id <- paste0(project_name, "_", substr(sha256_hash, 1, 16))
      #   project_folder <- file.path(base_folder, "/incoming_data", project_id)

      #   log_info(glue("Create project folder in: {project_folder}"))
      #   fs::dir_create(project_folder, recurse = TRUE, mode = "u=rwx,go=rwx")
      #   # Check mode of created folder and print into log
      #   log_info(glue("Create project folder in: {project_folder}"))
      #   # Save metadata
      #   target_metadata <- file.path(project_folder, "metadata.csv")
      #   fs::file_copy(input$file_metadata$datapath, target_metadata)
      #   fs::file_chmod(target_metadata, mode = 0660)
      #   # Write name and email
      #   project_json <- jsonlite::toJSON(
      #     list(
      #       project_name = project_name,
      #       submitter_email = submitter_email
      #     ),
      #     auto_unbox = TRUE
      #   )
      #   # Copy fastq file
      #   for (i in seq_along(input$files_fastq$datapath)) {
      #     srcPath <- input$files_fastq[[i, "datapath"]]
      #     log_debug(glue("Src: {srcPath}"))
      #     tgtPath <- file.path(project_folder, input$files_fastq[[i, "name"]])
      #     fs::file_move(srcPath, tgtPath)
      #     fs::file_chmod(tgtPath, mode = 0660)
      #   }
      #   # Send email to submitter

      #   # Notify user that it is uploaded
      #   url <- glue::glue("https://sysmiome/project/{project_id}")
      #   msg <- glue::glue("your project has been uploaded. You can access it at: {url}")

      #   showModal(modalDialog(
      #     title = "Uploaded",
      #     msg,
      #     size = "l"
      #   ))

      #   # Run vsearch
      #   # Open monitor pages

      # })

      observeEvent(input$project_sample, {
        log_info("Project sample button pressed")
        # Create project using sample data
        showModal(
          modalDialog(
            title = "Project sample",
            "A sample project",
            size = "l"
          )
        )
      })
    }
  )
}
