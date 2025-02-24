

ui_abundance_upload <- function(id = "ID_ABUNDANCE_MODULE") {
    ns <- NS(id)

    sidebarLayout(
        sidebarPanel(
            fluidPage(
                textInput(
                    inputId = ns("project_name"),
                    label = "Project name",
                    placeholder = "e.g., Gut Microbiome Study 2025"
                ),
                textInput(
                    inputId = ns("project_email"),
                    label = "Email",
                    placeholder = "e.g., researcher.name@example.com"
                ),
                fileInput(
                    inputId = ns("file_metadata"),
                    label = "Project metadata",
                    accept = c(
                        ".csv",
                        ".tsv"
                    )
                ),
                fileInput(ns("file_abundance"),
                    "OTU/ASV table ( abundance )",
                    accept = c(
                        ".csv",
                        ".tsv"
                    )
                ),
                fluidRow(
                    actionButton(
                        inputId = ns("abundance_submission"),
                        label = "Submit"
                    ),
                    downloadButton(
                        outputId = ns("metadata_example"),
                        label = "Example metadata file"
                    ),
                    downloadButton(
                        outputId = ns("abundance_example"),
                        label = "Example abundance file"
                    ),
                )
            )
        ),
        mainPanel(
            h1("Metadata:"),
            tableOutput(ns("summary_metadata")),
            h1("Input files:"),
            tableOutput(ns("summary_fastq"))
        )
    )
}

gen_abundance <- function(){
    tibble::tribble(
        ~OTU,     ~Species,                        ~Genus,               ~Family,             ~Order,              ~Class,              ~Phylum,         ~Kingdom,   ~s1, ~s2, ~s3, ~s4, ~s5, ~s6, ~s7, ~s8, ~s9, ~s10, ~s11, ~s12, ~s13, ~s14, ~s15, ~s16, ~s17, ~s18, ~s19, ~s20, ~s21, ~s22, ~s23, ~s24, ~s25, ~s26, ~s27, ~s28, ~s29, ~s30,
        "OTU1",   "Enterococcus columbae",         "Enterococcus",       "Enterococcaceae",   "Lactobacillales",   "Bacilli",           "Firmicutes",    "Bacteria",   1,   0,   5,   2,   8,   5,   4,   9,   6,   10,    2,    3,    8,    5,    6,    4,    3,    1,    7,    9,    5,    4,    6,    3,    5,    2,    8,    6,    9,    7,
        "OTU2",   "Mycobacterium saskatchewanense", "Mycobacterium",     "Mycobacteriaceae",  "Corynebacteriales", "Actinobacteria",    "Actinobacteria","Bacteria",   0,   1,   7,   9,   2,   6,   1,   1,   2,    4,    5,    2,    4,    7,    6,    8,    2,    3,    8,    9,    5,    3,    7,    2,    6,    4,    3,    2,    5,    4,
        "OTU3",   "Salmonella enterica",           "Salmonella",         "Enterobacteriaceae","Enterobacterales",  "Gammaproteobacteria","Proteobacteria","Bacteria",   1,   0,   6,   10,  1,   1,   3,   1,   1,    2,    7,    8,    4,    9,    3,    6,    4,    2,    1,    5,    8,    9,    6,    4,    7,    1,    6,    9,    8,    4,
        "OTU4",   "Sphingobacterium puteale",      "Sphingobacterium",   "Sphingobacteriaceae","Sphingobacteriales","Sphingobacteriia", "Bacteroidetes", "Bacteria",   0,   2,   1,   5,   3,   6,   3,   7,   3,   10,   1,    3,    5,    8,    7,    9,    2,    4,    3,    5,    6,    7,    9,    8,    2,    5,    6,    3,    7,    1,
        "OTU5",   "Staphylococcus aureus",         "Staphylococcus",     "Staphylococcaceae", "Bacillales",        "Bacilli",           "Firmicutes",    "Bacteria",   1,   0,   4,   7,   10,  8,   1,   6,   1,    3,    2,    5,    4,    7,    3,    8,    5,    9,    6,    2,    7,    5,    6,    9,    4,    6,    8,    9,    5,    3,
        "OTU6",   "Streptococcus sobrinus",        "Streptococcus",      "Streptococcaceae",  "Lactobacillales",   "Bacilli",           "Firmicutes",    "Bacteria",   0,   1,   2,   1,   4,   1,   9,   3,   4,    9,    4,    5,    3,    6,    7,    5,    9,    2,    3,    8,    9,    4,    7,    3,    6,    8,    4,    9,    3,    6
    )
}

#' @import tibble
#' @noRd
gen_metadata <- function(){
    tibble::tibble(
        sample_id = paste0("s", 1:30),
        date = rep(c("0day", "10days"), each = 15),
        treatment = rep(c("control", "untreat"), length.out = 30),
        location = rep(c("v1", "v2", "v3"), length.out = 30),
        numeric_col1 = sample(0:20, 30, replace = TRUE),
        numeric_col2 = sample(0:20, 30, replace = TRUE))
}

#' @import readr
#' @noRd
sv_abundance_upload <- function(id = "ID_ABUNDANCE_MODULE", tower) {
    moduleServer(
        id,
        function(input, output, session) {
            observeEvent(input$file_metadata, {
                log_info("Metadata file uploaded")
            })

            # Log upload file and show their stat in table
            observeEvent(input$file_abundance, {
                log_info("Abundance files uploaded")
            })

            output$metadata_example <- downloadHandler(
                filename = function() {
                    "metadata.tsv"
                },
                content = function(file) {
                    gen_metadata() %>% readr::write_tsv(file)
                }
            )

            output$abundance_example <- downloadHandler(
                filename = function() {
                    "abundance.tsv"
                },
                content = function(file) {
                    gen_abundance() %>% readr::write_tsv(file)
                }
            )

            # Observe submit button
            observeEvent(input$abundance_submission, {
                log_info("Abundance subumission")

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

                if (is.null(input$file_abundance)) {
                    log_error("No abundance files upload")
                    showModal(modalDialog(
                        title = "Error: no abundance file",
                        p("Please upload abundance file before submission"),
                        easyClose = TRUE,
                        size = "l"
                    ))
                    return()
                }

                # Since we already checked that these file are available
                # Validation
                log_info("Validating abundance input")

                abnfile <- input$file_abundance$datapath
                metafile <- input$file_metadata$datapath

                showPageSpinner(caption = (div(strong("Uploading project"), br(), em("Please wait"))))

                # Processing
                project_id <- tryCatch(
                    {
                        # Attempt to create the phyloseq object
                        tower$create_new_project_abundance(input$project_name, input$project_email, metafile, abnfile)
                    },
                    error = function(e) {
                        errorMess <- glue::glue("{e$message}")
                        log_error(errorMess)
                        showNotification("Error", type = "message")
                        return()
                    }
                )

                hidePageSpinner()

                finalMessg <- glue::glue("Results are saved in {project_id}")
                # Tell user what is the project name.
                showModal(modalDialog(
                    title = "Finished",
                        p(finalMessg),
                        easyClose = TRUE,
                        size = "l"
                    ))
            })
        }
    )
}

ui_new_project <- function(id = "ID_NEW_MODULE") {
    ns <- NS(id)

    ui_abundance_upload(ns("upload_abundance"))


    # compo_abundance <- tagList(
    #     ui_abundance_upload(ns("upload_abundance"))
    # )

    # compo_sysmiome <- tagList(
    #     ui_pipeline_upload(ns("upload_pipeline"))
    # )

    # compo_qiime2 <- tagList(
    #     ui_qiime2_upload(ns("upload_qiime2"))
    # )

    # collapse_compo(
    #     list(header = "1. Upload abundance", content = compo_abundance),
    #     list(header = "2. Upload SYSMIOME pipeline's", content = compo_sysmiome),
    #     list(header = "3. Upload QIIME2 pipeline's", content = compo_qiime2)
    # )
}

sv_new_project <- function(id = "ID_NEW_MODULE", tower) {
    moduleServer(
        id,
        function(input, output, session) {
            sv_abundance_upload("upload_abundance", tower = tower)
            # sv_pipeline_upload("upload_pipeline", tower = tower)
            # sv_qiime2_upload("upload_qiime2", tower = tower)
        }
    )
}


# Making sure that metadata and fastq files are valid
# These two inputs are uploaded from shiny so it has
# name size type datapath column where
# name = original file name
# size = file size
# type = file type
# datapath = temporary file location


# ui_pipeline_upload <- function(id = "ID_NEWPIPE_MODULE") {
#     ns <- NS(id)

#     tagList(
#         fileInput(ns("upload_sysmiome"), "Upload a file"),
#         fluidRow(
#             actionButton(
#                 inputId = ns("submit"),
#                 label = "Submit"
#             ),
#             actionButton(
#                 inputId = ns("project_sample"),
#                 label = "Example inputs"
#             )
#         )
#     )
# }

# sv_pipeline_upload <- function(id = "ID_NEWPIPE_MODULE", tower) {
#     moduleServer(
#         id,
#         function(input, output, session) {
#             ns <- session$ns

#             # Reactive value to store the uploaded file path
#             uploaded_file <- reactiveVal(NULL)

#             # Observe the file input
#             observeEvent(input$upload_sysmiome, {
#                 file <- input$upload_sysmiome
#                 if (is.null(file)) {
#                     showNotification("No file uploaded.", type = "error")
#                     return()
#                 }

#                 # Check if the file is a zip file
#                 if (tolower(tools::file_ext(file$name)) != "zip") {
#                     showNotification("Please upload a zip file.", type = "error")
#                     uploaded_file(NULL)
#                     return()
#                 }

#                 # Store the file path
#                 uploaded_file(file$datapath)
#                 showNotification("File uploaded successfully!", type = "message")
#             })

#             # Handle the submit button
#             observeEvent(input$submit, {
#                 if (is.null(uploaded_file())) {
#                     showNotification("No file uploaded. Please upload a zip file first.", type = "error")
#                     return()
#                 }

#                 # Check if abundance and metadata both has the same number of
#             })

#             # Handle the example inputs button
#             observeEvent(input$project_sample, {
#                 # Placeholder: Populate example inputs
#                 showNotification("Example inputs loaded (placeholder).", type = "message")
#                 # Add logic to display or set example data here
#             })

#             # Handle the debug button
#             observeEvent(input$debug, {
#                 # Debugging info
#                 showNotification("Debugging activated (placeholder).", type = "warning")
#                 # Add debug-specific logic or outputs here
#             })
#         }
#     )
# }


# ui_qiime2_upload <- function(id = "ID_NEWQIIME2_MODULE") {
#     ns <- NS(id)

#     tagList(
#         fileInput(ns("upload_qiime2"), "Upload a file"),
#         fluidRow(
#             actionButton(
#                 inputId = ns("submit_qiime2"),
#                 label = "Submit"
#             ),
#             actionButton(
#                 inputId = ns("project_sample"),
#                 label = "Example inputs"
#             ),
#             actionButton(
#                 inputId = ns("debug"),
#                 label = "Debug"
#             ),
#         )
#     )
# }

# sv_qiime2_upload <- function(id = "ID_NEWQIIME2_MODULE", tower) {
#     moduleServer(
#         id,
#         function(input, output, session) {
#         }
#     )
# }
validate_fastq_input <- function(filemeta, filesfastqs) {
    log_debug("Validating metadata file")
}


validate_qiime2_input <- function(filemeta, filesfastqs) {
    log_debug("Validating qiime2 file")
}

pipeline_list <- c(
    "16S rRNA" = "sysbiomics/minbreeze",
    "ITS" = "sysbiomics/itsbreeze"
)
