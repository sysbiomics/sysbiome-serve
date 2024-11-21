#' @import R6 fs purrr
#' @importFrom digest digest
#' @keywords internal
Tower <- R6::R6Class("Tower",
    public = list(
        #
        #' Base folder for storing project data
        #'
        #' @field base_folder A character string specifying the base folder path.
        base_folder = NULL,
        #' Initialize the Tower object
        #'
        #' @param base_folder A character string specifying the base folder path. Defaults to "/sysmiome".
        initialize = function(base_folder = "/sysmiome") {
            self$base_folder <- base_folder
        },
        #'
        #' @param project_id A project_id.
        #' @return A...
        project_search = function(query = NULL) {
            projects_folder <- file.path(self$base_folder, "public_data")
            all_projects <- list.dirs(projects_folder, full.names = FALSE, recursive = FALSE)

            if (!is.null(query) && nchar(query) > 0) {
                matching_projects <- grep(query, all_projects, value = TRUE, ignore.case = TRUE)
                return(matching_projects)
            } else {
                return(all_projects)
            }
        },
        #'
        #' Get project detail
        #'
        #' @param project_id A project_id.
        #' @return A
        get_project_details = function(project_id) {
            project_folder <- file.path(self$base_folder, "public_data", project_id)
            if (!dir.exists(project_folder)) {
                return(NULL)
            }

            # Here you can add more details about the project if needed
            list(
                id = project_id,
                path = project_folder
            )
        },
        #'
        #' Get project object
        #'
        #' @param project_id A project ID.
        #' @return A project object initialized with the project path.
        get_project = function(project_id) {
            # Assume that it is all in public for now.
            project_path <- file.path(self$base_folder, "public_data", project_id)
            project_obj <- Project$new(project_path)
            return(project_obj)
        },
        #' Get all projects id available
        #'
        #' @return A.
        get_projects = function() {

        },
        #'
        #' Get mock projects
        #'
        #' @param n The number of mock projects to generate.
        #' @return A data frame of mock projects.
        get_projects_mock = function(n = 10) {
            collect <- list()
            # For loop to run n times
            for (i in 1:n) {
                collect[[i]] <- generate_sample()
            }

            collect %>%
                purrr::map_dfr(~.x)
        },
        #'
        #' Create a new project
        #'
        #' @param project_name The name of the project.
        #' @param submitter_email The email of the submitter.
        #' @param metadata_file A metadata file.
        #' @param fastq_files A list of FASTQ files.
        #' @param pipeline The pipeline to use.
        create_new_project = function(project_name, submitter_email, metadata_file, fastq_files, pipeline) {
            # Generate project ID
            random_str <- paste(sample(c(0:9, letters, LETTERS), 20, replace = TRUE), collapse = "")
            sha256_hash <- digest::digest(random_str, algo = "sha256", serialize = FALSE)
            project_id <- paste0(project_name, "_", substr(sha256_hash, 1, 16))

            # Create project folder
            project_folder <- file.path(self$base_folder, "incoming_data", project_id)
            fs::dir_create(project_folder, recurse = TRUE, mode = "644")

            # Save metadata
            target_metadata <- file.path(project_folder, "metadata.csv")
            fs::file_copy(metadata_file$datapath, target_metadata)
            fs::file_chmod(target_metadata, mode = "0660")

            # Save project info
            project_info <- list(
                project_name = project_name,
                submitter_email = submitter_email,
                pipeline = pipeline
            )
            writeLines(jsonlite::toJSON(project_info, auto_unbox = TRUE), file.path(project_folder, "project_info.json"))

            # Copy fastq files
            for (i in seq_along(fastq_files$datapath)) {
                src_path <- fastq_files$datapath[i]
                tgt_path <- file.path(project_folder, fastq_files$name[i])
                fs::file_move(src_path, tgt_path)
                fs::file_chmod(tgt_path, mode = "0660")
            }

            return(project_id)
        }
    )
)


NAMES <- c("John", "Mary", "Peter", "Paul", "Jane", "Anne", "Lucy", "Mark", "Lisa", "Kate")
WORKFLOWS <- c("16s", "wgs", "ITS")
STATUS <- c("SUBMITTED", "RUNNING", "SUCCEEDED", "FAILED", "CANCELLED")

generate_sample <- function(seed = NA) {
    if (!is.na(seed)) set.seed(seed)

    list(
        launchId = digest::digest(as.character(Sys.time()), algo = "md5", serialize = FALSE),
        userName = sample(NAMES, 1, replace = T),
        status = sample(STATUS, 1, replace = T),
        start = generate_time() # Random number from 0 to 20,
    )
}

# A function that generate a random time from 50 seconds to 2 hours.
# Generate time in a format of Feb 20, 2021 12:00:00
generate_time <- function() {
    a_time <- sample(c(50:7200), 1)
    format(Sys.time() - a_time, "%b %d, %Y %H:%M:%S")
}
