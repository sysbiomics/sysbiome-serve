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

        #' List of project names
        #'
        #' @field num_projects ReactiveVal
        num_projects = NULL,

        #' Initialize the Tower object
        #'
        #' @param base_folder A character string specifying the base folder path. Defaults to "/sysmiome".
        initialize = function(base_folder = "/sysmiome") {
            self$base_folder <- base_folder
            self$num_projects <- reactiveVal(length(self$project_search()))
        },

        #'
        #' Search 
        #'
        #' @param project_id A project_id.
        #' @return A list of projects
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
        #' Create a new project from fastq
        #'
        #' @param project_name The name of the project.
        #' @param submitter_email The email of the submitter.
        #' @param metadata_file A metadata file.
        #' @param fastq_files A list of FASTQ files.
        #' @param pipeline The pipeline to use.
        # create_new_project_fq = function(project_name, submitter_email, metadata_file, fastq_files, pipeline) {
        #     # Generate project ID
        #     cproject_name <- gsub(" ", "_", project_name)
        #     random_str <- paste(sample(c(0:9, letters, LETTERS), 20, replace = TRUE), collapse = "")
        #     sha256_hash <- digest::digest(random_str, algo = "sha256", serialize = FALSE)
        #     # project_id <- paste0(submitter_email, "_", cproject_name, "_", substr(sha256_hash, 1, 6))
        #     project_id <- paste0(cproject_name, "_", substr(sha256_hash, 1, 6))

        #     # Create project folder
        #     project_folder <- file.path(self$base_folder, "public_data", project_id)
        #     fs::dir_create(project_folder, recurse = TRUE, mode = "644")

        #     # Save metadata
        #     target_metadata <- file.path(project_folder, "metadata.csv")
        #     fs::file_copy(metadata_file$datapath, target_metadata)
        #     fs::file_chmod(target_metadata, mode = "0660")

        #     # Save project info
        #     project_info <- list(
        #         project_name = project_name,
        #         submitter_email = submitter_email,
        #         pipeline = pipeline
        #     )

        #     writeLines(jsonlite::toJSON(project_info, auto_unbox = TRUE), file.path(project_folder, "project_info.json"))

        #     # Copy fastq files
        #     for (i in seq_along(fastq_files$datapath)) {
        #         src_path <- fastq_files$datapath[i]
        #         tgt_path <- file.path(project_folder, fastq_files$name[i])
        #         fs::file_move(src_path, tgt_path)
        #         fs::file_chmod(tgt_path, mode = "0660")
        #     }

        #     return(project_id)
        # },
        #'
        #' Create a new project from abundance file
        #'
        #' @import readr dplyr
        #' @param project_name The name of the project.
        #' @param submitter_email The email of the submitter.
        #' @param metadata_file A metadata file.
        #' @param fastq_files A list of FASTQ files.
        #' @param pipeline The pipeline to use.
        create_new_project_abundance = function(project_name, submitter_email, metadata_file, abundance_file) {

            cproject_name <- gsub(" ", "_", project_name)
            random_str <- paste(sample(c(0:9, letters, LETTERS), 20, replace = TRUE), collapse = "")
            sha256_hash <- digest::digest(random_str, algo = "sha256", serialize = FALSE)
            # project_id <- paste0(submitter_email, "_", cproject_name, "_", substr(sha256_hash, 1, 6))
            project_id <- paste0(cproject_name, "_", substr(sha256_hash, 1, 6))

            # Create project folder
            dir_mode = "755"
            fil_mode = "644"

            # Create project folder
            project_folder <- file.path(self$base_folder, "public_data", project_id)
            phyloseq_folder <- file.path(project_folder, "phyloseq", "complete")
            alpha_folder <- file.path(project_folder, "phyloseq", "alpha")
            beta_folder <- file.path(project_folder, "phyloseq", "beta")

            fs::dir_create(project_folder, recurse = TRUE, mode = dir_mode)
            fs::dir_create(phyloseq_folder, recurse = TRUE, mode = dir_mode)
            fs::dir_create(alpha_folder, recurse = TRUE, mode = dir_mode)
            fs::dir_create(beta_folder, recurse = TRUE, mode = dir_mode)
            fs::dir_create(file.path(beta_folder, "nmds"), recurse = TRUE, mode = dir_mode)
            fs::dir_create(file.path(beta_folder, "pcoa"), recurse = TRUE, mode = dir_mode)

            ps <- calculate_phyloseq(metadata_file, abundance_file)
            a_div <- calculate_alpha_diversity(ps)
            b_div_asv <- calculate_beta_diversity(ps)
            b_div_fam <- calculate_beta_diversity_rank(ps, "Family")

            # Create overview.json
            overview <- list(
                id = project_id,
                owner = submitter_email,
                name = project_name,
                run_command = list(
                    dataset = "Imported from abundance table"
                ),
                status = "Finished"
            )

            json <- toJSON(overview, pretty = TRUE)
            write(json, file = file.path(project_folder, "overview.json"))

            # Save rds file
            saveRDS(ps, file = file.path(phyloseq_folder, "phyloseq.RDS"))
            saveRDS(a_div, file = file.path(alpha_folder, "alpha_asv.RDS"))
            saveRDS(b_div_asv, file = file.path(beta_folder, "beta_asv.RDS"))

            # Write metadata.csv for compatibility
            metadata_file %>%
              read_tsv() %>%
              write_csv(file.path(project_folder, "metadata.csv"))

            # Save alpha diversity file
            readr::write_tsv(a_div[["otu"]], file.path(alpha_folder, "alpha_metrics_otu.tsv"))
            readr::write_tsv(a_div[["species"]], file.path(alpha_folder, "alpha_metrics_sp.tsv"))
            readr::write_tsv(a_div[["genus"]], file.path(alpha_folder, "alpha_metrics_gen.tsv"))
            readr::write_tsv(a_div[["family"]], file.path(alpha_folder, "alpha_metrics_fam.tsv"))

            # Save beta diversity file
            # 1 is bray, 2 is jaccard
            readr::write_tsv(b_div_asv$PCoA[[1]], file.path(beta_folder, "pcoa", "bray_asv.tsv"))
            readr::write_tsv(b_div_asv$NMDS[[1]], file.path(beta_folder, "nmds", "bray_asv.tsv"))
            readr::write_tsv(b_div_asv$PCoA[[2]], file.path(beta_folder, "pcoa", "jaccard_asv.tsv"))
            readr::write_tsv(b_div_asv$NMDS[[2]], file.path(beta_folder, "nmds", "jaccard_asv.tsv"))

            # Update value
            self$num_projects(self$num_projects() + 1)

            return(project_id)
        }
    )
)
