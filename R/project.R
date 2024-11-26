#' @export
#' @import forcats
#' @importFrom dplyr mutate
#' @importFrom phyloseq phyloseq otu_table tax_table sample_data
#' @importFrom tidyr replace_na
#' @importFrom ape read.tree
ReadQiime2 <- function(
    asvtab,
    taxtab,
    metatab,
    treenwk = NA,
    seqfasta = NA,
    ...) {
    asvtab.raw <- readr::read_tsv(asvtab, show_col_types = FALSE)
    taxtab_raw <- readr::read_tsv(taxtab, show_col_types = FALSE)
    metatab.raw <- readr::read_tsv(metatab, na = c("", "#N/A"), show_col_types = FALSE)

    pseq.tab <- asvtab.raw %>%
        tibble::column_to_rownames(names(asvtab.raw)[1]) %>%
        as.matrix() %>%
        otu_table(taxa_are_rows = TRUE)
    pseq.tax <- taxtab_raw %>%
        dplyr::mutate(across(everything(), ~ replace_na(.x, ""))) %>%
        tibble::column_to_rownames(names(taxtab_raw)[1]) %>%
        as.matrix() %>%
        tax_table()
    pseq.met <- metatab.raw %>%
        tibble::column_to_rownames(names(metatab.raw)[1]) %>%
        sample_data()

    ess_args <- list(pseq.tab, pseq.tax, pseq.met)
    # Optional
    if (!is.null(seqfasta)) {
        pseq.seq <- Biostrings::readDNAStringSet(seqfasta)
        ess_args <- append(ess_args, list(pseq.seq))
    }
    if (!is.null(treenwk)) {
        pseq.tre <- ape::read.tree(treenwk)
        ess_args <- append(ess_args, list(pseq.tre))
    }

    do.call(phyloseq, ess_args)
}

# Misc
#' @importFrom data.table :=
FastMelt <- function(physeq, includeSampleVars = character()) {
    require("phyloseq")
    require("data.table")
    # Fixed output name
    name.sam <- "ID_sample"
    name.abn <- "abn"
    name.tax <- "TaxaID"

    # Check if data.table has these name.

    # supports "naked" otu_table as `physeq` input.
    otutab <- as(otu_table(physeq), "matrix")
    if (!taxa_are_rows(physeq)) {
        otutab <- t(otutab)
    }
    otudt <- data.table(otutab, keep.rownames = name.tax)
    # Enforce character TaxaID key
    otudt[, (name.tax) := as.character(get(name.tax))]
    # Melt count table
    mdt <- melt.data.table(otudt,
        id.vars = name.tax,
        variable.name = name.sam,
        value.name = name.abn
    )
    # Omit NAs
    # mdt <- mdt[!is.na(abn)]
    if (!is.null(tax_table(physeq, errorIfNULL = FALSE))) {
        # If there is a tax_table, join with it. Otherwise, skip this join.
        taxdt <- data.table(as(tax_table(physeq, errorIfNULL = TRUE), "matrix"), keep.rownames = name.tax)
        taxdt[, (name.tax) := as.character(get(name.tax))]
        # Join with tax table
        setkeyv(taxdt, name.tax)
        setkeyv(mdt, name.tax)
        mdt <- taxdt[mdt]
    }

    # Save taxonomy columns

    wh.svars <- which(sample_variables(physeq) %in% includeSampleVars)
    if (length(wh.svars) > 0) {
        # Only attempt to include sample variables if there is at least one present in object
        sdf <- as(sample_data(physeq), "data.frame")[, wh.svars, drop = FALSE]
        sdt <- data.table(sdf, keep.rownames = name.sam)
        # Join with long table
        setkeyv(sdt, name.sam)
        setkeyv(mdt, name.sam)
        mdt <- sdt[mdt]
    }
    setkeyv(mdt, name.tax)
    return(mdt)
}

# Read folder as tree
tree.list <- function(file.or.dir) {
    isdir <- file.info(file.or.dir)$isdir
    if (!isdir) {
        out <- file.or.dir
    } else {
        files <- list.files(file.or.dir,
            full.names = TRUE,
            include.dirs = TRUE
        )
        out <- lapply(files, tree.list)
        names(out) <- basename(files)
    }
    out
}

# Enum for project status
# PROJECT_STAT <- c("RUNNING", "FINISHED", "FAILED")

#' Check status of the project
# CheckProject <- function(project_id) {
#     incoming <- file.path(BASE_FOLDER, "incoming_data", project_id)
#     finished <- file.path(BASE_FOLDER, "finished", project_id)

#     if (fs::dir_exists(incoming)) {
#         return("RUNNING")
#     } else if (fs::dir_exists(finished)) {
#         return("FINISHED")
#     } else {
#         return("FAILED")
#     }

#     return("FAILED")
# }


# Opt to do everything in biom format.

#' Load folder of microbiome data. The folder must have asv.tab, taxonomy.tsv, metadata.tsv files
#'
#' @return phyloseq object
#' @export
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom phyloseq phyloseq otu_table tax_table sample_data
#' @importFrom tidyr replace_na
#' @importFrom ape read.tree
phyloseq_to_biom <- function(physeq) {
    biom_obj <- make_biom(
        otu_table(physeq),
        sample_metadata = sample_data(physeq),
        observation_metadata = tax_table(physeq)
    )
    biom_obj
}

#' @importFrom biomformat biom_data sample_metadata
biom_to_phyloseq <- function(biom_obj, tax_level = NULL) {
    asv_tab <- biom_data(biom_obj) %>% as.matrix()
    tax_tab <- observation_metadata(biom_obj)
    met_tab <- sample_metadata(biom_obj)

    if (!is.null(tax_level)) {
        # check if it is equal, give a warning if it doesn't
        if (ncol(tax_tab) != length(tax_level)) {
            warning("no")
        }
        names(tax_tab) <- tax_level
    }
}


#' Project Class
#'
#' @description
#' An R6 class to handle project data operations.
#' To use start with my_project <- ProjectA$new("path/to/your/folder")
#'
#' @import R6
#' @importFrom readr read_tsv
#' @importFrom dplyr rename_with
#' @export
Project <- R6::R6Class("Project",
    public = list(
        #' @field folder_path The path to the project folder
        folder_path = NULL,

        #' @description
        #' Create a new ProjectA object
        #' @param folder_path The path to the project folder
        initialize = function(folder_path) {
            self$folder_path <- folder_path
            self$validate()
        },

        #' @description
        #' Validate the presence of required files
        validate = function() {
            required_files <- c(
                "metadata.csv", "phyloseq/complete/phyloseq.RDS",
                "phyloseq/alpha/alpha_metrics_otu.tsv", "phyloseq/beta/pcoa/bray_asv.tsv", "phyloseq/beta/nmds/bray_asv.tsv"
            )
            missing_files <- required_files[!file.exists(file.path(self$folder_path, required_files))]
            if (length(missing_files) > 0) {
                log_error(self$folder_path)
                stop(paste("Missing required files:", paste(missing_files, collapse = ", ")))
            }
        },

        #' @description
        #' Get alpha diversity data
        #' @return A list containing alpha diversity and metadata
        get_alpha_data = function() {
            alpha_dat <- readr::read_tsv(file.path(self$folder_path, "phyloseq", "alpha", "alpha_metrics_otu.tsv"), show_col_types = FALSE) |>
                dplyr::rename_with(.cols = 1, ~"ID_sample")
            meta_dat <- self$get_metadata()$meta
            list(alpha = alpha_dat, meta = meta_dat)
        },

        #' @description
        #' Get beta diversity data
        #' @return A list containing PCoA, NMDS, and metadata
        get_beta_data = function() {
            create_nested_list <- function(directory) {
                # Helper function to split and nest files based on directory structure
                nest_files <- function(files) {
                    split(files, sapply(files, function(file) {
                        # Extract the grouping identifier (e.g., "bray", "jaccard")
                        strsplit(basename(file), "_")[[1]][1]
                    }))
                }

                # Recursively walk through directories and build the nested list
                build_structure <- function(path) {
                    items <- list.files(path, full.names = TRUE)
                    dirs <- items[file.info(items)$isdir]
                    files <- items[!file.info(items)$isdir]

                    structure <- list()
                    if (length(files) > 0) {
                        structure <- nest_files(files)
                    }
                    for (dir in dirs) {
                        structure[[basename(dir)]] <- build_structure(dir)
                    }
                    structure
                }

                build_structure(directory)
            }

            create_nested_list(file.path(self$folder_path, "beta/"))

            pcoa_dat <- readr::read_tsv(file.path(self$folder_path, "phyloseq/beta/pcoa/bray_asv.tsv"), show_col_types = FALSE) |>
                dplyr::rename_with(.cols = 1, ~"ID_sample")
            nmds_dat <- readr::read_tsv(file.path(self$folder_path, "phyloseq/beta/nmds/bray_asv.tsv"), show_col_types = FALSE) |>
                dplyr::rename_with(.cols = 1, ~"ID_sample")
            meta_dat <- self$get_metadata()$meta

            list(pcoa = pcoa_dat, nmds = nmds_dat, meta = meta_dat)
        },

        #' @description
        #' Get taxonomic data
        #' @return A list containing ASV, taxonomy, and metadata
        get_taxa_data = function() {
            meta_dat <- self$get_metadata()$meta
            # ps_dat <- LoadFolder(self$folder_path)

            ps_dat <- readRDS(file.path(self$folder_path, "phyloseq/complete/phyloseq.RDS"))

            list(ps = ps_dat, meta = meta_dat)
        },
        get_metadata = function() {
            meta_dat <- readr::read_csv(file.path(self$folder_path, "metadata.csv"), show_col_types = FALSE) |>
                dplyr::rename_with(.cols = 1, ~"ID_sample")
            list(meta = meta_dat)
        },
        get_filtered_metadata = function() {
            max_category_ratio <- 0.3
            max_na_ratio <- 0.5
            n_samples <- phyloseq::nsamples(self$get_taxa_data()$ps)
            meta_table <- self$get_metadata()$meta
            filtered_metadata <- meta_table %>%
                select(where(~ length(unique(.)) / n_samples <= max_category_ratio &&
                    mean(is.na(.)) <= max_na_ratio))

            list(meta = filtered_metadata)
        },
        get_ecfunc_data = function() {
            ec_dat <- readr::read_tsv(file.path(self$folder_path, "picrust2/EC_pred_metagenome_unstrat_descrip.tsv"), show_col_types = FALSE) |>
                dplyr::rename_with(.cols = 1, ~"EC_ID")
        },
        get_kofunc_data = function() {
            ec_dat <- readr::read_tsv(file.path(self$folder_path, "picrust2/KO_pred_metagenome_unstrat_descrip.tsv"), show_col_types = FALSE) |>
                dplyr::rename_with(.cols = 1, ~"KO_ID")
        },
        get_ko_sankey = function() {
            ko_rawdat <- self$get_kofunc_data()
            fpath <- system.file("ko_mapfiles", "ko00001.tsv.gz", package = "sysmiome.serve")
            ko_map <- read_tsv(fpath)

            ko_datfull <- ko_rawdat %>%
                dplyr::select(-description) %>%
                tidyr::pivot_longer(-c(KO_ID)) %>%
                group_by(KO_ID) %>%
                summarize(gene_count = sum(value)) %>%
                left_join(ko_map, c("KO_ID" = "KO"))

            ko_sankey <- ko_datfull %>%
                # Create connections between level1 and level2
                transmute(source = level1, target = level2, value = gene_count) %>%
                bind_rows(
                    # Add connections between level2 and level3
                    ko_datfull %>% transmute(source = level2, target = level3, value = gene_count)
                )

            # Filtered time. Since the data is too much to display. I would get only top 20 of second and top 30 of the third
            aggregated_data <- ko_datfull %>%
                group_by(level1, level2, level3) %>%
                summarise(total_gene_count = sum(gene_count), .groups = "drop")

            # Step 2: Lump level2 into top 10 categories + "Others"
            aggregated_data <- aggregated_data %>%
                mutate(level2 = forcats::fct_lump_n(level2, n = 15, w = total_gene_count)) %>%
                mutate(level3 = forcats::fct_lump_n(level3, n = 50, w = total_gene_count))

            agg_sankey <- bind_rows(
                aggregated_data %>%
                    group_by(level1, level2) %>%
                    summarise(value = sum(total_gene_count), .groups = "drop") %>%
                    transmute(source = level1, target = as.character(level2), value),
                aggregated_data %>%
                    group_by(level2, level3) %>%
                    summarise(value = sum(total_gene_count), .groups = "drop") %>%
                    transmute(source = as.character(level2), target = as.character(level3), value)
            )

            ko_sankey
        },
        get_ptfunc_data = function() {
            ec_dat <- readr::read_tsv(file.path(self$folder_path, "picrust2/METACYC_path_abun_unstrat_descrip.tsv"), show_col_types = FALSE) |>
                dplyr::rename_with(.cols = 1, ~"METACYC_ID")
        },
        get_project_overview = function() {
            json_location <- file.path(self$folder_path, "overview.json")
            json_content <- paste(readLines(json_location), collapse = "\n")
            json_content
        },
        # get_project_ps = function() {
        #     file.path(self$folder_path, "phyloseq/complete/phyloseq.RDS")
        # },
        get_project_id = function() {
            return(basename(self$folder_path))
        }
    )
)
