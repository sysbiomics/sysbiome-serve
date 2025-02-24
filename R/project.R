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


#' Extract and Reorder Taxonomic Columns
#'
#' This function extracts taxonomic columns from a tibble and reorders them according to a predefined order:
#' Kingdom, Phylum, Class, Order, Family, Genus, and Species. It also removes the first column (assumed to be an ID column).
#'
#' @import dplyr microbiome
#' @param taxa_tibble A tibble containing taxonomic data. The first column is assumed to be an ID column
#' and will be removed.
#' @return A tibble containing only the taxonomic columns in the specified order. Column names are
#' preserved in their original case.
#' @examples
#' # Example data
#' data <- tibble::tibble(
#'     FeatureID = c("ASV_8562", "ASV_5425", "ASV_5387"),
#'     kingdom = c("Bacteria", "Bacteria", "Bacteria"),
#'     Phylum = c("Firmicutes", "Firmicutes", "Firmicutes"),
#'     class = c("Clostridia", "Clostridia", "Clostridia"),
#'     Order = c("Lachnospirales", "Lachnospirales", "Lachnospirales"),
#'     family = c("Lachnospiraceae", "Lachnospiraceae", "Lachnospiraceae"),
#'     Genus = c(NA, NA, NA),
#'     Species = c(NA, NA, NA)
#' )
#'
#' # Extract and reorder columns
#' extract_taxa_name(data)
#'
#' @export
extract_taxa_name <- function(taxa_tibble) {
    # Define the desired column order (case-insensitive)
    custom_order <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

    taxa_tibble %>%
        dplyr::select(-1) %>% # Drop the first column by position (becuase it is prob asv/otu)
        dplyr::select(all_of(names(.)[match(custom_order, tolower(names(.)))])) %>% # Match and reorder columns
        names()
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
        ps = NULL,

        #' @description
        #' Create a new ProjectA object
        #' @param folder_path The path to the project folder
        initialize = function(folder_path) {
            self$folder_path <- folder_path
            self$ps <- readRDS(file.path(self$folder_path, "phyloseq/complete/phyloseq.RDS")) %>%
                tax_fix(verbose = FALSE)
                # %>%
                # tax_filter(min_prevalence = 5 / 100, min_total_abundance = 20, verbose = FALSE)
            self$validate()
        },

        #' @description
        #' Validate the presence of required files
        validate = function() {
            required_files <- c(
                "phyloseq/complete/phyloseq.RDS",
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
        get_corelation = function(taxa_level) {

        },
        #' @description
        #' Get taxonomic data
        #' @return A list containing ASV, taxonomy, and metadata
        get_taxa_data = function() {
            ps_dat <- self$ps
            list(ps = ps_dat)
        },
        get_taxa_name = function() {
            ps_dat <- self$get_taxa_data()$ps
            taxa_tib <- tax_tibble(ps_dat)
            extract_taxa_name(taxa_tib)
        },
        get_correlation_data = function(rank = NULL) {
            # Do nothing if rank is null
            if (is.null(rank)) {
                agg_func <- function(ps, ...) {
                    return(ps)
                }
            } else {
                agg_func <- function(ps){ps %>% tax_agg(rank = rank)}
            }
            self$ps %>%
                tax_fix(verbose = FALSE) %>%
                tax_filter(min_prevalence = 5 / 100, tax_level = rank) %>%
                agg_func %>%
                cor_calc(
                    taxa = tax_top(., n = 20),
                    seriation_method = "Identity",
                )
        },
        get_metadata = function() {
            meta_dat <- self$ps %>%
                samdat_tbl(sample_names_col = "ID_sample") %>%
                dplyr::select(1, where(is.character))

            list(meta = meta_dat)
        },
        get_cat_metadata = function() {
            # Unironically, this also get cat data because of its requirement
            max_category_ratio <- 0.3
            max_na_ratio <- 0.5
            n_samples <- phyloseq::nsamples(self$get_taxa_data()$ps)
            meta_table <- self$get_metadata()$meta
            filtered_metadata <- meta_table %>%
                select(where(~ length(unique(.)) / n_samples <= max_category_ratio &&
                    mean(is.na(.)) <= max_na_ratio))

            list(meta = filtered_metadata)
        },
        get_cat_data = function() {
            # Get metadata as chr AND num
            samtbl <- self$ps %>%
                samdat_tbl(sample_names_col = "ID_sample")
            metadata_cat <- samtbl %>%
                dplyr::select(1, where(is.character))
            metadata_num <- samtbl %>%
                dplyr::select(1, where(is.numeric))

            return(
                metadata_num = metadata_num,
                metadata_cat = metadata_cat
            )
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



# ps %>%
#   microViz::ps_mutate(
#     across(everything(), ~ na_if(.x, ""))  # Convert "" to NA
#   ) %>%
#   microViz::ps_mutate(
#     across(everything(), ~ if (all(!is.na(suppressWarnings(as.numeric(.x))))) as.numeric(.x) else .x)) %>% microViz::samdat_tbl()


#
#' Util for correlation calculation
df_to_numeric_matrix <- function(df, vars = NA, trans_fun = NA) {
  if (inherits(df, "matrix")) {
    stopifnot(storage.mode(df) %in% c("double", "integer", "logical"))
    mat <- df
  } else {
    nm <- sapply(df, function(x) is.numeric(x) | is.logical(x) | is.integer(x))
    df <- df[, nm, drop = FALSE]
    mat <- as.matrix.data.frame(df)
  }
  num_vars <- colnames(mat)
  if (length(num_vars) == 0) stop("no numeric/integer/logical variables found")
  if (!identical(vars, NA)) {
    stopifnot(is.character(vars))
    if (all(vars %in% num_vars)) {
      mat <- mat[, vars, drop = FALSE]
    } else {
      stop(
        paste(vars[!vars %in% num_vars], collapse = " "),
        " is/are not valid variable names in the (sample_) data\n",
        "Possible numeric/integer/logical variables include:\n",
        paste(utils::head(x = num_vars, n = 10), collapse = " ")
      )
    }
  }
  # apply transformation function to matrix columns?
  if (!identical(trans_fun, NA)) {
    if (inherits(trans_fun, "function")) {
      mat <- apply(mat, MARGIN = 2, FUN = trans_fun)
    } else if (inherits(trans_fun, "character")) {
      mat <- apply(
        X = mat, MARGIN = 2,
        FUN = function(x) do.call(what = trans_fun, args = list(x))
      )
    } else {
      stop("var transformation must be specified as a function or name of one")
    }
  }
  return(mat)
}

cor_calc <- function(data,
                     taxa = NA,
                     taxon_renamer = identity,
                     vars = NA,
                     var_anno = NULL,
                     cor = "spearman",
                     cor_use = "everything",
                     var_fun = "identity",
                     ...) {
    # check correlation type argument
    cor <- match.arg(cor)

    if (inherits(data, "data.frame")) {
        otu_mat <- NULL # causes cor to only use x (meta_mat)
        meta_mat <- df_to_numeric_matrix(data, vars = vars, trans_fun = var_fun)
    } else if (methods::is(data, "phyloseq")) {
        ps <- ps_get(data)

        # handle sample metadata
        samdat <- phyloseq::sample_data(ps)
        meta_mat <- df_to_numeric_matrix(samdat, vars = vars, trans_fun = var_fun)

        # default taxa names is all taxa names
        if (identical(taxa, NA)) taxa <- phyloseq::taxa_names(ps)

        # handle otu_table data if relevant
        if (identical(taxa, NULL)) {
            otu_mat <- NULL
        } else {
            otu_mat <- unclass(otu_get(data)[, taxa, drop = FALSE])

            # rename taxa
            colnames(otu_mat) <- taxon_renamer(colnames(otu_mat))
        }
    } else {
        stop(
            "data must be phyloseq, psExtra, or data.frame, not: ",
            paste(class(data), collapse = " ")
        )
    }

    # correlate datasets (x to y or just pairs within x if y is NULL)
    cor_mat <- stats::cor(x = meta_mat, y = otu_mat, use = cor_use, method = cor)
    cor_mat
}



# #' @import Hmisc
# #' @importFrom dplyr filter mutate %>%
# #' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_minimal
# #'
# #' @param ps A phyloseq object or a matrix of abundances
# #' @param alpha Significance level for filtering correlations (default = 0.05)
# #' @return A list containing the correlation matrix, p-value matrix, and filtered correlations
# #' @examples
# #' # Example usage
# #' cor_results <- correlation_calc(ps)
# #' correlation_heatmap(cor_results)
# correlation_calc <- function(ps, cor.type = "spearman") {
#     check_is_phyloseq(ps)

#     corr_res <- ps %>%
#         microbiome::transform("compositional") %>%
#         microbiome::abundances() %>%
#         t() %>%
#         Hmisc::rcorr(type = cor.type)

#     # contain r, p, and n
#     corr_res
# }



# #' Reshape the `rcorr` results into a long-format tibble
# #'
# #' @param corr_res The result from the `rcorr` function (a list containing r, p, and n matrices)
# #' @return A long-format tibble containing pairwise correlations, p-values, and sample sizes
# #' @importFrom tidyr pivot_longer
# #' @import dplyr
# #' @examples
# #' # Example usage
# #' corr_res <- rcorr(matrix(rnorm(100), 10, 10), type = "spearman")
# #' long_corr_df <- reshape_corr(corr_res)
# reshape_corr <- function(corr_res) {
#     corr_mat <- corr_res$r
#     p_mat <- corr_res$P
#     n_mat <- corr_res$n

#     # Convert to data frames and add row/column names
#     corr_df <- as.data.frame(corr_mat) %>%
#         rownames_to_column(var = "Var1") %>%
#         pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

#     p_df <- as.data.frame(p_mat) %>%
#         rownames_to_column(var = "Var1") %>%
#         pivot_longer(cols = -Var1, names_to = "Var2", values_to = "p_value")

#     n_df <- as.data.frame(n_mat) %>%
#         rownames_to_column(var = "Var1") %>%
#         pivot_longer(cols = -Var1, names_to = "Var2", values_to = "n")


#     # Join the data frames
#     long_corr_df <- corr_df %>%
#         left_join(p_df, by = c("Var1", "Var2")) %>%
#         left_join(n_df, by = c("Var1", "Var2")) %>%
#         # Remove redundant rows
#         filter(Var1 <= Var2)
#     return(long_corr_df)
# }

# plot_cor <- function(ps, rank = "Family") {
#     alpha <- 0.05
#     N <- 20 # Top N correlation

#     r_correlation <- ps %>%
#         tax_fix() %>%
#         tax_filter(min_prevalence = 5 / 100, tax_level = "Family") %>%
#         tax_agg(rank) %>%
#         correlation_calc()

#     long_corr_df <- r_correlation %>% reshape_corr()

#     # 1. List row/col with at least one p-value < alpha
#     significant_vars <- unique(c(
#         long_corr_df$Var1[long_corr_df$p_value < alpha],
#         long_corr_df$Var2[long_corr_df$p_value < alpha]
#     ))

#     # 2. Filter (retain only variables that are significant with at least one other)
#     filtered_data <- long_corr_df %>%
#         filter((Var1 %in% significant_vars) & (Var2 %in% significant_vars) & (p_value < alpha))

#     # 3. Calculate mean absolute correlation for each variable
#     mean_abs_cor <- filtered_data %>%
#         group_by(Var1) %>%
#         dplyr::summarise(mean_abs_cor1 = mean(abs(Correlation), na.rm = TRUE)) %>%
#         rename(Variable = Var1) %>%
#         bind_rows(
#             filtered_data %>%
#                 group_by(Var2) %>%
#                 dplyr::summarise(mean_abs_cor2 = mean(abs(Correlation), na.rm = TRUE)) %>%
#                 rename(Variable = Var2, mean_abs_cor1 = mean_abs_cor2)
#         ) %>%
#         group_by(Variable) %>%
#         dplyr::summarise(mean_abs_cor = max(mean_abs_cor1)) %>%
#         arrange(desc(mean_abs_cor))

#     # 4. Get Top N variables
#     top_N_vars <- mean_abs_cor$Variable[1:min(N, nrow(mean_abs_cor))]

#     # 5. Filter the original filtered data to include only the top N variables
#     top_N_data <- filtered_data %>%
#         filter((Var1 %in% top_N_vars) & (Var2 %in% top_N_vars))

#     # 3. Plot with ggplot2
#     if (nrow(filtered_data) > 0) {
#         ggplot(data = top_N_data, aes(x = Var1, y = Var2, fill = Correlation)) +
#             geom_tile() +
#             scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
#             theme_minimal() +
#             theme(
#                 axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
#                 axis.title = element_blank(),
#                 axis.text.y = element_blank()
#             ) + # Remove axis labels (this is the new line)
#             coord_fixed() +
#             labs(title = paste("Correlation Heatmap (p <", alpha, ") - Significant Variables Only"))
#     } else {
#         ggplot() +
#             annotate("text",
#                 x = 1, y = 1, label = "Please select a taxonomy level first.",
#                 size = 5, color = "red"
#             ) +
#             xlim(0, 2) + # Set x-axis limits
#             ylim(0, 2) + # Set y-axis limits
#             theme_void()
#     }
# }

# plot_cor(ps, rank = "Order")


#     reshape_corr %>%
#     # filter(p_value < 0.01) %>%
#     arrange(desc(abs(Correlation)))

# dup <- top_point %>%
#   dplyr::select(., Var2, Var1, Correlation, p_value, n) %>%
#   dplyr::rename(Var1 = Var2, Var2 = Var1)

# top_point  %>%  # Keep only the top N strongest correlations
#     bind_rows(., dup) %>%
#     ggplot(aes(x = Var1, y = Var2, fill = Correlation)) +
#         geom_tile() +  # Tile the heatmap
#         scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
#         labs(title = "Correlation Heatmap", x = "Taxa", y = "Taxa", fill = "Correlation") +
#         theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
