#' Building phyloseq from abundance and metadata
#'
#' @param abundance Abundance file with column FeatureID species.... kingdom sample1, sample2, sample3
#' @import phyloseq tibble dplyr readr
#' @noRd
calculate_phyloseq <- function(metadata, abundance) {
    #   options(readr.show_col_types = FALSE)
    abnfile <- read_tsv(abundance)
    metfile <- read_tsv(metadata)

    # Split abundance file into tax and table
    # Instead of making otu, user have to provide their own.
    oabnfile <- abnfile %>%
        rename(OTU = 1)

    # Split the data into two tables
    taxtable <- oabnfile %>%
        dplyr::select(OTU, Kingdom, Phylum, Class, Order, Family, Genus, Species) %>%
        tibble::column_to_rownames(var = "OTU")

    # abntable is after kingdom

    abntable <- oabnfile %>%
        select(OTU, (match("Kingdom", names(oabnfile)) + 1):ncol(oabnfile)) %>%
        tibble::column_to_rownames(var = "OTU")

    metatable <- metfile %>%
        tibble::column_to_rownames(var = colnames(.)[1])

    metatable <- sample_data(metatable)
    taxtable <- as.matrix(taxtable)
    abntable <- as.matrix(abntable)

    ps <- phyloseq(otu_table(abntable, taxa_are_rows = TRUE), tax_table(taxtable), metatable)
    return(ps)
}


#' Building phyloseq from abundance and metadata
#'
#' @import microbiome phyloseq dplyr tidyr readr tibble
#' @noRd
calculate_alpha_diversity <- function(ps) {
    ## Open data
    phy <- ps

    tab <- as(phy@otu_table, "matrix")
    counts <- sample_sums(phy@otu_table)

    calc_alpha <- function(.ps) {
        .ps %>%
            microbiome::alpha(index = "all") %>%
            as_tibble(rownames = "ID_sample") %>%
            mutate(Simpson = -(diversity_gini_simpson - 1)) %>%
            suppressMessages()
    }

    ans <- list()

    ans[["otu"]] <- phy %>%
        calc_alpha()

    ans[["species"]] <- phy %>%
        aggregate_taxa("Species") %>%
        calc_alpha()

    ans[["genus"]] <- phy %>%
        aggregate_taxa("Genus") %>%
        calc_alpha()

    ans[["family"]] <- phy %>%
        aggregate_taxa("Family") %>%
        calc_alpha()

    return(ans)
}

#' Calculate raw beta divesity
#'
#' @noRd
calculate_beta_diversity <- function(.ps) {
    calc_beta(.ps)
}

#' Calculate rank beta divesity
#'
#' @noRd
calculate_beta_diversity_rank <- function(.ps, rank) {
    .ps2 <- aggregate_taxa(.ps, rank)
    calc_beta(.ps2)
}

#' Helper function for calculating beta diversity
#'
#' @import microbiome phyloseq dplyr readr tidyr purrr vegan forcats
#' @noRd
calc_beta <- function(.ps) {
    output <- tibble(
        method = c("bray", "jaccard")
    ) %>%
        rowwise() %>%
        mutate(
            dist = list(
                .ps %>%
                    phyloseq::distance(method = method) %>%
                    as.matrix() %>%
                    as_tibble(rownames = "rn")
            ),
            NMDS_raw = list(
                .ps %>%
                    quietly(ordinate)("NMDS", method)
            ),
            NMDS = list(
                NMDS_raw %>%
                    pluck("result") %>%
                    vegan::scores(display = "sites") %>%
                    as_tibble(rownames = "ID_sample")
            ),
            PCoA_raw = list(
                .ps %>%
                    quietly(ordinate)("PCoA", method)
            ),
            PCoA = list(
                PCoA_raw %>%
                    pluck("result", "vectors") %>%
                    as_tibble(rownames = "ID_sample")
            ),
            PCoA_percentage = list(
                PCoA_raw %>%
                    pluck("result", "values", "Relative_eig") * 100
            )
        )

    output
}
