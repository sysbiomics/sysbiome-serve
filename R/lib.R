library(tidyverse)
library(phyloseq)

# Metadata can be
ReadQiime2 <- function(asvtab, taxtab, seqfst, treenwk, metatab) {
  require(phyloseq)
  require(Biostrings)
  require(ape)
  asvtab.raw <- read_tsv(asvtab, show_col_types = FALSE)
  pseq.tab <- asvtab.raw %>%
    column_to_rownames(names(asvtab.raw)[1]) %>%
    as.matrix() %>%
    otu_table(taxa_are_rows = TRUE)
  taxtab_raw <- read_tsv(taxtab, show_col_types = FALSE)
  pseq.tax <- taxtab_raw %>%
    mutate(across(everything(), ~ replace_na(.x, ""))) %>%
    column_to_rownames(names(taxtab_raw)[1]) %>%
    as.matrix() %>%
    tax_table()
  metatab.raw <- read_tsv(metatab,
    na = c("", "#N/A"),
    show_col_types = FALSE
  )

  pseq.met <- metatab.raw %>%
    column_to_rownames(names(metatab.raw)[1]) %>%
    sample_data()

  pseq.seq <- Biostrings::readDNAStringSet(seqfst)
  pseq.tre <- ape::read.tree(treenwk)

  do.call(phyloseq, list(pseq.tab, pseq.tax, pseq.seq, pseq.tre, pseq.met))
  # phyloseq(pseq.tab, pseq.tax, pseq.seq, pseq.tre, pseq.met)
}

#' @export
#' @import forcats readr microViz
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


# Read data into TSE
ReadToExp <- function(){
  # Summarized experiment.
  count <- read.csv("data/asv.tab", sep = "\t", row.names=1)
  sample <- read.csv("data/metadata.tsv", sep = "\t", row.names=1)
  tax <- read.csv("data/taxonomy.tsv", sep = "\t", row.names=1)

  # Column and row need to be order with the same exact order, pits
  sample <- sample[ order((row.names(sample))), ]
  # tax

  tse <- SummarizedExperiment(
    assays = count,
    rowData = tax,
    colData = sample
  )

  return(tse)
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

#' Melt fast, calc fast
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

FastMelt.Q <- function(physeq, ...) {
  includeSampleVarsS <- ensyms(...)
  includeSampleVars <- purrr::map(includeSampleVarsS, rlang::as_string)

  FastMelt(physeq, includeSampleVars)
}

AggregateTaxa.Q <- function(ps, taxa_lvl, verbose = FALSE) {
  txt <- rlang::as_label(rlang::enquo(taxa_lvl))
  microbiome::aggregate_taxa(ps, txt, verbose)
}

AggregateTaxa <- function(.ps, rank = "ASV") {
  if (rank == "ASV") {
    return(.ps)
  } else {
    return(microbiome::aggregate_taxa(.ps, rank))
  }
}

# Misc function
# Function for sort heatmap
radial_theta <- function(x) {
  x <- as(x, "matrix")
  theta <- atan2((x[, 2] - mean(x[, 2])), (x[, 1] - mean(x[, 1])))
  names(theta) <- rownames(x)
  theta
}

# Phyloseq helper. Avoid using subset (with cause many headache down the road)
with_samples <- function(ps, condition) {
  if (is.null(sample_data(ps))) {
    stop("Nothing subset. No sample_data in physeq.\n")
  }

  condition_call <- enquo(condition)
  oldDF <- as(sample_data(ps), "data.frame")
  r <- rlang::eval_tidy(condition_call, oldDF)
  newDF <- oldDF[r, ]
  sample_data(ps) <- sample_data(newDF)
  return(ps)
}

with_taxa <- function(physeq, condition) {
  if (is.null(tax_table(physeq))) {
    stop("Nothing subset. No taxonomyTable in physeq.\n")
  }

  condition_call <- enquo(condition)
  oldMA <- as(tax_table(physeq), "matrix")
  oldDF <- data.frame(oldMA)
  r <- rlang::eval_tidy(condition_call, oldDF)
  newDF <- as(oldDF[r, ], "matrix")
  tax_table(physeq) <- tax_table(newDF)
  return(physeq)
}

#' Calculate prevalence from phyloseq
CalcPrev <- function(ps) {
  data.frame(
    Prevalence = apply(
      X = otu_table(ps),
      MARGIN = ifelse(taxa_are_rows(ps), yes = 1, no = 2),
      FUN = function(x) {
        sum(x > 0)
      }
    ),
    TotalAbundance = taxa_sums(ps),
    tax_table(ps)
  ) %>%
    as_tibble(rownames = "ID_OTU")
}

PlotPrev <- function(ps) {
  prevdf <- PrevChk(ps)

  ggplot(prevdf, aes(TotalAbundance, Prevalence / phyloseq::nsamples(ps), color = phylum)) +
    # Include a guess for parameter
    geom_hline(yintercept = 0.05, alpha = 0.5, linetype = 2) +
    geom_point(size = 2, alpha = 0.7) +
    scale_x_log10() +
    xlab("Total Abundance") +
    ylab("Prevalence [Frac. Samples]") +
    facet_wrap(~phylum) +
    theme(legend.position = "none")
}

SumPrev <- function(df, level) {
  df %>%
    mutate(phylum = replace_na({{ level }}, "")) %>%
    group_by(kingdom, {{ level }}) %>%
    summarise(
      maxprev = max(Prevalence),
      meanprev = mean(Prevalence),
      sumprev = sum(Prevalence),
      avgabn = mean(TotalAbundance),
      sumabn = sum(TotalAbundance),
      .groups = "drop"
    )
}

FiltPrev <- function(ps, taxa_lvl, prev = 0.1) {
  prevcount <- ps %>%
    aggregate_taxa(rlang::as_label(enquo(taxa_lvl))) %>%
    microbiome::transform("compositional") %>%
    microbiome::prevalence(detection = 0.0005, sort = T, count = T, include.lowest = T)

  nlimit <- prevcount / nsamples(ps) >= prev
  name_t <- nlimit[nlimit] %>% names()

  with_taxa(ps, {{ taxa_lvl }} %in% name_t)
}

# Hierachical clustering all at once
WideVar <- function(ps, tax_lvl, top_n = 50) {
  rank_str <- rlang::as_label(enquo(tax_lvl)) %>% str_remove_all("\"")

  LongVar(ps, {{ tax_lvl }}, top_n) %>%
    pivot_wider(id_cols = {{ tax_lvl }}, names_from = "ID_sample", values_from = "abn", values_fn = sum) %>%
    column_to_rownames(rank_str)
}


# Longvar ALWAYS tranform with CLR
LongVar <- function(ps, tax_lvl, top_n = 50) {
  rank_str <- rlang::as_label(enquo(tax_lvl)) %>% str_remove_all("\"")

  if (rank_str == "ASV") {
    wdat <- ps %>%
      microbiome::transform("clr") %>%
      FastMelt()
    return(wdat)
  }

  if (top_n) {
    taxfilt <- ps %>%
      AggregateTaxa(rank_str) %>%
      microbiome::transform("compositional") %>%
      FastMelt() %>%
      group_by({{ tax_lvl }}) %>%
      summarise(var = var(abn)) %>%
      arrange(desc(var)) %>%
      dplyr::filter({{ tax_lvl }} != "") %>%
      dplyr::filter({{ tax_lvl }} != "uncultured") %>%
      slice_head(n = top_n) %>%
      pull({{ tax_lvl }})
    wdat <- ps %>%
      AggregateTaxa(rank_str) %>%
      with_taxa({{ tax_lvl }} %in% taxfilt) %>%
      microbiome::transform("clr") %>%
      FastMelt()
  } else {
    # Just without with_taxa()
    wdat <- ps %>%
      AggregateTaxa(rank_str) %>%
      microbiome::transform("clr") %>%
      FastMelt()
  }

  return(wdat)
}


# IO

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