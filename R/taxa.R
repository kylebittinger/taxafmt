#' Standard taxonomic ranks.
#'
#' @export
taxonomic_ranks <- c(
  "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

#' Split lineages into taxa
#'
#' @param lineage Character vector of taxonomic lineages.
#' @param pattern Pattern on which to split taxa in each lineage.
#' @param ranks Character vector of taxonomic ranks, used as column names in the
#'   resultant data frame.
#' @return A data frame of taxonomic assignments.
#' @export
split_lineage <- function (lineage, pattern = "; ", ranks = taxonomic_ranks) {
  lineage <- as.character(lineage) # Just in case it is a factor
  nranks <- length(taxonomic_ranks)
  result <- stringr::str_split_fixed(lineage, pattern, n = nranks)
  colnames(result) <- taxonomic_ranks
  result <- tibble::as_tibble(result)
  result <- dplyr::mutate(result, dplyr::across(tidyselect::everything(), na_if_empty))
  result
}

na_if_empty <- function (x) {
  x[x %in% ""] <- NA_character_
  x
}

#' Remove rank-specific prefixes in taxa
#'
#' @param x Character vector of taxa or lineages
#' @param pattern Regular expression for rank-specific prefix
#' @return Character vector with prefixes removed
#' @export
remove_rank_prefix <- function (x, pattern = "[kpcofgs]__") {
  x <- stringr::str_remove_all(x, pattern)
  dplyr::if_else(x %in% "", NA_character_, x)
}

#' Add the genus to create a binomial species name
#'
#' @param genus_name The genus name, i.e. the first word of the species name in
#'   binomial nomenclature.
#' @param specific_name The specific name, i.e. the second word of the species
#'   name in binomial nomenclature.
#' @return The binomial species name.
#' @export
make_binomial_name <- function (genus_name, specific_name) {
  dplyr::if_else(
    is.na(genus_name) | is.na(specific_name), NA_character_,
    paste(genus_name, specific_name))
}

#' Format taxonomic assignments for presentation
#'
#' @param taxdf A data frame containing taxonomic assignments.
#' @param guide Column name or column number of high-ranking taxa to use as a
#'   guide for lower-ranking taxa. If guide is `NULL`, no guide taxon will be
#'   used.
#' @param sep Separator to use between guide taxon and lowest-ranking taxon.
#' @param unclassified_prefix If the lowest-ranking taxon is `NA`, the function
#'   will use the lowest-ranking taxon available. In this case, a prefix will
#'   be added indicate that the lowest-ranking taxon was undetermined. If
#'   unclassified_prefix is `NULL`, no prefix will be added.
#' @return A character vector of formatted taxonomic assignments.
#' @export
format_taxa <- function(taxdf, guide = "Phylum", sep = " - ",
                        unclassified_prefix = "unclassified") {
  if (is.null(guide) || is.na(guide)) {
    guide_idx <- NULL
  } else if (is.integer(guide)) {
    guide_idx <- guide
  } else {
    guide_idx <- match(guide, colnames(taxdf))
  }
  apply(
    taxdf, 1, format_lineage_vector, guide_idx = guide_idx, sep = sep,
    unclassified_prefix = unclassified_prefix)
}

format_lineage_vector <- function (x, guide_idx = 2, sep = " - ",
                                 unclassified_prefix = "unclassified") {
  primary_idx <- max_idx(x)
  # Nothing is filled in, return NA
  if (is.na(primary_idx)) return(NA_character_)
  primary_taxon <- x[primary_idx]
  # Add a prefix if necessary
  if ((primary_idx < length(x)) & (!is.null(unclassified_prefix))) {
    primary_taxon <- paste(unclassified_prefix, primary_taxon)
  }
  # No guide, return the lowest-ranking taxon
  if (is.null(guide_idx)) return(primary_taxon)
  # Nothing is filled in below the guide, return the lowest-ranking taxon
  if (primary_idx <= guide_idx) return(primary_taxon)
  guide_taxon <- x[guide_idx]
  paste(guide_taxon, primary_taxon, sep = sep)
}

max_idx <- function (x) {
  is_filled <- !is.na(x)
  if (any(is_filled)) {
    max(which(is_filled))
  } else {
    NA_integer_
  }
}
