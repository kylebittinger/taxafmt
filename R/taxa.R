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
#' @param guide Column name of high-ranking taxa to use as a guide
#'   for lower-ranking taxa.
#' @param sep Separator to use between guide taxon and lowest-ranking taxon.
#' @param unclassified_prefix If the lowest-ranking taxon is `NA`, the function
#'   will use the lowest-ranking taxon available. In this case, a prefix will
#'   be added indicate that the lowest-ranking taxon was undetermined. If
#'   unclassified_prefix is `NULL`, no prefix will be added.
#' @return A character vector of formatted taxonomic assignments.
#' @export
format_taxa <- function(taxdf, guide = "Phylum", sep = " - ",
                        unclassified_prefix = "unclassified") {
  if (is.integer(guide)) {
    guide_idx <- guide
  } else {
    guide_idx <- match(guide, colnames(taxdf))
  }
  apply(taxdf, 1, function (x) {
    n_total <- length(x)
    n_filled <- filled_length(x)
    if (n_filled < guide_idx) return(paste(x, collapse = sep))
    guide_taxon <- x[guide_idx]
    if (n_filled == guide_idx) return(guide_taxon)
    lowest_taxon <- x[n_filled]
    if ((n_filled < n_total) & (!is.null(unclassified_prefix))) {
      prefixed <- paste(unclassified_prefix, lowest_taxon)
      return(paste(guide_taxon, prefixed, sep = sep))
    }
    return(paste(guide_taxon, lowest_taxon, sep = sep))
  })
}

filled_length <- function (x) {
  n_empty <- match(TRUE, cumsum(is.na(rev(x))) > 0, nomatch = 0)
  length(x) - n_empty
}
