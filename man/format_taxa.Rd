% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa.R
\name{format_taxa}
\alias{format_taxa}
\title{Format taxonomic assignments for presentation}
\usage{
format_taxa(
  taxdf,
  guide = "Phylum",
  sep = " - ",
  unclassified_prefix = "unclassified",
  all_unclassified = "no assignment"
)
}
\arguments{
\item{taxdf}{A data frame containing taxonomic assignments.}

\item{guide}{Column name or column number of high-ranking taxa to use as a
guide for lower-ranking taxa. If guide is \code{NULL}, no guide taxon will be
used.}

\item{sep}{Separator to use between guide taxon and lowest-ranking taxon.}

\item{unclassified_prefix}{If the lowest-ranking taxon is \code{NA}, the function
will use the lowest-ranking taxon available. In this case, a prefix will
be added indicate that the lowest-ranking taxon was undetermined. If
unclassified_prefix is \code{NULL}, no prefix will be added.}

\item{all_unclassified}{If all the taxa are \code{NA}, this value will be used in
the result.}
}
\value{
A character vector of formatted taxonomic assignments.
}
\description{
Format taxonomic assignments for presentation
}
