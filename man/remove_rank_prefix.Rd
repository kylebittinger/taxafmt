% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa.R
\name{remove_rank_prefix}
\alias{remove_rank_prefix}
\title{Remove rank-specific prefixes in taxa}
\usage{
remove_rank_prefix(x, pattern = "[kpcofgs]__")
}
\arguments{
\item{x}{Character vector of taxa or lineages}

\item{pattern}{Regular expression for rank-specific prefix}
}
\value{
Character vector with prefixes removed
}
\description{
Remove rank-specific prefixes in taxa
}
