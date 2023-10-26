
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taxafmt

<!-- badges: start -->
<!-- badges: end -->

The goal of taxafmt is to format and parse taxonomic assignments,
especially for microbiome data sets.

## Installation

You can install the development version of taxafmt like so:

``` r
# install.packages("devtools")
devtools::install_github("kylebittinger/taxafmt")
```

## Example

The `taxafmt` package comes with a small data set of taxonomic
assignments.

``` r
sprockett_taxa
#> # A tibble: 10 × 2
#>    feature_id lineage                                                           
#>    <chr>      <chr>                                                             
#>  1 f2ad15e    k__Bacteria; p__Firmicutes; c__Bacilli; o__Lactobacillales; f__En…
#>  2 560c185    k__Bacteria; p__Actinobacteria; c__Actinobacteria; o__Bifidobacte…
#>  3 2766544    k__Bacteria; p__Proteobacteria; c__Gammaproteobacteria; o__Entero…
#>  4 ac7454f    k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__L…
#>  5 39ba0d9    k__Bacteria; p__Actinobacteria; c__Actinobacteria; o__Bifidobacte…
#>  6 79286fc    k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__L…
#>  7 f895c95    k__Bacteria; p__Firmicutes; c__Bacilli; o__Lactobacillales; f__En…
#>  8 a6ac740    k__Bacteria; p__Proteobacteria; c__Gammaproteobacteria; o__Entero…
#>  9 848fce2    k__Bacteria; p__Actinobacteria; c__Actinobacteria; o__Bifidobacte…
#> 10 b8bedcf    k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__L…
```

Here, we’ll show how to parse and format the assignments for use in
plots or statistical comparisons. Our first goal is to split up the taxa
into ranks, like family, genus, and species. The `split_lineage`
function does this for us.

``` r
split_lineage(sprockett_taxa$lineage)
#> # A tibble: 10 × 7
#>    Kingdom     Phylum            Class                Order Family Genus Species
#>    <chr>       <chr>             <chr>                <chr> <chr>  <chr> <chr>  
#>  1 k__Bacteria p__Firmicutes     c__Bacilli           o__L… f__En… g__E… <NA>   
#>  2 k__Bacteria p__Actinobacteria c__Actinobacteria    o__B… f__Bi… g__B… <NA>   
#>  3 k__Bacteria p__Proteobacteria c__Gammaproteobacte… o__E… f__En… <NA>  <NA>   
#>  4 k__Bacteria p__Firmicutes     c__Clostridia        o__C… f__La… g__[… s__gna…
#>  5 k__Bacteria p__Actinobacteria c__Actinobacteria    o__B… f__Bi… g__B… <NA>   
#>  6 k__Bacteria p__Firmicutes     c__Clostridia        o__C… f__La… g__B… s__    
#>  7 k__Bacteria p__Firmicutes     c__Bacilli           o__L… f__En… g__E… <NA>   
#>  8 k__Bacteria p__Proteobacteria c__Gammaproteobacte… o__E… f__En… <NA>  <NA>   
#>  9 k__Bacteria p__Actinobacteria c__Actinobacteria    o__B… f__Bi… g__B… <NA>   
#> 10 k__Bacteria p__Firmicutes     c__Clostridia        o__C… f__La… g__[… s__gna…
```

That’s okay, but we’d prefer to have the taxa lined up beside the
Feature ID’s. We can use the `tidyverse` for that.

``` r
library(tidyverse)
sprockett_taxa |>
  mutate(split_lineage(lineage))
#> # A tibble: 10 × 9
#>    feature_id lineage            Kingdom Phylum Class Order Family Genus Species
#>    <chr>      <chr>              <chr>   <chr>  <chr> <chr> <chr>  <chr> <chr>  
#>  1 f2ad15e    k__Bacteria; p__F… k__Bac… p__Fi… c__B… o__L… f__En… g__E… <NA>   
#>  2 560c185    k__Bacteria; p__A… k__Bac… p__Ac… c__A… o__B… f__Bi… g__B… <NA>   
#>  3 2766544    k__Bacteria; p__P… k__Bac… p__Pr… c__G… o__E… f__En… <NA>  <NA>   
#>  4 ac7454f    k__Bacteria; p__F… k__Bac… p__Fi… c__C… o__C… f__La… g__[… s__gna…
#>  5 39ba0d9    k__Bacteria; p__A… k__Bac… p__Ac… c__A… o__B… f__Bi… g__B… <NA>   
#>  6 79286fc    k__Bacteria; p__F… k__Bac… p__Fi… c__C… o__C… f__La… g__B… s__    
#>  7 f895c95    k__Bacteria; p__F… k__Bac… p__Fi… c__B… o__L… f__En… g__E… <NA>   
#>  8 a6ac740    k__Bacteria; p__P… k__Bac… p__Pr… c__G… o__E… f__En… <NA>  <NA>   
#>  9 848fce2    k__Bacteria; p__A… k__Bac… p__Ac… c__A… o__B… f__Bi… g__B… <NA>   
#> 10 b8bedcf    k__Bacteria; p__F… k__Bac… p__Fi… c__C… o__C… f__La… g__[… s__gna…
```

In some taxonomic systems like this one, the taxa are given a one-letter
prefix to indicate the rank. This is convenient, especially when we want
to double check that the correct taxon is in each column, but it’s too
ugly to use in plots and reports. The function `remove_rank_prefix` will
remove this prefix.

``` r
sprockett_taxa |>
  mutate(split_lineage(lineage)) |>
  mutate(across(Kingdom:Species, remove_rank_prefix))
#> # A tibble: 10 × 9
#>    feature_id lineage            Kingdom Phylum Class Order Family Genus Species
#>    <chr>      <chr>              <chr>   <chr>  <chr> <chr> <chr>  <chr> <chr>  
#>  1 f2ad15e    k__Bacteria; p__F… Bacter… Firmi… Baci… Lact… Enter… Ente… <NA>   
#>  2 560c185    k__Bacteria; p__A… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>   
#>  3 2766544    k__Bacteria; p__P… Bacter… Prote… Gamm… Ente… Enter… <NA>  <NA>   
#>  4 ac7454f    k__Bacteria; p__F… Bacter… Firmi… Clos… Clos… Lachn… [Rum… gnavus 
#>  5 39ba0d9    k__Bacteria; p__A… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>   
#>  6 79286fc    k__Bacteria; p__F… Bacter… Firmi… Clos… Clos… Lachn… Blau… <NA>   
#>  7 f895c95    k__Bacteria; p__F… Bacter… Firmi… Baci… Lact… Enter… Ente… <NA>   
#>  8 a6ac740    k__Bacteria; p__P… Bacter… Prote… Gamm… Ente… Enter… <NA>  <NA>   
#>  9 848fce2    k__Bacteria; p__A… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>   
#> 10 b8bedcf    k__Bacteria; p__F… Bacter… Firmi… Clos… Clos… Lachn… [Rum… gnavus
```

The species names are not quite right–they only contain the specific
epithet, which is the second word of the species name. The first word in
the species name is the genus. To generate real species names, we can
use the function `make_binomial_name`.

``` r
sprockett_taxa |>
  mutate(split_lineage(lineage)) |>
  mutate(across(Kingdom:Species, remove_rank_prefix)) |>
  mutate(Species = make_binomial_name(Genus, Species))
#> # A tibble: 10 × 9
#>    feature_id lineage            Kingdom Phylum Class Order Family Genus Species
#>    <chr>      <chr>              <chr>   <chr>  <chr> <chr> <chr>  <chr> <chr>  
#>  1 f2ad15e    k__Bacteria; p__F… Bacter… Firmi… Baci… Lact… Enter… Ente… <NA>   
#>  2 560c185    k__Bacteria; p__A… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>   
#>  3 2766544    k__Bacteria; p__P… Bacter… Prote… Gamm… Ente… Enter… <NA>  <NA>   
#>  4 ac7454f    k__Bacteria; p__F… Bacter… Firmi… Clos… Clos… Lachn… [Rum… [Rumin…
#>  5 39ba0d9    k__Bacteria; p__A… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>   
#>  6 79286fc    k__Bacteria; p__F… Bacter… Firmi… Clos… Clos… Lachn… Blau… <NA>   
#>  7 f895c95    k__Bacteria; p__F… Bacter… Firmi… Baci… Lact… Enter… Ente… <NA>   
#>  8 a6ac740    k__Bacteria; p__P… Bacter… Prote… Gamm… Ente… Enter… <NA>  <NA>   
#>  9 848fce2    k__Bacteria; p__A… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>   
#> 10 b8bedcf    k__Bacteria; p__F… Bacter… Firmi… Clos… Clos… Lachn… [Rum… [Rumin…
```

Finally, we’d like to create a shortened label for each taxonomic
assignment, something that’s nice to use in a plot or report. The
function `format_taxa` does this. We give `format_taxa` a data frame of
the taxa, and it generates labels using the lowest-ranking taxon in the
data frame (which should be on the right hand side). If that taxon is
`NA`, then we get a label containing the lowest-rank taxon that’s
available, but tagged to indicate that it’s not classified all the way
down.

Even though we worked to tidy up the species names, we can’t really
trust species-level assignments for a data set like this. Therefore, we
use the `pick` function from `dplyr` to select the taxa from `Kingdom`
to `Genus` when making the labels.

``` r
sprockett_taxa |>
  mutate(split_lineage(lineage)) |>
  mutate(across(Kingdom:Species, remove_rank_prefix)) |>
  mutate(Species = make_binomial_name(Genus, Species)) |>
  mutate(label = format_taxa(pick(Kingdom:Genus)))
#> # A tibble: 10 × 10
#>    feature_id lineage      Kingdom Phylum Class Order Family Genus Species label
#>    <chr>      <chr>        <chr>   <chr>  <chr> <chr> <chr>  <chr> <chr>   <chr>
#>  1 f2ad15e    k__Bacteria… Bacter… Firmi… Baci… Lact… Enter… Ente… <NA>    Firm…
#>  2 560c185    k__Bacteria… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>    Acti…
#>  3 2766544    k__Bacteria… Bacter… Prote… Gamm… Ente… Enter… <NA>  <NA>    Prot…
#>  4 ac7454f    k__Bacteria… Bacter… Firmi… Clos… Clos… Lachn… [Rum… [Rumin… Firm…
#>  5 39ba0d9    k__Bacteria… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>    Acti…
#>  6 79286fc    k__Bacteria… Bacter… Firmi… Clos… Clos… Lachn… Blau… <NA>    Firm…
#>  7 f895c95    k__Bacteria… Bacter… Firmi… Baci… Lact… Enter… Ente… <NA>    Firm…
#>  8 a6ac740    k__Bacteria… Bacter… Prote… Gamm… Ente… Enter… <NA>  <NA>    Prot…
#>  9 848fce2    k__Bacteria… Bacter… Actin… Acti… Bifi… Bifid… Bifi… <NA>    Acti…
#> 10 b8bedcf    k__Bacteria… Bacter… Firmi… Clos… Clos… Lachn… [Rum… [Rumin… Firm…
```

Now, let’s check out those labels.

``` r
sprockett_taxa |>
  mutate(split_lineage(lineage)) |>
  mutate(across(Kingdom:Species, remove_rank_prefix)) |>
  mutate(Species = make_binomial_name(Genus, Species)) |>
  mutate(label = format_taxa(pick(Kingdom:Genus))) |>
  select(feature_id, label)
#> # A tibble: 10 × 2
#>    feature_id label                                           
#>    <chr>      <chr>                                           
#>  1 f2ad15e    Firmicutes - Enterococcus                       
#>  2 560c185    Actinobacteria - Bifidobacterium                
#>  3 2766544    Proteobacteria - unclassified Enterobacteriaceae
#>  4 ac7454f    Firmicutes - [Ruminococcus]                     
#>  5 39ba0d9    Actinobacteria - Bifidobacterium                
#>  6 79286fc    Firmicutes - Blautia                            
#>  7 f895c95    Firmicutes - Enterococcus                       
#>  8 a6ac740    Proteobacteria - unclassified Enterobacteriaceae
#>  9 848fce2    Actinobacteria - Bifidobacterium                
#> 10 b8bedcf    Firmicutes - [Ruminococcus]
```
