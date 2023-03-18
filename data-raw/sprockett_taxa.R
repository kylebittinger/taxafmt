## code to prepare `sprockett_taxa` dataset goes here

library(tidyverse)

sprockett_taxa <- read_tsv(
  "data-raw/sprockett_taxa.tsv", show_col_types = FALSE) %>%
  slice(1:10) %>%
  select(feature_id = `Feature ID`, lineage = Taxon) %>%
  mutate(feature_id = str_sub(feature_id, 1, 7))

usethis::use_data(sprockett_taxa, overwrite = TRUE)
