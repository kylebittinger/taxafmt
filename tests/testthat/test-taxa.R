lineages <- c(
  "k__Bacteria; p__Firmicutes; c__Bacilli; o__Lactobacillales; f__Enterococcaceae; g__Enterococcus",
  "k__Bacteria; p__Actinobacteria; c__Actinobacteria; o__Bifidobacteriales; f__Bifidobacteriaceae",
  "k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales")

taxa <- tibble::tibble(
  Kingdom = c("k__Bacteria", "k__Bacteria", "k__Bacteria"),
  Phylum = c("p__Firmicutes", "p__Actinobacteria", "p__Firmicutes"),
  Class = c("c__Bacilli", "c__Actinobacteria", "c__Clostridia"),
  Order = c("o__Lactobacillales", "o__Bifidobacteriales", "o__Clostridiales"),
  Family = c("f__Enterococcaceae", "f__Bifidobacteriaceae", NA_character_),
  Genus = c("g__Enterococcus", NA_character_, NA_character_),
  Species = c(NA_character_, NA_character_, NA_character_))

assignments <- c(
  "p__Firmicutes - g__Enterococcus",
  "p__Actinobacteria - unclassified f__Bifidobacteriaceae",
  "p__Firmicutes - unclassified o__Clostridiales")

test_that("split_lineage works", {
  expect_equal(split_lineage(lineages), taxa)
})

test_that("remove_rank_prefix works for individual taxa", {
  expect_equal(
    remove_rank_prefix(taxa$Class),
    c("Bacilli", "Actinobacteria", "Clostridia"))
})

test_that("remove_rank_prefix does not leave empty strings", {
  expect_equal(remove_rank_prefix(c("s__A", "s__")), c("A", NA))
})

test_that("remove_rank_prefix works for lineages", {
  expect_equal(remove_rank_prefix(lineages), c(
      "Bacteria; Firmicutes; Bacilli; Lactobacillales; Enterococcaceae; Enterococcus",
      "Bacteria; Actinobacteria; Actinobacteria; Bifidobacteriales; Bifidobacteriaceae",
      "Bacteria; Firmicutes; Clostridia; Clostridiales"))
})

test_that("simplify_assignments works", {
  expect_equal(format_taxa(taxa[, 1:6]), assignments) # Kingdom-Genus
})

test_that("make_binomial_name works", {
  expect_equal(
    make_binomial_name(c("[Ruminococcus]", "Bacteroides"), c("gnavus", NA)),
    c("[Ruminococcus] gnavus", NA))
})
