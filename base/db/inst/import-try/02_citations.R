# Add DOIs to TRY citations
library(tidyverse)
library(DBI)
library(RSQLite)
library(rcrossref)
library(PEcAn.logger)
library(PEcAn.DB)
library(here)

wd <- here("inst", "import-try")
configfile <- file.path(wd, "config.R")
source(configfile)

data_dir <- file.path(wd, "data-proc")
dir.create(data_dir, showWarnings = FALSE)

logger.setLevel("DEBUG")

trydb <- dbConnect(SQLite(), sqlite_file)
reference_dat <- tbl(trydb, "datasets") %>%
  distinct(Reference, ReferenceID) %>%
  collect()

refs_proc_file <- file.path(data_dir, "refs_proc.rds")
if (file.exists(refs_proc_file)) {
  refs_proc <- readRDS(refs_proc_file)
} else {
  refs_proc <- reference_dat %>%
    mutate(cr_df = map(Reference, search_references, min_score = 40)) %>%
    unnest()
  saveRDS(refs_proc, refs_proc_file)
}

# Replace bad matches with NA
minscore <- 85
fill_na <- function(field, score) {
  na <- as(NA, class(field))
  if_else(score > minscore, field, na)
}
refs_proc2 <- refs_proc %>%
  mutate_at(
    c("author", "year", "journal", "vol", "pg", "doi"),
    fill_na,
    score = .$score
  ) %>%
  mutate(
    title = if_else(score > minscore, title, Reference)
  ) %>%
  # TODO: Hack until Bety author column is extended
  mutate_if(is.character, substr, start = 0, stop = 254) %>%
  # TODO: Hack until Bety constraints on these columns are relaxed
  select(-pg)


# Check character column length

bety <- db.open(betyparams)

bety_refs <- db_merge_into(refs_proc2, "citations", bety) %>%
  collect()
