# Add DOIs to TRY citations
library(tidyverse)
library(DBI)
library(RSQLite)
library(rcrossref)
library(PEcAn.logger)
library(PEcAn.DB)

logger.setLevel("DEBUG")

sqlite_file <- "try.sqlite"

trydb <- dbConnect(SQLite(), sqlite_file)
reference_dat <- tbl(trydb, "datasets") %>%
  distinct(Reference, ReferenceID) %>%
  collect()

refs_proc <- reference_dat %>%
  mutate(cr_df = map(Reference, search_references, min_score = 40)) %>%
  unnest()

source("inst/import-try/bety_connect.R")
bety_refs <- db_merge_into(refs_proc, "citations", bety, "doi") %>%
  collect()

# TODO: Use object `bety`, table `citations`
