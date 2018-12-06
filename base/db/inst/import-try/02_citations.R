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

bety <- db.open(betyparams)
if (!"notes" %in% dbListFields(bety, "citations")) {
  logger.severe(
    "`notes` column required in Bety citations table ",
    "but not found in this version of Bety. ",
    "Please make sure you have performed schema migration ",
    "version 20180206152600 (relax_citations)."
  )
}

trydb <- dbConnect(SQLite(), sqlite_file)

reference_dat <- tbl(trydb, "datasets") %>%
  distinct(Reference, ReferenceID) %>%
  collect()

refs_proc_file <- file.path(data_dir, "refs_proc.rds")
if (file.exists(refs_proc_file)) {
  refs_proc <- readRDS(refs_proc_file)
} else {
  logger.setLevel("DEBUG")    # To get status messages
  refs_proc <- reference_dat %>%
    mutate(cr_df = map(Reference, search_references, min_score = 40)) %>%
    unnest()
  logger.setLevel("INFO")
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
    c("title", "author", "year", "journal", "vol", "pg", "doi"),
    fill_na,
    score = .$score
  ) %>%
  mutate(
    title = if_else(!is.na(title), title, paste0("TRY ReferenceID ", ReferenceID)),
    author = if_else(!is.na(author), author, "Unknown TRY data (see title)"),
    author = substr(author, 0, 254),    # Trim author to 255 characters
    journal = if_else(!is.na(journal), journal, "Unknown TRY data (see title)"),
    # Use the Kattge 2007 TRY paper's DOI as a placeholder
    doi = if_else(!is.na(doi), doi, "10.1111/j.1365-2486.2011.02451.x"),
    year = if_else(!is.na(year), year, 2018),
    pg = if_else(!is.na(pg), pg, "9999"),
    notes = paste("Original TRY reference: ", Reference)
  )

bety_refs <- db_merge_into(refs_proc2, "citations", bety, "notes") %>%
  collect()
saveRDS(bety_refs, file.path(data_dir, "refs_bety.rds"))
