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
references <- trydb %>%
  tbl("datasets") %>%
  distinct(Reference) %>%
  pull(Reference)

try_refs <- search_references(references, min_score = 1)

source("inst/import-try/bety_connect.R")

# TODO: Use object `bety`, table `citations`
