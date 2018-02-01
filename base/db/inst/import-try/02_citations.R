# Add DOIs to TRY citations
library(tidyverse)
library(DBI)
library(RSQLite)
library(rcrossref)
library(PEcAn.logger)

sqlite_file <- "try.sqlite"

trydb <- dbConnect(SQLite(), sqlite_file)
references <- trydb %>%
  tbl("datasets") %>%
  distinct(Reference) %>%
  pull(Reference)

references <- readLines("references")[1:15]

logger.setLevel("DEBUG")
tidy_refs <- search_references(references)
