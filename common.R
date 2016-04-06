# Common definitions and packages

# Load packages
library(data.table)
library(bit64)
library(PEcAn.DB)
library(RPostgreSQL)

# Database parameters
user_id <- "1000000013"     # Alexey Shiklomanov
dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety")
con <- db.open(dbparms)
