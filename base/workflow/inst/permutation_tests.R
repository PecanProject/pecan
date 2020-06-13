#!/usr/bin/env Rscript

# This script can be used to quickly generate a permuted list of models,
# met-products, and sites for comprehensive PEcAn integration testing. It
# produces as output a CSV file that can be used by `batch_run.R`.

# It can take the following command line arguments:
#
# - --machine-id=<ID> -- Machine ID. If not provided, try to determine
#   the machine ID from the hostname.
# (TODO: More command line arguments; for now, just modify the variables at the
# top)

library(PEcAn.workflow)
library(tidyverse)

argv <- commandArgs(trailingOnly = TRUE)

met_name <- c("CRUNCEP", "AmerifluxLBL")
startdate <- "2004-01-01"
enddate <- "2004-12-31"
out.var <- "NPP"
ens_size <- 1
sensitivity <- FALSE
outfile <- "permuted_table.csv"

## Insert your path to base pecan
## pecan_path <- "/fs/data3/tonygard/work/pecan"
pecan_path <- file.path("..", "..")
php_file <- file.path(pecan_path, "web", "config.php")
stopifnot(file.exists(php_file))
config.list <- PEcAn.utils::read_web_config(php_file)
bety <- PEcAn.DB::betyConnect(php_file)

# Create path for outfile
dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)

## Find name of Machine R is running on
machid_rxp <- "^--machine_id="
if (any(grepl(machid_rxp, argv))) {
  machid_raw <- grep(machid_rxp, argv, value = TRUE)
  mach_id <- as.numeric(gsub(machid_rxp, "", machid_raw))
  message("Using specified machine ID: ", mach_id)
} else {
  mach_name <- Sys.info()[[4]]
  message("Auto-detected machine name: ", mach_name)
  ## mach_name <- "docker"
  mach_id <- tbl(bety, "machines") %>%
    filter(hostname == !!mach_name) %>%
    pull(id)
}

## Find all models available on the current machine
model_df <- tbl(bety, "dbfiles") %>%
  filter(machine_id == !!mach_id) %>%
  filter(container_type == "Model") %>%
  left_join(tbl(bety, "models"), c("container_id" = "id")) %>%
  select(model_id = container_id, model_name, revision,
         file_name, file_path, dbfile_id = id, ) %>%
  collect() %>%
  mutate(exists = file.exists(file.path(file_path, file_name)))

message("Found the following models on the machine:")
print(model_df)

if (!all(model_df$exists)) {
  message("WARNING: The following models are registered on the machine ",
          "but their files do not exist:")
  model_df %>%
    filter(!exists) %>%
    print()

  model_df <- model_df %>%
    filter(exists)
}

## Find Sites
## Site with no inputs from any machines that is part of Ameriflux site group and Fluxnet Site group
site_id_noinput <- tbl(bety, "sites") %>%
  anti_join(tbl(bety, "inputs")) %>%
  inner_join(tbl(bety, "sitegroups_sites") %>%
               filter(sitegroup_id == 1),
             by = c("id" = "site_id")) %>%
  dplyr::select("id.x", "notes", "sitename") %>%
  dplyr::filter(grepl("TOWER_BEGAN", notes)) %>%
  collect() %>%
  dplyr::mutate(
    # Grab years from string within the notes
    start_year = substring(stringr::str_extract(notes,pattern = ("(?<=TOWER_BEGAN = ).*(?=  TOWER_END)")),1,4),
    #Empty tower end in the notes means that it goes until present day so if empty enter curent year.
    end_year = dplyr::if_else(
      substring(stringr::str_extract(notes,pattern = ("(?<=TOWER_END = ).*(?=)")),1,4) == "",
      as.character(lubridate::year(Sys.Date())),
      substring(stringr::str_extract(notes,pattern = ("(?<=TOWER_END = ).*(?=)")),1,4)
    ),
    #Check if startdate year is within the inerval of that is given
    in_date = data.table::between(as.numeric(lubridate::year(startdate)),as.numeric(start_year),as.numeric(end_year))
  ) %>%
  dplyr::filter(
    in_date,
    as.numeric(end_year) - as.numeric(start_year) > 1
  ) %>%
  mutate(sitename = gsub(" ", "_", sitename)) %>%
  rename(site_id = id.x)

site_id_noinput %>%
  select(site_id, sitename) %>%
  print(n = Inf)

message("Running tests at ", nrow(site_id_noinput), " sites:")
print(site_id_noinput)

site_id <- site_id_noinput$site_id
site_name <- gsub(" ", "_", site_id_noinput$sitename)

# Create permutations of all arguments
options(scipen = 999)
run_table <- tidyr::crossing(
  # Keep model name and revision together -- don't cross them
  tidyr::nesting(
    model = model_df[["model_name"]],
    revision = model_df[["revision"]]
    # Eventually, PFT will go here...
  ),
  # Permute everything else
  met = met_name,
  site_id = site_id,
  pft = NA,
  start_date = startdate,
  end_date = enddate,
  # Possibly, nest these as well, e.g.
  ## tidyr::nesting(
  ##   sensitivity = c(FALSE, TRUE),
  ##   ensemble_size = c(1, 100)
  ## ),
  sensitivity = sensitivity,
  ensemble_size = ensemble_size,
  comment = ""
)

write.csv(run_table, outfile, row.names = FALSE)
