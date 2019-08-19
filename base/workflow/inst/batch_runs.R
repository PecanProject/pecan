#!/usr/bin/env Rscript

# if (!requireNamespace("huxtable")) {
#   message("Installing missing package 'huxtable'")
#   install.packages("huxtable")
# }
# 
# if (!requireNamespace("htmlTable")) {
#   message("Installing missing package 'htmlTable'")
#   install.packages("htmlTable")
# }

## This script contains the Following parts:
## Part 1 - Write Main Function
## A. takes in a list defining specifications for a single run and assigns them to objects
## B. The function then writes out a pecan settings list object
## C. A directory is created from the ouput folder defined in a users config.php file
## D. Settings object gets written into pecan.xml file and put in output directory.
## E. workflow.R is copied into this directory and executed.
## F. console output is stored in a file called workflow.Rout
## Part 2 - Write run settings
## A. Set BETY connection object. Needs user to define path to their pecan directory.
## B. Find Machine ID
## C. Find Models ids based on machine
## D. Manually Define Met, right now CRUNCEP and AMerifluxlbl
## E. Manually Define start and end date
## F. Manually Define Output var
## G. Manually Define Sensitivity and enesmble
## H. Find sites not associated with any inputs(meaning data needs to be downloaded), part of ameriflux network, and have data for start-end year range.
## Available ameriflux data is found by parsing the notes section of the sites table where ameriflux sites have a year range.
## Part 3 - Create Run table
## A. Create table that contains combinations of models,met_name,site_id, startdate, enddate, pecan_path,out.var, ensemble, ens_size, sensitivity args that the function above will use to do runs.
## Part 4 - Run the Function across the table
## A. Use the pmap function to apply the function across each row of arguments and append a pass or fail outcome column to the original table of runs
## Part 5 - (In progress) Turn output table into a table

#' .. title/description ..
#'
#' @param model_id Model ID (from `models` table)
#' @param met Meteorology input source (e.g. "CRUNCEP")
#' @param site_id Site ID (from `sites` table)
#' @param start_date Run start date
#' @param end_date Run end date
#' @param pecan_path Path to PEcAn source code. Default is current
#'   working directory.
#' @param user_id
#' @param output_folder
#' @param dbfiles_folder
#' @param pft_name
#' @param ensemble_size
#' @param ensemble_variable
#' @param sensitivity
#' @param db_bety_username
#' @param db_bety_password
#' @param db_bety_hostname
#' @param db_bety_driver
#' @return 
#' @author Alexey Shiklomanov
create_execute_test_xml <- function(model_id,
                                    met,
                                    site_id,
                                    start_date,
                                    end_date,
                                    dbfiles_folder,
                                    user_id,
                                    output_folder = "batch_test_output",
                                    pecan_path = getwd(),
                                    pft_name = NULL,
                                    ensemble_size = 1,
                                    ensemble_variable = "NPP",
                                    sensitivity = FALSE,
                                    db_bety_username = "bety",
                                    db_bety_password = "bety",
                                    db_bety_hostname = "localhost",
                                    db_bety_driver = "PostgreSQL") {

  php_file <- file.path(pecan_path, "web", "config.php")
  config.list <- read_web_config(php_file)
  bety <- betyConnect(php_file)
  con <- bety$con

  settings <- list(
    info = list(notes = "Test_Run",
                userid = user_id,
                username = "None",
                dates = Sys.Date())
  )

  #Outdir
  model.new <- tbl(bety, "models") %>%
    filter(id == !!model_id) %>%
    collect()
  outdir_pre <- paste(
    model.new[["model_name"]],
    format(as.Date(start_date), "%Y-%m"),
    format(as.Date(end_date), "%Y-%m"),
    met, site_id, "test_runs",
    sep = "_"
  )
  outdir <-  file.path(output_folder, outdir_pre)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  settings$outdir <- outdir

  #Database BETY
  settings$database <- list(
    bety = list(user = db_bety_username,
                password = db_bety_password,
                host = db_bety_hostname,
                dbname = "bety",
                driver = db_bety_driver,
                write = FALSE),
    dbfiles = dbfiles_folder
  )

  #PFT
  if (is.null(pft_name)){
    # Select the first PFT in the model list.
    pft <- tbl(bety, "pfts") %>%
      filter(modeltype_id == !!model.new$modeltype_id) %>%
      collect()
    pft_name <- pft$name[[1]]
  }
  settings$pfts <- list(
    pft = list(name = pft_name,
               constants = list(num = 1))
  )

  #Meta Analysis
  settings$meta.analysis <- list(iter = 3000, random.effects = FALSE)

  #Ensemble
  settings$ensemble <- list(
    size = ensemble_size,
    variable = ensemble_variable,
    samplingspace = list(met = list(method = "sampling"),
                         parameters = list(method = "uniform"))
  )

  #Sensitivity 
  if (sensitivity) {
    settings$sensitivity.analysis <- list(
      quantiles = list(sigma1 = -2, sigma2 = -1, sigma3 = 1, sigma4 = 2)
    )
  }

  #Model
  settings$model$id <- model.new[["id"]]

  #Workflow
  settings$workflow$id
  settings$workflow$id <- paste0("Test_run_","_",model.new$model_name)
  settings$run <- list(
    site = list(id = site_id, met.start = start_date, met.end = end_date),
    inputs = list(met = list(source = met, output = model.new[["model_name"]],
                             username = "pecan")),
    start.date = start_date, end.date = end_date
  )
  settings$host$name <- "localhost"

  #create file and Run
  saveXML(listToXml(settings, "pecan"), file = file.path(outdir, "pecan.xml"))
  file.copy(file.path(pecan_path, "web", "workflow.R"), outdir)
  cwd <- getwd()
  setwd(outdir)
  on.exit(setwd(cwd), add = TRUE)

  system("Rscript workflow.R 2>&1 | tee workflow.Rout")
}

library(tidyverse)
library(PEcAn.DB)
library(PEcAn.utils)
library(XML)
library(PEcAn.settings)

argv <- commandArgs(trailingOnly = TRUE)

##Create Run Args

## Insert your path to base pecan
## pecan_path <- "/fs/data3/tonygard/work/pecan"
pecan_path <- file.path("..", "..")
php_file <- file.path(pecan_path, "web", "config.php")
stopifnot(file.exists(php_file))
config.list <- PEcAn.utils::read_web_config(php_file)
bety <- PEcAn.DB::betyConnect(php_file)
con <- bety$con

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

## Find Models
#devtools::install_github("pecanproject/pecan", subdir = "api")
model_ids <- tbl(bety, "dbfiles") %>%
  filter(machine_id == !!mach_id) %>%
  filter(container_type == "Model") %>%
  pull(container_id)

models <- model_ids
met_name <- c("CRUNCEP", "AmerifluxLBL")
startdate <- "2004-01-01"
enddate <- "2004-12-31"
out.var <- "NPP"
ensemble <- FALSE
ens_size <- 100
sensitivity <- FALSE
user_id <- 99000000002   # TODO: Un-hard-code this
dbfiles_folder <- normalizePath("~/output/dbfiles") # TODO: Un-hard-code this

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


site_id <- site_id_noinput$site_id
site_name <- gsub(" ", "_", site_id_noinput$sitename)

#Create permutations of arg combinations
options(scipen = 999)
run_table <- expand.grid(
  model_id = models,
  met = met_name,
  site_id = site_id,
  start_date = startdate,
  end_date = enddate,
  pecan_path = pecan_path,
  ensemble_variable = out.var,
  # ensemble = ensemble,
  ensemble_size = ens_size,
  sensitivity = sensitivity,
  user_id = user_id,
  dbfiles_folder = dbfiles_folder,
  stringsAsFactors = FALSE
)
#Execute function to spit out a table with a column of NA or success

for (i in seq_len(NROW(run_table))) {
  message("\n\n############################################")
  message("Testing the following configuration:")
  glimpse(run_table[i, ])
  do.call(create_execute_test_xml, run_table[i, ])
  message("Done!")
  message("############################################\n\n")
}

# tab <- run_table %>%
#   mutate(
#     outcome = purrr::pmap(
#       .,
#       purrr::possibly(function(...) create_execute_test_xml(list(...)),
#                       otherwise = NA)
#     )
#   )
# 
# print(tab, n = Inf)

## print to table
# tux_tab <- huxtable::hux(tab)
# html_table <- huxtable::print_html(tux_tab)
# htmlTable::htmlTable(tab)
