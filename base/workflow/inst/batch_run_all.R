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

library(PEcAn.workflow)
library(tidyverse)

argv <- commandArgs(trailingOnly = TRUE)

##Create Run Args
met_name <- c("CRUNCEP", "AmerifluxLBL")
startdate <- "2004-01-01"
enddate <- "2004-12-31"
out.var <- "NPP"
ensemble <- FALSE
ens_size <- 1 # Run ensemble analysis for some models?
sensitivity <- FALSE
user_id <- 99000000002   # TODO: Un-hard-code this
dbfiles_folder <- normalizePath("~/output/dbfiles") # TODO: Un-hard-code this

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

models <- model_df[["model_id"]]

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

result_table <- as_tibble(run_table) %>%
  mutate(
    outdir = NA_character_,
    workflow_complete = NA,
    has_jobsh = NA,
    model_output_raw = NA,
    model_output_processed = NA
  )

for (i in seq_len(nrow(run_table))) {
  result_table %>%
    filter(!is.na(outdir)) %>%
    write_csv("result_table.csv")
  message("\n\n############################################")
  message("Testing the following configuration:")
  glimpse(run_table[i, ])
  raw_result <- do.call(create_execute_test_xml, run_table[i, ])
  outdir <- raw_result$outdir
  result_table$outdir[[i]] <- outdir
  ##################################################
  # Did the workflow finish?
  ##################################################
  raw_output <- readLines(file.path(outdir, "workflow.Rout"))
  result_table$workflow_complete[[i]] <- any(grepl("PEcAn Workflow Complete", raw_output))
  ##################################################
  # Did we write a job.sh file?
  ##################################################
  out <- file.path(outdir, "out")
  run <- file.path(outdir, "run")
  jobsh <- list.files(run, "job\\.sh", recursive = TRUE)
  ## pft <- file.path(outdir, "pft")
  if (length(jobsh) > 0) {
    result_table$has_jobsh[[i]] <- TRUE
  } else {
    next
  }
  ##################################################
  # Did the model produce any output?
  ##################################################
  raw_out <- list.files(out, recursive = TRUE)
  if (length(raw_out) > 0) {
    result_table$model_output_raw[[i]] <- TRUE
  } else {
    next
  }
  ##################################################
  # Did PEcAn post-process the output?
  ##################################################
  # Files should have name `YYYY.nc`
  proc_out <- list.files(out, "[[:digit:]]{4}\\.nc", recursive = TRUE)
  if (length(proc_out) > 0) {
    result_table$model_output_processed[[i]] <- TRUE
  } else {
    next
  }
}

result_table %>%
  filter(!is.na(outdir)) %>%
  # Add model information, to make this easier to read
  left_join(select(model_df, model_id, model_name, revision),
            "model_id") %>%
  write_csv("result_table.csv")

## out_table <- result_table %>%
##   left_join(select(model_df, model_id, model_name, revision),
##             "model_id") %>%
##   select(model = model_name, revision, site_id,
##          workflow_complete, has_jobsh, model_output_raw,
##          model_output_processed)

## write_csv(result_table, "result_table.csv")

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


## Test the Demos
# Site Niwot ridge
# Temperate COniferous
# Year 2003/01/01-2006/12/31
# MET AmerifluxLBL
# Model - Sipnet
# Basic Run, then sensitivity and ensemble run.
##
demo_model <- 1000000014
demo_met <- "AmerifluxLBL"
demo_start <- "2003/01/01"
demo_end <- "2006/12/31"
demo_site <- 772

demo_one_result <- create_execute_test_xml(
  model_id = demo_model,
  met = demo_met,
  site_id = demo_site,
  start_date = demo_start,
  end_date = demo_end,
  dbfiles_folder = dbfiles_folder,
  user_id = user_id,
  output_folder = "batch_test_demo1_output",
)

demo_two_result <- create_execute_test_xml(
  model_id = demo_model,
  met = demo_met,
  site_id = demo_site,
  start_date = demo_start,
  end_date = demo_end,
  dbfiles_folder = dbfiles_folder,
  user_id = user_id,
  output_folder = "batch_test_demo2_output",
  ensemble_size = 100,
  sensitivity = TRUE
)
