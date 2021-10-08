#!/usr/bin/env Rscript
library(dplyr)
library(purrr)
library(PEcAn.workflow)
library(furrr)
library(PEcAn.DB)
library(PEcAn.utils)
plan(multiprocess)
##################################################
# Parse arguments
argv <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% argv) {
  message(
    "This script supports the following options:\n",
    "--help			Print this help message.\n",
    "--dbfiles=<path>	Path to dbfiles folder",
    "--table=<path>	Path to table listing tests to run",
    "--userid=<id>		User ID for registering workflow.",
    "--outdir=<id>		Path to output directory.",
    "--pecandir=<id>	Path to PEcAn root directory.",
    "--outfile=<path>	Path to output table"
  )
  quit(save = "no", status = 0)
}

get_arg <- function(argv, pattern, default_value) {
  if (any(grepl(pattern, argv))) {
    result <- argv[grep(pattern, argv)] %>%
      gsub(pattern = paste0(pattern, "="), replacement = "")
  } else {
    result <- default_value
  }
  return(result)
}

dbfiles_folder <- normalizePath(get_arg(argv, "--dbfiles", "~/output/dbfiles"))
input_table_file <- get_arg(argv, "--table",system.file("default_tests.csv", package = "PEcAn.workflow"))
user_id <- as.numeric(get_arg(argv, "--userid", 99000000002))
pecan_path <- get_arg(argv, "--pecandir", getwd())
output_folder <- get_arg(argv, "--outdir", "batch_test_output")
outfile <- get_arg(argv, "--outfile", "test_result_table.csv")
##################################################
# Establish database connection based on config.php
php_file <- file.path(pecan_path, "web", "config.php")
stopifnot(file.exists(php_file))
config.list <- PEcAn.utils::read_web_config(php_file)
bety <- PEcAn.DB::betyConnect(php_file)

# Create outfile directory if it doesn't exist
dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
input_table <- read.csv(input_table_file, stringsAsFactors = FALSE) %>%
  tidyr::replace_na(list(revision = "")) %>%
  mutate(
    folder= paste(model,
                  format(as.Date(start_date), "%Y-%m"),
                  format(as.Date(end_date), "%Y-%m"),
                  met, site_id, "test_runs", sep = "_")
    outdir = NA_character_,
    workflow_complete = NA,
    has_jobsh = NA,
    model_output_raw = NA,
    model_output_processed = NA
  )

for (i in seq_len(nrow(input_table))) {
  table_row <- input_table[i, ]

  # Get model ID
  model <- table_row$model
  revision <- table_row$revision
  message("Model: ", shQuote(model))
  message("Revision: ", shQuote(revision))
  model_df <- tbl(bety, "models") %>%
    filter(model_name == !!model,
           revision == !!revision) %>%
    collect()
  if (nrow(model_df) == 0) {
    message("No models found with name ", model,
	    " and revision ", revision, ".\n",
	    "Moving on to next row.")
    next
  } else if (nrow(model_df) > 1) {
    print(model_df)
    message("Multiple models found with name ", model,
	    " and revision ", revision, ".\n",
	    "Moving on to next row.")
    next
  } else {
    model_id <- model_df$id
  }

  pft <- table_row$pft
  if (is.na(pft)) pft <- NULL

  # Run test
  raw_result <- create_execute_test_xml(
    model_id = model_id,
    met = table_row$met,
    site_id = table_row$site_id,
    pft = pft,
    start_date = table_row$start_date,
    end_date = table_row$end_date,
    dbfiles_folder = dbfiles_folder,
    pecan_path = pecan_path,
    user_id = user_id,
    ensemble_size = table_row$ensemble_size,
    sensitivity = table_row$sensitivity
  )
#----------------------- Parallel Distribution of jobs
seq_len(nrow(input_table)) %>%
  furrr::future_map(function(i){
    # Each job needs to have its own connection
    # Establish database connection based on config.php
    php_file <- file.path(pecan_path, "web", "config.php")
    stopifnot(file.exists(php_file))
    config.list <- PEcAn.utils::read_web_config(php_file)
    bety <- PEcAn.DB::betyConnect(php_file)
    con <- bety$con
    
    # Get model ID
    table_row <- input_table[i, ]
    model <- table_row$model
    revision <- table_row$revision
    message("Model: ", shQuote(model))
    message("Revision: ", shQuote(revision))
    
    
    
    model_df <- tbl(con, "models") %>%
      filter(model_name == !!model,
             revision == !!revision) %>%
      collect()
    
    if (nrow(model_df) == 0) {
      message("No models found with name ", model,
              " and revision ", revision, ".\n",
              "Moving on to next row.")
      next
    } else if (nrow(model_df) > 1) {
      print(model_df)
      message("Multiple models found with name ", model,
              " and revision ", revision, ".\n",
              "Moving on to next row.")
      next
    } else {
      model_id <- model_df$id
    }
    
    pft <- table_row$pft
    if (is.na(pft)) pft <- NULL
    
    # Run test
    raw_result <- create_execute_test_xml(
      model_id = model_id,
      met = table_row$met,
      site_id = table_row$site_id,
      pft = pft,
      start_date = table_row$start_date,
      end_date = table_row$end_date,
      dbfiles_folder = dbfiles_folder,
      pecan_path = pecan_path,
      user_id = user_id,
      ensemble_size = table_row$ensemble_size,
      sensitivity = table_row$sensitivity,
      output_folder=output_folder
    )
    
  })



#----------- Checking the results of the runs
checks_df<-file.path(output_folder, input_table$folder)%>% 
  purrr::map_dfr(function(outdir){
    
    result_table <-NULL
    ##################################################
    # Did the workflow finish?
    ##################################################
    if (file.exists(file.path(outdir, "workflow.Rout"))) {
      raw_output <- readLines(file.path(outdir, "workflow.Rout"))
      result_table$workflow_complete <- any(grepl(
        "PEcAn Workflow Complete",
        raw_output
      ))
    }else{
      result_table$workflow_complete <- FALSE
    }
    ##################################################
    # Did we write a job.sh file?
    ##################################################
    out <- file.path(outdir, "out")
    run <- file.path(outdir, "run")
    jobsh <- list.files(run, "job\\.sh", recursive = TRUE)
    ## pft <- file.path(outdir, "pft")
    result_table$has_jobsh <-ifelse(length(jobsh) > 0, TRUE, FALSE)
    ##################################################
    # Did the model produce any output?
    ##################################################
    raw_out <- list.files(out, recursive = TRUE)
    result_table$model_output_raw <-ifelse(length(raw_out) > 0, TRUE, FALSE)
    ##################################################
    # Did PEcAn post-process the output?
    ##################################################
    # Files should have name `YYYY.nc`
    proc_out <- list.files(out, "[[:digit:]]{4}\\.nc", recursive = TRUE)
    result_table$model_output_processed <-ifelse(length(proc_out) > 0, TRUE, FALSE)
    
    return(result_table %>% 
             as.data.frame() %>%
             mutate(folder=basename(outdir))
    )
    
  })

#-- Writing down the results
input_table %>%
  left_join(checks_df,
            by="folder") %>%
  write.csv(outfile, row.names = FALSE)
