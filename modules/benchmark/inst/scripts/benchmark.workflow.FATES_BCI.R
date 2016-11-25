##' Benchmarking workflow
##' Only use this if using and existing run (ie settings$new_run == FALSE)
##' Author: Betsy Cowdery
##' Modified by Mike Dietze for FATES BCI example

## ----- setup: these are not necessary but make testing easier ----- ##
rm(list = setdiff(ls(), lsf.str()))  # clear environment except for sourced functions
# rm(list= ls()[!(ls() %in% c('objects you want to save'))] # clear environment except for ...
for (i in dbListConnections(PostgreSQL())) db.close(i) #close any stray database connections
options(digits = 10) # just to make things easier to read
## ----- setup: these are not necessary but make testing easier ----- ##


## connect to database
bety <- PEcAn.DB::betyConnect("web/config.php")


# ----- Pick a settings file -----#
settings.file <- "/fs/data2/output//PEcAn_1000002914/pecan.CONFIGS.xml"
settings <- PEcAn.settings::read.settings(settings.file)

## *** set up the benchmarks *** 
## To do this you need to first register the input data with the database
## which may also require registering a format
## 1) Load data onto server
## 2) Create Files record for data (machine, path, filename)
## 3) Create Inputs record for data (name, format, site), save ID
input_id <- 1000011170
## 4) Edit Input to associate File
## 5) Verify that PEcAn is able to find and load file
input <- PEcAn.DB::query.file.path(input_id,host_name = "localhost",con = bety$con)
format <- PEcAn.DB::query.format.vars(bety,input_id)
field <- PEcAn.benchmark::load_data(input,format)
## 6) Look up variable_id in database
## 7) Look up metric_id (web interface in progress, for now run:
metrics <- tbl(bety,"metrics")
View(metrics)
## define benchmark in settings
settings$benchmarking <- list(
  benchmark=list(                            
  input_id = input_id,
  variable_id = 419,
  site_id = as.numeric(settings$run$site$id),
  metrics = 1000000004
  ),
  ensemble_id = as.numeric(settings$ensemble$ensemble.id),
  info = settings$info
)

## Brown dog conversion test (part of #5)
## remember to enter your own username and password!
output_path <- getwd()
key <- BrownDog::get_key("https://bd-api.ncsa.illinois.edu",username,password)
token <- BrownDog::get_token("https://bd-api.ncsa.illinois.edu",key)
foo <- BrownDog::convert_file("https://bd-api.ncsa.illinois.edu", input, output_path, "csv", token,wait=900)

bm.settings <- define_benchmark(bm.settings = settings$benchmarking,bety)

# For testing (make sure new_run is FALSE)
str(bm.settings)

# This is a quick fix - can be solved with longer db query that I don't want to write now
add_workflow_info <- function(settings){
  if (is.MultiSettings(settings)) {
    return(papply(settings, add_workflow_id))
  }
  if(!as.logical(settings$benchmarking$new_run)){
    settings$workflow$id <- tbl(bety,"ensembles") %>% 
      filter(id == settings$benchmarking$ensemble_id) %>% 
      select(workflow_id) %>% collect %>% .[[1]]
    wf <- tbl(bety, 'workflows') %>% filter(id == settings$workflow$id) %>% collect()
    settings$rundir <- file.path(wf$folder, "run")
    settings$modeloutdir <- file.path(wf$folder, "out")
    settings$outdir <- wf$folder
  }
  return(settings)
}

settings <- add_workflow_info(settings)

bm_settings2pecan_settings <- function(bm.settings){
  if (is.MultiSettings(bm.settings)) {
    return(papply(bm.settings, bm_settings2pecan_settings))
  }
  return(append(bm.settings["reference_run_id"],
                bm.settings$benchmark[which(names(bm.settings$benchmark) == "benchmark_id")]))
} 

settings$benchmarking <- bm_settings2pecan_settings(bm.settings)

if(bm.settings$new_run){
  write.settings(settings,pecan.xml,outputdir = settings$outdir)
  # Run the workflow! YAY
  settings <- read.settings(file.path(settings$outdir,"pecan.CHECKED.xml"))
  results <- load(file.path(settings$outdir,"benchmarking.output.Rdata"))
  
}else{
  
  settings <- read_settings_BRR(settings)
  settings <- prepare.settings(settings)
  results <- papply(settings, function(x) calc_benchmark(x, bety))
}
  
# 
# # This may just be for testing or something that will ultimately be used with Shiny
# rmarkdown::render(system.file("scripts/Benchmarking.Report.Rmd", package = "PEcAn.benchmark"), 
#                   params = list(file.path = file.path(settings$outdir,"benchmarking.output.Rdata")),
#                   output_dir = settings$outdir)




str(results)
