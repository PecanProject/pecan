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
input_id <- 1000011171
## 4) Edit Input to associate File
## 5) Verify that PEcAn is able to find and load file
input <- PEcAn.DB::query.file.path(input_id,host_name = "localhost",con = bety)
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
  variable_id = 1000000132,
  site_id = as.numeric(settings$run$site$id),
  metrics = c(1000000004,1000000001,1000000009,1000000010,1000000011)
  ),
  ensemble_id = as.numeric(settings$ensemble$ensemble.id),
  info = settings$info
)

## Brown dog conversion test (part of #5)
## remember to enter your own username and password!
output_path <- getwd()
key <- BrownDog::get_key("https://bd-api.ncsa.illinois.edu",username,password)
token <- BrownDog::get_token("https://bd-api.ncsa.illinois.edu",key)
foo <- BrownDog::convert_file("https://bd-api.ncsa.illinois.edu", input,"csv", output_path, token,wait=900)

bm.settings <- define_benchmark(bm.settings = settings$benchmarking,bety)
bm.settings$new_run=FALSE

## Now that the Benchmark is setup, verify that benchmark metrics can be calculated
bm_settings2pecan_settings <- function(bm.settings){
  if (PEcAn.settings::is.MultiSettings(bm.settings)) {
    return(papply(bm.settings, bm_settings2pecan_settings))
  }
  return(append(bm.settings["reference_run_id"],
                bm.settings$benchmark[which(names(bm.settings$benchmark) == "benchmark_id")]))
} 

new.settings <- settings
new.settings$benchmarking <- bm_settings2pecan_settings(bm.settings)

if(bm.settings$new_run){
  write.settings(new.settings,pecan.xml,outputdir = settings$outdir)
  # Run the workflow! YAY
  settings <- read.settings(file.path(new.settings$outdir,"pecan.CHECKED.xml"))
  results <- load(file.path(new.settings$outdir,"benchmarking.output.Rdata"))
  
}else{
  
  new.settings <- read_settings_BRR(new.settings)
  new.settings <- PEcAn.settings::prepare.settings(new.settings)
  results <- papply(new.settings, function(x) calc_benchmark(x, bety))
}
str(results)
