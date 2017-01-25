
settings <- read.settings("/fs/data3/ecowdery/FACE/DUKE_AMB_ED.xml")

d <- settings$database$bety[c("dbname", "password", "host", "user")]
bety <- src_postgres(host = d$host, user = d$user, password = d$password, dbname = d$dbname)
settings$host$name <- "localhost"

bm.settings <- define_benchmark(settings,bety)

# For testing (make sure new_run is FALSE)
str(bm.settings)

# This is a quick fix - can be solved with longer db query that I don't want to write now
add_workflow_info <- funct3ion(settings){
  if (is.MultiSettings(settings)) {
    return(papply(settings, add_workflow_id))
  }
  if(!as.logical(settings$benchmarking$new_run)){
    settings$workflow$id <- tbl(bety,"ensembles") %>% 
      filter(id == settings$benchmarking$ensemble_id) %>% 
      dplyr::select(workflow_id) %>% collect %>% .[[1]]
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
  out <- bm.settings["reference_run_id"]
  for(i in grep("benchmark", names(bm.settings))){
    print(bm.settings[i]$benchmark$benchmark_id)
    out <- append(out, list(benchmark_id = bm.settings[i]$benchmark$benchmark_id))
  }
  return(out)
} 

settings$benchmarking <- bm_settings2pecan_settings(bm.settings)

############################################################################################################

sprintf("MODEL: %s", settings$model$type)


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
