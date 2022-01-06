## Workflow assumes that you have already run
## settings <- read.settings("path_to_settings")
#> settings <- read.settings("/fs/data2/output/PEcAn_1000008263/pecan.BENCH.xml")


library(PEcAn.all)
library(PEcAn.benchmark)
library(lubridate)


con <- PEcan.DB::db.open(settings$database$bety)
settings$host$name <- "localhost"

bm.settings <- define_benchmark(settings, con)

# For testing (make sure new_run is FALSE)
str(bm.settings)



settings <- add_workflow_info(settings)
settings$benchmarking <- bm_settings2pecan_settings(bm.settings)

################################################################################

if(bm.settings$new_run){

  # This section isn't ready yet
  # write.settings(settings = settings, outputfile = "pecan.xml", outputdir = settings$outdir)
  # # Run the workflow! YAY
  # settings <- read.settings(file.path(settings$outdir,"pecan.CHECKED.xml"))
  # results <- load(file.path(settings$outdir,"benchmarking.output.Rdata"))

}else{

  settings <- read_settings_BRR(settings)
  sprintf("MODEL: %s", settings$model$type)

  settings <- prepare.settings(settings)
  results <- papply(settings, function(x) calc_benchmark(x, con))
}
