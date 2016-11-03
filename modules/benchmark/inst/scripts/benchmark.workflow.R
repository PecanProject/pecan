##' Benchmarking workflow
##' Only use this if using and existing run (ie settings$new_run == FALSE)
##' Author: Betsy Cowdery

## ----- setup: these are not necessary but make testing easier ----- ##
rm(list = setdiff(ls(), lsf.str()))  # clear environment except for sourced functions
# rm(list= ls()[!(ls() %in% c('objects you want to save'))] # clear environment except for ...
for (i in dbListConnections(PostgreSQL())) db.close(i) #close any stray database connections
options(digits = 10) # just to make things easier to read
## ----- setup: these are not necessary but make testing easier ----- ##

library(PEcAn.all)
library(PEcAn.visualization) # Really shouldn't need this but I haven't reconsiled this and PEcAn.DB

bety <- betyConnect("web/config.php")

# Start with a benchmarking settings xml file that would be created though the interface 

# ----- Pick a settings file -----#
# WORKING: 1 variable, 1 metric, 1 site, 1 model 
settings.file <- "modules/benchmark/inst/scripts/bm.1var.1metric.1site.1model.xml"

# Test all the metrics
settings.file <- "modules/benchmark/inst/scripts/bm.1var.ALLmetric.1site.1model.xml"


# WORKING: 2 variables, 2 metric, 1 site, 1 model 
# settings.file <- "modules/benchmark/inst/scripts/bm.2var.2metric.1site.1model.xml"

# NOT WORKING: 2 variables, 2 metric, 2 site, 1 model (multisettings object)
settings.file <- "modules/benchmark/inst/scripts/bm.2var.2metric.2site.1model.xml"

# Ryan's multisite as an example
# settings.file <- "/fs/data2/rykelly/mandifore/testruns/028_mandSE_test_08_n2/pecan.xml"
# --------------------------------#

settings <- read.settings(settings.file)

# For testing (make sure new_run is FALSE)
settings$benchmark

settings <- papply(settings, read.settings.RR)
settings <- do.conversions(settings)
settings <- papply(settings, function(x) create.benchmark(x, bety))
results <- papply(settings, function(x) calc.benchmark(x, bety))

rmarkdown::render("modules/benchmark/inst/scripts/Benchmarking.Report.Rmd",
                  params = list(file.path = "/fs/data2/output//PEcAn_1000002773/benchmarking.output.Rdata"),output_format = "pdf_document")
# 
# # This may just be for testing or something that will ultimately be used with Shiny
# rmarkdown::render(system.file("scripts/Benchmarking.Report.Rmd", package = "PEcAn.benchmark"), 
#                   params = list(file.path = file.path(settings$outdir,"benchmarking.output.Rdata")),
#                   output_dir = settings$outdir)

str(results)
