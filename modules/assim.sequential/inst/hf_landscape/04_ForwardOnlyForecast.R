## Forward-only automation
library("PEcAn.all")

## parse start date
option_list = list(optparse::make_option("--start.date",
                      default = Sys.Date(),
                      type="character"))
args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
#args$start.date = "2022-05-17"
start.date = lubridate::as_date(args$start.date)
  
#### grab & update default settings ####
set = readRDS("/projectnb/dietzelab/dietze/hf_landscape_SDA/test02/pecan.RDS")
##start.date
for(s in seq_along(set)){
  set[[s]]$run$start.date = start.date
  set[[s]]$run$end.date   = start.date + lubridate::days(35)
}
## Find GEFS
con <-PEcAn.DB::db.open(set$database$bety)
input_check <- PEcAn.DB::dbfile.input.check(
  siteid = 646, #NEON 1000004945, #EMS 758,
  startdate = as.character(start.date),
  enddate = NULL,
  parentid = NA,
  mimetype="text/csv",
  formatname="Sipnet.climna",
  con = con,
  hostname = PEcAn.remote::fqdn(),
  pattern = NULL, 
  exact.dates = TRUE,
  return.all=TRUE
)
metList = as.list(file.path(input_check$file_path,input_check$file_name))
names(metList) = rep("path",length(metList))
## met path
for(s in seq_along(set)){
 set[[s]]$run$inputs$met$source = "GEFS" 
 set[[s]]$run$inputs$met$path = metList
}
## outdirs
set$outdir = file.path(set$outdir,paste0("FOF",start.date))
set$rundir = file.path(set$outdir,"run")
set$modeloutdir = file.path(set$outdir,"out")
set$pfts$pft$outdir = file.path(set$outdir,"pft")
dir.create(set$outdir)
dir.create(set$rundir)
dir.create(set$modeloutdir)
dir.create(set$pfts$pft$outdir)
## job.sh
set$model$jobtemplate = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test02/template.job"


set <- PEcAn.settings::prepare.settings(set, force = FALSE)

## add soil pft to all sites
soil.pft = grep(pattern = "soil",x=unlist(sapply(set$pfts,function(x){x$name})))
for(i in seq_along(set)){
  set$run[[i]]$site$site.pft[[2]] = set$pfts[[soil.pft]]$name
  names(set$run[[i]]$site$site.pft)[2] = "pft.name"
}

## check PFTs
pft.names = unlist(sapply(set$pfts,function(x){x$name}))
set$pfts[[which(is.na(pft.names))]] = NULL

## run workflow
set <- PEcAn.workflow::runModule.run.write.configs(set)
PEcAn.workflow::runModule_start_model_runs(set, stop.on.error = FALSE)

## future work
## * Integrate in Phyllis's changes to be able to save and reuse previous ensemble draws
## * restart from previous forecast
## not sure why need to delete last line from met to get things to run.