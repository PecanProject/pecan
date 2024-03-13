## Forward-only automation
library("PEcAn.all")

## parse start date
option_list = list(optparse::make_option("--start.date",
                      default = Sys.Date(),
                      type="character"))
args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
#args$start.date = "2021-09-25"
start.date = lubridate::as_date(args$start.date)
  
#### grab & update default settings ####
set = readRDS("/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/pecan.RDS")
if(FALSE){
  ## one time updates
  projectdir = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03"
  set$outdir = projectdir
  set$ensemble$size = 100  ## increase ensemble size
  set$pfts[[1]]$posterior.files = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/pfts/PFT_Files_new/temperate/post.distns.Rdata"
  set$pfts[[2]]$posterior.files = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/pfts/PFT_Files_new/conifer/post.distns.Rdata"
  set$pfts[[3]]$posterior.files = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/pfts/PFT_Files_new/grass/post.distns.Rdata"
  set$pfts[[4]]$posterior.files = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/pfts/trait.mcmc.pda.soil.HPDA_som_respiration_rate_correction.Rdata"
  ## add soil pft to all sites
  soil.pft = grep(pattern = "soil",x=unlist(sapply(set$pfts,function(x){x$name})))
  for(i in seq_along(set)){
    set[[i]]$run$site$site.pft[[2]] = set$pfts[[soil.pft]]$name
    names(set[[i]]$run$site$site.pft)[2] = "pft.name"
  }
  ## initial conditions
  for(i in seq_along(set)){
    set[[i]]$run$inputs$poolinitcond = list()
    set[[i]]$run$inputs$poolinitcond$path = paste0("/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/IC/IC_site_1-269",40+i,"_1.nc")
    set[[i]]$run$inputs$pft.site = file.path(projectdir,"site_pft.csv")
  }
  saveRDS(set,file = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/pecan.RDS")
}

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
set$model$jobtemplate = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/template.job"


#set <- PEcAn.settings::prepare.settings(set, force = FALSE)



## check PFTs
pft.names = unlist(sapply(set$pfts,function(x){x$name}))
set$pfts[[which(is.na(pft.names))]] = NULL

## run workflow
set <- PEcAn.workflow::runModule.run.write.configs(set,use.existing.samples=TRUE)
PEcAn.workflow::runModule_start_model_runs(set, stop.on.error = FALSE)

## future work
## * Integrate in Phyllis's changes to be able to save and reuse previous ensemble draws
## * restart from previous forecast
## not sure why need to delete last line from met to get things to run.

## manual hack to recover runs.txt
library(stringr)
runs = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test03/FOF2021-09-25/run/runs.txt"
runset = paste0("ENS-",str_pad(rep(1:100,times = 10), 5, pad = "0"),"-10000269",rep(41:50,each=100))
write(runset,file=runs)
