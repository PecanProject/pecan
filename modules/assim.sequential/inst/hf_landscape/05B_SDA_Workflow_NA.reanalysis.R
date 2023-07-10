## Reanalysis helper script around 05_SDA_Workflow_NA

###########  CONFIGURATION ###################

## original start date was 2022-05-17
runDays <- seq(as.Date("2023-07-06"), as.Date("2023-07-08"), by="days")
FORCE = FALSE  ## should we overwrite previously completed runs

## forecast configuration
projectdir = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test02/" ## main folder
set = readRDS(file.path(projectdir,"pecan.RDS"))
pecanhome = "/home/dietze/pecan"  ## directory where pecan is installed 

## S3 bucket for output
source(file.path(pecanhome,"modules/assim.sequential/inst/hf_landscape/minio_secrets.R"))
## minio_secrets contains the following:
#minio_key <- Sys.getenv("MINIO_ACCESS_KEY", "username")
#minio_secret <- Sys.getenv("MINIO_SECRET_KEY", "password")
minio_host <- Sys.getenv("MINIO_HOST", "test-pecan.bu.edu")
minio_port <- Sys.getenv("MINIO_PORT", "9000")
minio_arrow_bucket <- Sys.getenv("MINIO_ARROW_BUCKET", "hf-landscape-none")

################# Initial configuration (one time): ############################
##  * update local paths (uncomment, run once, recomment)
# set$outdir = projectdir
# for(i in seq_along(set$pfts)){
#   set$pfts[[i]]$posterior.files = file.path(projectdir,"pfts",basename(set$pfts[[i]]$posterior.files))
# }
# set$model$binary = file.path(projectdir,"model",basename(set$model$binary))
# set$model$jobtemplate = file.path(projectdir,"template.job")
# for(i in seq_along(set$run)){
#   set$run[[i]]$inputs$pft.site = file.path(projectdir,"site_pft.csv")
#   set$run[[i]]$inputs$poolinitcond$path = file.path(projectdir,"IC",basename(set$run[[i]]$inputs$poolinitcond$path))
#   set$run[[i]]$inputs$met$path = file.path(projectdir,"GEFS")
# }
# saveRDS(set,file=file.path(projectdir,"pecan.RDS"))  
##  * update set$database$bety$host
##  * set up separate cron jobs for input prep (met, constraints)

###########  RUN REANALYSIS ####################
for (s in seq_along(runDays)) {
  ## did we do this run already?
  now  = file.path(projectdir,paste0("FNA",runDays[s]))
  print(now)
  this.out = dir(file.path(now,"out"),full.names = TRUE)
  if(length(this.out) > 0 & !FORCE) next
  
  ## find previous run
  yesterday = runDays[s] - lubridate::days(1)
  NoMet = read.csv(file.path(projectdir,"NO_MET"),header=FALSE)[,1]
  while(as.character(yesterday) %in% NoMet & yesterday - runDays[s] < lubridate::days(35) ){
    yesterday = yesterday - lubridate::days(1)
  }
  prev = file.path(projectdir,paste0("FNA",yesterday,"/"))
  if(dir.exists(prev)){
    ## is there output there?
    prev.out = dir(file.path(prev,"out"),full.names = TRUE)
    if(length(prev.out)>0){
      prev.files = sapply(as.list(prev.out),function(x){length(dir(x,pattern = "*.nc"))})
      if(min(prev.files)>0){
        
        #########   RUN FORECAST   ########
        msg = system2(file.path(pecanhome,"modules/assim.sequential/inst/hf_landscape/05_SDA_Workflow_NA.R"),
                      paste("--start.date",runDays[s],
                            "--prev",prev,
                            "--settings",file.path(projectdir,"pecan.RDS")),
                      wait=TRUE,
                      stdout="stdout.log",
                      stderr="stderr.log")
        print(msg)
        
      }
    } else { break }
  } else {
    ## previous run didn't occur
    break
  }
  
}

##########################################
## push output to minio in EFI standard ##
##########################################
source(file.path(pecanhome,"modules/assim.sequential/inst/hf_landscape/PEcAn2EFI.R"))
# helper function for minio URIs
minio_path <- function(...) paste(minio_arrow_bucket, ..., sep = "/")
minio_uri <- function(...) {
  template <- "s3://%s:%s@%s?scheme=http&endpoint_override=%s%s%s"
  sprintf(template, minio_key, minio_secret, minio_path(...), minio_host, ":", minio_port)
}
minio_uri_public <- function(...) {
  template <- "s3://%s?scheme=http&endpoint_override=%s%s%s"
  sprintf(template, minio_path(...), minio_host, ":", minio_port)
}

runDays <- seq(as.Date("2023-06-22"), as.Date("2023-07-10"), by="days")

## loop over dates
FORCE = FALSE
for (s in seq_along(runDays)) {
  ## did we do this run already?
  now  = file.path(projectdir,paste0("FNA",runDays[s]))
  print(now)
  this.out = dir(file.path(now,"out"),full.names = TRUE)
  if(length(this.out) == 0){
    print("no output")
    next
  } 
  
  ## did we write this run to minio already?
  if(!FORCE){  ## if not overwriting
    ens = arrow::open_dataset(minio_uri_public(), format = "parquet" ) %>% 
      dplyr::filter(lubridate::as_datetime(reference_datetime) == runDays[s]) %>%  
      dplyr::distinct(parameter) %>% dplyr::collect()
    if(length(ens$parameter)>0) { ## already have output
      print(paste("skipping",length(ens$parameter)))
      next
    }
  }
  
  ## identify runs in the output folder
  runs     =        sapply(strsplit(this.out,"/"),function(x){x[grep("ENS",x)]})
  site_ids = unique(sapply(strsplit(runs    ,"-"),function(x){as.numeric(x[3])}))
  ens_ids  = unique(sapply(strsplit(runs    ,"-"),function(x){as.numeric(x[2])}))
  
  ## read output, convert to EFI standard
  out = list()
  for(i in seq_along(runs)){
    out[[runs[i]]] = PEcAn2EFI.ens(outdir = file.path(now,"out"),
                                   run.id = runs[i],
                                   start_date = runDays[s])
  }
  out = dplyr::bind_rows(out)
  if(!is.numeric(nrow(out)) | nrow(out) == 0) next  ## don't insert empty days into minio
  out = out %>% relocate(parameter) %>% 
    relocate(site_id) %>%
    relocate(time_bounds) %>% rename(datetime=time_bounds) %>%
    relocate(reference_datetime)
  out = tidyr::pivot_longer(out,5:ncol(out),names_to = "variable",values_to = "prediction")

  ## push to container in parquet format
  out %>% dplyr::group_by(reference_datetime) %>% arrow::write_dataset(minio_uri(),format="parquet")
  
}

## list set of uploaded dates
submitted = arrow::open_dataset(minio_uri_public(), format = "parquet" ) %>% 
  dplyr::distinct(reference_datetime) %>% dplyr::collect() %>% as.vector()
sort(submitted$reference_datetime)
