#You must run this script in the terminal using the code:
#Rscript --vanilla EFI_workflow.R "[file path to site xml]" "[file path to output folder]" [start_date] [end_date]

library("PEcAn.all")
library("PEcAn.utils")
library("RCurl")
library("REddyProc")
library("tidyverse")
library("furrr")
library("R.utils")
library("dynutils")

###### Preping Workflow for regular SIPNET Run ##############
#set home directory as object (remember to change to your own directory before running this script)
homedir <- "/projectnb/dietzelab/ahelgeso"

#Load site.xml, start & end date, (with commandArgs specify args in terminal) and outputPath (i.e. where the model outputs will be stored) into args
tmp = commandArgs(trailingOnly = TRUE)
if(length(tmp)<3){
  logger.severe("Missing required arguments")
}
args = list()
args$settings = tmp[1]
if(!file.exists(args$settings)){
  logger.severe("Not a valid xml path")
}
args$outputPath = tmp[2]
if(!isAbsolutePath(args$outputPath)){
  logger.severe("Not a valid outputPath")
}
args$start_date = as.Date(tmp[3])
if(is.na(args$start_date)){
  logger.severe("No start date provided")
}

if(length(args)>3){
  args$end_date = as.Date(tmp[4])
} else {
  args$end_date = args$start_date + 35
}

if(length(args)>4){
  args$continue = tmp[5]
} else {
  args$continue = TRUE
}

if(!dir.exists(args$outputPath)){dir.create(args$outputPath, recursive = TRUE)}
setwd(args$outputPath)

# Open and read in settings file for PEcAn run.
settings <- PEcAn.settings::read.settings(args$settings)

start_date <- args$start_date
end_date<- args$end_date

# Finding the right end and start date
met.start <- start_date 
met.end <- met.start + lubridate::days(35)



settings$run$start.date <- as.character(met.start)
settings$run$end.date <- as.character(met.end)
settings$run$site$met.start <- as.character(met.start)
settings$run$site$met.end <- as.character(met.end)
#info
settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), " +0000")

# Update/fix/check settings.
# Will only run the first time it's called, unless force=TRUE
settings <-
  PEcAn.settings::prepare.settings(settings, force = FALSE)

# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")

#manually add in clim files 
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)

input_check <- PEcAn.DB::dbfile.input.check(
  siteid=settings$run$site$id %>% as.character(),
  startdate = settings$run$start.date %>% as.Date,
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

#If INPUTS already exists, add id and met path to settings file

if(length(input_check$id) > 0){
  #met paths 
  clim_check = list()
  for(i in 1:length(input_check$file_path)){
    
    clim_check[[i]] <- file.path(input_check$file_path[i], input_check$file_name[i])
  }#end i loop for creating file paths 
  #ids
  index_id = list()
  index_path = list()
  for(i in 1:length(input_check$id)){
    index_id[[i]] = as.character(input_check$id[i])#get ids as list
    
  }#end i loop for making lists
  names(index_id) = sprintf("id%s",seq(1:length(input_check$id))) #rename list
  names(clim_check) = sprintf("path%s",seq(1:length(input_check$id)))
  
  settings$run$inputs$met$id = index_id
  settings$run$inputs$met$path = clim_check
}else{PEcAn.utils::logger.error("No met file found")}
#settings <- PEcAn.workflow::do_conversions(settings, T, T, T)

if(is_empty(settings$run$inputs$met$path) & length(clim_check)>0){
  settings$run$inputs$met$id = index_id
  settings$run$inputs$met$path = clim_check
}
PEcAn.DB::db.close(con)

# Write out the file with updated settings
PEcAn.settings::write.settings(settings, outputfile = "pecan.GEFS.xml")

# start from scratch if no continue is passed in
status_file <- file.path(settings$outdir, "STATUS")
if (args$continue && file.exists(status_file)) {
  file.remove(status_file)
}

# Do conversions
#settings <- PEcAn.workflow::do_conversions(settings)

# Write model specific configs
if (PEcAn.utils::status.check("CONFIG") == 0) {
  PEcAn.utils::status.start("CONFIG")
  settings <-
    PEcAn.workflow::runModule.run.write.configs(settings)
  
  PEcAn.settings::write.settings(settings, outputfile = "pecan.CONFIGS.xml")
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, "pecan.CONFIGS.xml"))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, "pecan.CONFIGS.xml"))
}

if ((length(which(commandArgs() == "--advanced")) != 0)
    && (PEcAn.utils::status.check("ADVANCED") == 0)) {
  PEcAn.utils::status.start("ADVANCED")
  q()
}

# Start ecosystem model runs
if (PEcAn.utils::status.check("MODEL") == 0) {
  PEcAn.utils::status.start("MODEL")
  stop_on_error <- as.logical(settings[[c("run", "stop_on_error")]])
  if (length(stop_on_error) == 0) {
    # If we're doing an ensemble run, don't stop. If only a single run, we
    # should be stopping.
    if (is.null(settings[["ensemble"]]) ||
        as.numeric(settings[[c("ensemble", "size")]]) == 1) {
      stop_on_error <- TRUE
    } else {
      stop_on_error <- FALSE
    }
  }
  PEcAn.remote::runModule.start.model.runs(settings, stop.on.error = stop_on_error)
  PEcAn.utils::status.end()
}

# Get results of model runs
if (PEcAn.utils::status.check("OUTPUT") == 0) {
  PEcAn.utils::status.start("OUTPUT")
  runModule.get.results(settings)
  PEcAn.utils::status.end()
}

# # Run ensemble analysis on model output.
# if ("ensemble" %in% names(settings)
#     && PEcAn.utils::status.check("ENSEMBLE") == 0) {
#   PEcAn.utils::status.start("ENSEMBLE")
#   runModule.run.ensemble.analysis(settings, TRUE)
#   PEcAn.utils::status.end()
# }

# Run state data assimilation
if ("state.data.assimilation" %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    settings <- sda.enfk(settings)
    PEcAn.utils::status.end()
  }
}

# Pecan workflow complete
if (PEcAn.utils::status.check("FINISHED") == 0) {
  PEcAn.utils::status.start("FINISHED")
  PEcAn.remote::kill.tunnel(settings)
  db.query(
    paste(
      "UPDATE workflows SET finished_at=NOW() WHERE id=",
      settings$workflow$id,
      "AND finished_at IS NULL"
    ),
    params = settings$database$bety
  )
  
  # Send email if configured
  if (!is.null(settings$email)
      && !is.null(settings$email$to)
      && (settings$email$to != "")) {
    sendmail(
      settings$email$from,
      settings$email$to,
      paste0("Workflow has finished executing at ", base::date()),
      paste0("You can find the results on ", settings$email$url)
    )
  }
  PEcAn.utils::status.end()
}

db.print.connections()
print("---------- PEcAn Workflow Complete ----------")


#EFI Output Configuration
library("ggplot2")
library("plotly")
library("gganimate")
library("thematic")
thematic_on()
source("/projectnb/dietzelab/ahelgeso/pecan/scripts/efi_data_process.R")
#Load Output args
site.num <- settings$run$site$id
outdir <- args$outputPath
site.name <- settings$run$site$name
wid <- settings$workflow$id

output_args = c(as.character(wid), site.num, outdir)

data = efi.data.process(output_args)

#Run SIPNET Outputs
data.final = data %>%
  mutate(date = as.Date(date)) %>%
  filter(date < end_date) %>%
  arrange(ensemble, date) %>%
  mutate(time = as.POSIXct(paste(date, Time, sep = " "), format = "%Y-%m-%d %H %M")) %>%
  mutate(siteID = site.name,
         forecast = 1,
         data_assimilation = 0,
         time = lubridate::force_tz(time, tz = "UTC"))
#re-order columns and delete unnecessary columns in data.final
datacols <- c("date", "time", "siteID", "ensemble", "nee", "le", "vswc", "forecast", "data_assimilation")
data.final = data.final[datacols]

############ Plots to check out reliability of forecast #########################

# ggplot(data.final, aes(x = time, y = nee, group = ensemble)) +
#   geom_line(aes(x = time, y = nee, color = ensemble))
#
# ggplot(data.final, aes(x = time, y = le, group = ensemble)) +
#   geom_line(aes(x = time, y = le, color = ensemble))
#
# ggplot(data.final, aes(x = time, y = vswc, group = ensemble)) +
#   geom_line(aes(x = time, y = vswc, color = ensemble))

########### Export data.final  ###############

write.csv(data.final, file = paste0(site.name, "-", start_date, "-", end_date, ".csv"))


