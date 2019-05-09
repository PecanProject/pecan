## This script contains the Following parts:
## Part 1 - Write Main Function
## A. takes in a list defining specifications for a single run and assigns them to objects
## B. The function then writes out a pecan settings list object
## C. A directory is created from the ouput folder defined in a users config.php file
## D. Settings object gets written into pecan.xml file and put in output directory.
## E. workflow.R is copied into this directory and executed.
## F. console output is stored in a file called workflow.Rout
## Part 2 - Write run settings
## A. Set BETY connection object. Needs user to define path to their pecan directory.
## B. Find Machine ID
## C. Find Models ids based on machine
## D. Manually Define Met, right now CRUNCEP and AMerifluxlbl
## E. Manually Define start and end date
## F. Manually Define Output var
## G. Manually Define Sensitivity and enesmble 
## H. Find sites not associated with any inputs(meaning data needs to be downloaded), part of ameriflux network, and have data for start-end year range.
## Available ameriflux data is found by parsing the notes section of the sites table where ameriflux sites have a year range.
## Part 3 - Create Run table
## A. Create table that contains combinations of models,met_name,site_id, startdate, enddate, pecan_path,out.var, ensemble, ens_size, sensitivity args that the function above will use to do runs.
## Part 4 - Run the Function across the table 
## A. Use the pmap function to apply the function across each row of arguments and append a pass or fail outcome column to the original table of runs
## Part 5 - (In progress) Turn output table into a table 

create_execute_test_xml <- function(run_list){
  library(PEcAn.DB)
  library(dplyr)
  library(PEcAn.utils)
  library(XML)
  library(PEcAn.settings)
  
  #Read in Table 
  model_id <- run_list[[1]]
  met <- run_list[[2]]
  site_id<- run_list[[3]]
  start_date<- run_list[[4]]
  end_date<- run_list[[5]]
  pecan_path<- run_list[[6]]
  out.var<- run_list[[7]]
  ensemble<- run_list[[8]]
  ens_size<- run_list[[9]]
  sensitivity<- run_list[[10]]
  user_id<- NA
  pft_name<- NA
  
  config.list <-read_web_config(paste0(pecan_path,"/web/config.php"))
  bety <- betyConnect(paste0(pecan_path,"/web/config.php"))
  con <- bety$con
  settings <- list()
  
  # Info
  settings$info$notes <- paste0("Test_Run")
  settings$info$userid <- user_id
  settings$info$username <- "None"
  settings$info$dates <- Sys.Date()
  #Outdir
  model.new <- tbl(bety, "models") %>% filter(model_id == id) %>% collect()
  outdir_base<-config.list$output_folder
  outdir_pre <- paste(model.new$model_name,format(as.Date(start_date), "%Y-%m"),
                      format(as.Date(end_date), "%Y-%m"),
                      met,site_id,"test_runs",
                      sep="_",collapse =NULL)
  outdir <-  paste0(outdir_base,outdir_pre)
  dir.create(outdir)
  settings$outdir <- outdir
  #Database BETY
  settings$database$bety$user <- config.list$db_bety_username
  settings$database$bety$password <- config.list$db_bety_password
  settings$database$bety$host <- "localhost"
  settings$database$bety$dbname <- config.list$db_bety_database
  settings$database$bety$driver <- "PostgreSQL"
  settings$database$bety$write <- FALSE
  #Database dbfiles
  settings$database$dbfiles <- config.list$dbfiles_folder
  #PFT
  if (is.na(pft_name)){
    pft <- tbl(bety, "pfts") %>% filter(modeltype_id == model.new$modeltype_id) %>% collect()
    pft_name <- pft$name[1]
  }
  settings$pfts$pft$name <- pft_name
  settings$pfts$pft$constants$num <- 1
  #Meta Analysis
  settings$meta.analysis$iter <- 3000
  settings$meta.analysis$random.effects <- FALSE
  #Ensemble
  if(ensemble){
    settings$ensemble$size <- ens_size
    settings$ensemble$variable <- out.var
    settings$ensemble$samplingspace$met$method <- "sampling"
    settings$ensemble$samplingspace$parameters$method <- "uniform"
  }else{
    settings$ensemble$size <- 1
    settings$ensemble$variable <- out.var
    settings$ensemble$samplingspace$met$method <- "sampling"
    settings$ensemble$samplingspace$parameters$method <- "uniform"
  }
  #Sensitivity
  if(sensitivity){
    settings$sensitivity.analysis$quantiles <-
      settings$sensitivity.analysis$quantiles$sigma1 <--2
    settings$sensitivity.analysis$quantiles$sigma2 <--1
    settings$sensitivity.analysis$quantiles$sigma3 <- 1
    settings$sensitivity.analysis$quantiles$sigma4 <- 2
    names(settings$sensitivity.analysis$quantiles) <-c("sigma","sigma","sigma","sigma")
  }
  #Model
  settings$model$id <- model.new$id
  #Worflow
  settings$workflow$id <- paste0("Test_run_","_",model.new$model_name)
  settings$run$site$id <- site_id
  settings$run$site$met.start <- start_date
  settings$run$site$met.end <- end_date
  settings$run$inputs$met$source <- met
  settings$run$inputs$met$output <- model.new$model_name
  settings$run$inputs$met$username <- "pecan" 
  settings$run$start.date <- start_date
  settings$run$end.date <- end_date
  settings$host$name <-"localhost"
  
  #create file and Run
  saveXML(listToXml(settings, "pecan"), file=paste0(outdir,"/","pecan.xml"))
  file.copy(paste0(config.list$pecan_home,"web/","workflow.R"),to = outdir)
  setwd(outdir)
  ##Name log file
  #log <- file("workflow.Rout", open = "wt")
  #sink(log)
  #sink(log, type = "message")
  
  system("./workflow.R 2>&1 | tee workflow.Rout")
  #source("workflow.R")
  #sink()
}

##Create Run Args

## Insert your path to base pecan
pecan_path <- "/fs/data3/tonygard/work/pecan"
config.list <- PEcAn.utils::read_web_config(paste0(pecan_path,"/web/config.php"))
bety <- PEcAn.DB::betyConnect(paste0(pecan_path,"/web/config.php"))
con <- bety$con
library(tidyverse)

## Find name of Machine R is running on
mach_name <- Sys.info()[[4]]
mach_id <- tbl(bety, "machines")%>% filter(grepl(mach_name,hostname)) %>% pull(id)

## Find Models
#devtools::install_github("pecanproject/pecan", subdir = "api")
model_ids <- tbl(bety, "dbfiles") %>% 
  filter(machine_id == mach_id) %>% 
  filter(container_type == "Model") %>%
  pull(container_id)



models <- model_ids
met_name <- c("CRUNCEP","AmerifluxLBL")
startdate<-"2004/01/01"
enddate<-"2004/12/31"
out.var <- "NPP"
ensemble <- FALSE
ens_size <- 100
sensitivity <- FALSE
## Find Sites
## Site with no inputs from any machines that is part of Ameriflux site group and Fluxnet Site group
site_id_noinput<- anti_join(tbl(bety, "sites"),tbl(bety, "inputs")) %>%
  inner_join(tbl(bety, "sitegroups_sites") %>% 
               filter(sitegroup_id == 1),
             by = c("id" = "site_id")) %>%
  dplyr::select("id.x", "notes", "sitename") %>%
  dplyr::filter(grepl("TOWER_BEGAN", notes)) %>% 
  collect() %>%
  dplyr::mutate(
    # Grab years from string within the notes
    start_year = substring(stringr::str_extract(notes,pattern = ("(?<=TOWER_BEGAN = ).*(?=  TOWER_END)")),1,4),
    #Empty tower end in the notes means that it goes until present day so if empty enter curent year.
    end_year = dplyr::if_else(
      substring(stringr::str_extract(notes,pattern = ("(?<=TOWER_END = ).*(?=)")),1,4) == "",
      as.character(lubridate::year(Sys.Date())),
      substring(stringr::str_extract(notes,pattern = ("(?<=TOWER_END = ).*(?=)")),1,4)
    ),
    #Check if startdate year is within the inerval of that is given
    in_date = data.table::between(as.numeric(lubridate::year(startdate)),as.numeric(start_year),as.numeric(end_year))
  ) %>%
  dplyr::filter(in_date & as.numeric(end_year) - as.numeric(start_year) > 1) %>%
  mutate(sitename= gsub(" ","_",.$sitename)) %>% rename_at(id.x = site_id)


site_id <- site_id_noinput$id.x
site_name <- gsub(" ","_",site_id_noinput$sitename)

#Create permutations of arg combinations
options(scipen = 999)
run_table <- expand.grid(models,met_name,site_id, startdate, enddate, 
                         pecan_path,out.var, ensemble, ens_size, sensitivity, stringsAsFactors = FALSE)
#Execute function to spit out a table with a column of NA or success

tab <-run_table %>% mutate(outcome = purrr::pmap(.,purrr::possibly(function(...){
  create_execute_test_xml(list(...))
},otherwise =NA))
)

## print to table
tux_tab <- huxtable::hux(tab)
html_table <- huxtable::print_html(tux_tab)
htmlTable::htmlTable(tab)
