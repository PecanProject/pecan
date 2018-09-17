## Function to Create and execute pecan xml
create_exec_test_xml <- function(run_list){
  library(PEcAn.DB)
  library(dplyr)
  library(PEcAn.utils)
  library(XML)
  library(PEcAn.settings)
  model_id <- run_list[[1]]
  met <- run_list[[2]]
  site_id<- run_list[[3]]
  start_date<- run_list[[4]]
  end_date<- run_list[[5]]
  pecan_path<- run_list[[6]]
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
                      met,site_id,"_test_runs",
                      sep="",collapse =NULL)
  outdir <-  paste0(outdir_base,outdir_pre)
  dir.create(outdir)
  settings$outdir <- outdir
  #Database BETY
  settings$database$bety$user <- config.list$db_bety_username
  settings$database$bety$password <- config.list$db_bety_password
  settings$database$bety$host <- config.list$db_bety_hostname
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
  settings$ensemble$size <- 1
  settings$ensemble$variable <- "GPP"
  settings$ensemble$samplingspace$met$method <- "sampling"
  settings$ensemble$samplingspace$parameters$method <- "uniform"
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
  settings$host$name <-config.list$db_bety_hostname
  
  #create file and Run
  saveXML(listToXml(settings, "pecan"), file=paste0(outdir,"/","pecan.xml"))
  file.copy(paste0(config.list$pecan_home,"web/","workflow.R"),to = outdir)
  setwd(outdir)
  ##Name log file
  log <- file("workflow.Rout", open = "wt")
  sink(log)
  sink(log, type = "message")
  source("workflow.R")
  sink()
}




##Create Run Args
pecan_path <- "/fs/data3/tonygard/work/pecan"
config.list <- PEcAn.utils::read_web_config(paste0(pecan_path,"/web/config.php"))
bety <- betyConnect(paste0(pecan_path,"/web/config.php"))
con <- bety$con

## Find name of Machine R is running on
mach_name <- Sys.info()[[4]]
mach_id <- tbl(bety, "machines")%>% filter(grepl(mach_name,hostname)) %>% pull(id)
  
model_ids <- tbl(bety, "dbfiles") %>% filter(machine_id == mach_id) %>% 
              filter(container_type == "Model") %>% pull(container_id)



models <- model_ids
met_name <- c("CRUNCEP","AmerifluxLBL")
startdate<-"2004/01/01"
enddate<-"2004/12/31"

## Find Sites
## Site with no inputs from any machines that is part of Ameriflux site group and Fluxnet Site group
site_id_noinput<- anti_join(tbl(bety, "sites"),tbl(bety, "inputs")) %>%
  inner_join(tbl(bety, "sitegroups_sites")
             %>% filter(sitegroup_id == 1),
             by = c("id" = "site_id")) %>%
  dplyr::select("id.x", "notes", "sitename") %>%
  dplyr::filter(grepl("TOWER_BEGAN", notes))  %>% collect()  %>%
  dplyr::mutate(
    start_year = stringi::stri_extract_first_regex(notes, "[0-9]+"),
    end_year = if_else(
      stringi::stri_extract_last_regex(notes, "[0-9]+") == start_year,
      as.character(lubridate::year(Sys.Date())),
      stringi::stri_extract_last_regex(notes, "[0-9]+")
    ),
    contains_run = if_else(
      between(lubridate::year(startdate), start_year, end_year),
      "TRUE",
      "FALSE"
    ),
    len = as.integer(end_year) - as.integer(start_year)
  ) %>%
  filter(contains_run == TRUE) %>%
  filter(str_length(end_year) == 4) %>%
  filter(len == max(len)) %>%
  select("id.x")


site_id <- "772"

#Create permutations of arg combinations
options(scipen = 999)
run_table <- expand.grid(models,met_name,site_id, startdate,enddate,pecan_path,stringsAsFactors = FALSE)
#Execute function to spit out a table with a clomn of NA or success

tab <-run_table %>% mutate(outcome = purrr::pmap(.,purrr::possibly(function(...){
                                                      create_exec_test_xml(list(...))
                                                   },otherwise =NA))
                          )

