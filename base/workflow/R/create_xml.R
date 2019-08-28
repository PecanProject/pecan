##' @export
##' @aliases create_xml
##' @name create_xml
##' @title create_xml
##' @description Function to create a viable PEcAn xml from a table containing specifications of a run
##' @param run_list PEcAn table containing specifications of runs
##' @param overwrite.met,overwrite.fia,overwrite.ic logical
##'
##' @author Tony Gardella

create_execute_test_xml <- function(run_list){
  
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
  
  config.list <-PEcAn.utils::read_web_config(paste0(pecan_path,"/web/config.php"))
  bety <- PEcAn.DB::betyConnect(paste0(pecan_path,"/web/config.php"))
  con <- bety$con
  settings <- list()
  
  # Info
  settings$info$notes <- paste0("Test_Run")
  settings$info$userid <- user_id
  settings$info$username <- "None"
  settings$info$dates <- Sys.Date()
  #Outdir
  model.new <- dplr::tbl(bety, "models") %>% dplyr::filter(model_id == id) %>% dplyr::collect()
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
    pft <- dplyr::tbl(bety, "pfts") %>% dplyr::collect() %>% dplyr::filter(modeltype_id == model.new$modeltype_id)
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
  #Workflow
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
  XML::saveXML(PEcAn.settings::listToXml(settings, "pecan"), file=paste0(outdir,"/","pecan.xml"))
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
