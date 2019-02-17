#' Obs.data.prepare.MultiSite
#'
#' @param obs.path Path to the obs data which is expected to be an .Rdata.
#' @param site.ids a character vector of site ids which need to be extracted.
#'
#' @return a list of observed mean and cov as the SDA expected it to be.
#' @export
Obs.data.prepare.MultiSite <- function(obs.path="../Obs/LandTrendr_AGB_output50s.RData", site.ids) {
  #Load the .Rdata file
  load(obs.path)
  
  point_list$median_AGB[[1]] <- point_list$median_AGB[[1]] %>%
    filter(Site_ID != '1000000074')
  
  
  point_list$stdv_AGB[[1]] <- point_list$stdv_AGB[[1]] %>%
    filter(Site_ID != '1000000074')
  
  #--------------------------------------------------------------------------------
  #for multi site both mean and cov needs to be a list like this
  # +date
  #   +siteid
  #     c(state variables)/matrix(cov state variables)
  #
  #Filter all the obs just for the sites we are simulating
  point_list$median_AGB <-
    point_list$median_AGB[[1]] %>% filter(Site_ID %in% site.ids)
  point_list$stdv_AGB  <-
    point_list$stdv_AGB[[1]] %>% filter(Site_ID %in% site.ids)
  
  #Finding the orders
  site.order <-
    sapply(site.ids, function(x)
      which(point_list$median_AGB$Site_ID %in% x)) %>%
    as.numeric() %>% na.omit()
  
  #Reordering
  point_list$median_AGB <- point_list$median_AGB[site.order, ]
  point_list$stdv_AGB <- point_list$stdv_AGB[site.order, ]
  
  # truning lists to dfs  for both mean and cov
  date.obs <-
    strsplit(names(point_list$median_AGB), "_")[3:length(point_list$median_AGB)] %>%
    map_chr( ~ .x[2]) %>% paste0(., "/12/31")
  
  #Making in a format that we need
  obs.mean <-
    names(point_list$median_AGB)[3:length(point_list$median_AGB)] %>%
    map(function(namesl) {
      ((point_list$median_AGB)[[namesl]] %>%
         map( ~ .x %>% as.data.frame %>% `colnames<-`(c('AbvGrndWood'))) %>%
         setNames(site.ids[1:length(.)])
      )
    }) %>% setNames(date.obs)
  
  
  
  obs.cov <-
    names(point_list$stdv_AGB)[3:length(point_list$median_AGB)] %>%
    map(function(namesl) {
      ((point_list$stdv_AGB)[[namesl]] %>%
         map(~ (.x) ^ 2 %>% as.matrix()) %>%
         setNames(site.ids[1:length(.)]))
      
    }) %>% setNames(date.obs)
  
  
  
  return(list(obs.mean = obs.mean,
              obs.cov = obs.cov))
}



#' SDA_remote_launcher
#'
#' @param settingPath The Path to the setting that will run SDA
#' @param ObsPath  Path to the obs data which is expected to be an .Rdata.
#'
#' @export
#' @example 
#' \dontrun{
#'   library(PEcAn.all)
#'  library(purrr)
#'  library(PEcAn.assim.sequential)
#'  
#'  settingPath <-
#'    "/fs/data3/hamzed/Projects/GeoTunnel/RemoteSDA/pecan.SDA.4sites.xml"
#'  ObsPath <-
#'    "/fs/data3/hamzed/Projects/GEF_MultiSite/Obs/LandTrendr_AGB_output50s.RData"
#'  
#'  
#'  SDA_remote_launcher(settingPath, ObsPath)
#'}
#'
SDA_remote_launcher <-function(settingPath, 
                               ObsPath){
  
  #---------------------------------------------------------------
  # Reading the settings
  #---------------------------------------------------------------
  settings <- read.settings(settingPath)
  my_host <- list(name =settings$host$name , tunnel = settings$host$tunnel, user=settings$host$user)
  local_path <-settings$outdir
  #---------------------------------------------------------------
  # Cheking the setting xml
  #---------------------------------------------------------------
  if (is.null(settings$host$folder)) {
    PEcAn.logger::logger.severe("You need to specify the folder tag in the host inside your pecan xml !")
  } else if (!test_remote(my_host)) {
    PEcAn.logger::logger.severe("There is something wrong with your tunnel !")
  } 
  #---------------------------------------------------------------
  # Creating a new folder
  #---------------------------------------------------------------
  folder_name <- as.numeric(Sys.time())
  #creating a folder on remote
  out <-remote.execute.R(script=paste0("dir.create(\"/",settings$host$folder,"//",folder_name,"\")"),
                         host = my_host,
                         user = 'hamzed',
                         scratchdir = ".")
  #---------------------------------------------------------------
  # samples/PFT
  #---------------------------------------------------------------
  # test to see samples.Rdata
  if ("samples.Rdata" %in% list.files(settings$outdir)){
    remote.copy.to(
      my_host,
      paste0(settings$outdir,"//","samples.Rdata"),
      paste0(settings$host$folder,"//",folder_name),
      delete = FALSE,
      stderr = FALSE
    )
  }else if("pft" %in% list.dirs(settings$outdir, full.names=F)){#  test for PFT folder
    remote.copy.to(
      my_host,
      paste0(settings$outdir,"//pft"),
      paste0(settings$host$folder,"//",folder_name,"//pft"),
      delete = FALSE,
      stderr = FALSE
    )
  }else{
    #
    PEcAn.logger::logger.severe("You need to have either PFT folder or sample.Rdata !")
  }
  #----------------------------------------------------------------
  # Obs
  #---------------------------------------------------------------
  # testing the obs path and copying over
  if (file.exists(ObsPath)){
    remote.copy.to(
      my_host,
      ObsPath,
      paste0(settings$host$folder,"//",folder_name,"//Obs//"),
      delete = FALSE,
      stderr = FALSE
    )
  }else{
    PEcAn.logger::logger.severe("I can't access to your obs path !")
  }
  #----------------------------------------------------------------
  # met check
  #---------------------------------------------------------------
  # testing the met folders and copying them over
  met.paths <-settings %>% map(~.x[['run']] ) %>% map('inputs') %>% map('met')%>% map('path') %>% unlist()
  
  met.test <- met.paths %>% map_lgl(function(.x) {
    out <- remote.execute.R(
      script = paste0("file.exists(\"/", .x, "\")"),
      host = my_host,
      user = 'hamzed',
      scratchdir = "."
    )
    out
  }) %>%
    unlist()
  
  if (!all(met.test)) {
    PEcAn.logger::logger.severe("At least one of the mets specified in your pecan xml was not found on the remote machine")
    #TODO: At some point I could also copy over the ones that there not in the remote and edit the xml accrodingly.
  }
  #----------------------------------------------------------------
  # Site- PFT file 
  #---------------------------------------------------------------
  site.pft.paths <- settings %>% map(~.x[['run']] ) %>% map('inputs') %>% map('pft.site') %>% map('path') %>% unlist %>% unique()
  
  site.pft.paths %>%
    map(function(pft.path){
      if (file.exists(file.path(settings$outdir,pft.path))){
        remote.copy.to(
          my_host,
          file.path(settings$outdir,pft.path),
          paste0(settings$host$folder,"//",folder_name,"//"),
          delete = FALSE,
          stderr = FALSE
        )
      }else{
        PEcAn.logger::logger.severe("I can't access to your site.pft file path !")
      }
    })
  
  
  #----------------------------------------------------------------
  # Cleaning up the settings and getting it ready
  #---------------------------------------------------------------
  settings$outdir <- paste0(settings$host$folder,"/", folder_name)
  
  settings$host$name <- "localhost"
  #setting the new run and out dirs
  settings$host$rundir <- paste0(settings$host$folder,"/", folder_name,"//run")
  settings$host$outdir <- paste0(settings$host$folder,"/", folder_name,"//out")
  settings$rundir <- paste0(settings$host$folder,"/", folder_name,"//run")
  settings$modeloutdir  <-paste0(settings$host$folder,"/", folder_name,"//out")
  
  save.setting.dir <- tempdir()
  PEcAn.settings::write.settings(settings, basename(settingPath), save.setting.dir)
  
  # copying over the settings
  remote.copy.to(
    my_host,
    file.path(save.setting.dir, basename(settingPath)),
    settings$outdir,
    delete = FALSE,
    stderr = FALSE
  )
  #----------------------------------------------------------------
  # Copying over the luncher and sending the command
  #---------------------------------------------------------------
  # copying over the luncher
  remote.copy.to(
    my_host,
    system.file("RemoteLauncher", "Remote_SDA_launcher.R", package = "PEcAn.assim.sequential"),
    settings$outdir,
    delete = FALSE,
    stderr = FALSE
  )
  
  cmd <- paste0("nohup  Rscript ",
                settings$outdir,"//Remote_SDA_launcher.R ", # remote luncher
                settings$outdir,"//",basename(settingPath), # path to settings
                " Obs//", basename(ObsPath), # Path to Obs
                " > ", 
                settings$outdir,"//SDA_nohup.out 2>&1 &"
  )
  


   PEcAn.logger::logger.info("Running this command on your remote: \n")
   PEcAn.logger::logger.info(cmd)
  
   #calling SDA
   out<-remote.execute.R(paste0("system(\" ",cmd, "\")"),
                         my_host,
                         user = 'hamzed',
                         scratchdir = ".")

   # Let's see what is the PID of the job doing the nohup
   # I'll use this to track the progress of my SDA job
   PIDS<-remote.execute.cmd(my_host, cmd = "lsof",
                      args = c(paste0(settings$outdir,"//SDA_nohup.out")))
   
   if (length(PIDS)>1){
     #some cleaning
     PID<-PIDS[-1] %>% 
       map_dbl(function(line){
         ll <- strsplit(line, " ")[[1]]
         ll <- ll[nchar(ll)>0]
         ll[2] %>% 
           as.numeric()
       }) %>%
       unique()
   }else{
     PEcAn.logger::logger.severe("Something broke the run before it starts!")
     #TODO: read the nohup.out if it exists
   }


   #This where you can find your SDA
   return(list(Remote.Path = settings$outdir,
               PID = PID))
  
}


