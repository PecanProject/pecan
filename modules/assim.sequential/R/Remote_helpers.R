#' Obs.data.prepare.MultiSite
#'
#' @param obs.path Path to the obs data which is expected to be an .Rdata.
#' @param site.ids a character vector of site ids which need to be extracted.
#'
#' @return a list of observed mean and cov as the SDA expected it to be.
#' @export
Obs.data.prepare.MultiSite <- function(obs.path, site.ids) {
  #Load the .Rdata file
  load(obs.path)
  
  #--------------------------------------------------------------------------------
  #for multi site both mean and cov needs to be a list like this
  # +date
  #   +siteid
  #     c(state variables)/matrix(cov state variables)
  #
  #Filter all the obs just for the sites we are simulating
  point_list$median_AGB <-
    point_list$median_AGB[[1]] %>% dplyr::filter(.data$Site_ID %in% site.ids)
  point_list$stdv_AGB  <-
    point_list$stdv_AGB[[1]] %>% dplyr::filter(.data$Site_ID %in% site.ids)
  
  #Finding the orders
  site.order <-
    sapply(site.ids, function(x)
      which(point_list$median_AGB$Site_ID %in% x))  %>% unlist() %>% as.numeric() %>% stats::na.omit()
  #Reordering
  point_list$median_AGB <- point_list$median_AGB[site.order, ]
  point_list$stdv_AGB <- point_list$stdv_AGB[site.order, ]
  
  # truning lists to dfs  for both mean and cov
  date.obs <-
    strsplit(names(point_list$median_AGB), "_")[3:length(point_list$median_AGB)] %>%
    purrr::map_chr( ~ .x[2]) %>% paste0(., "/12/31")
  
  #Making in a format that we need
  obs.mean <-
    names(point_list$median_AGB)[3:length(point_list$median_AGB)] %>%
    purrr::map(function(namesl) {
      ((point_list$median_AGB)[[namesl]] %>%
         purrr::map( ~ .x %>% as.data.frame %>% `colnames<-`(c('AbvGrndWood'))) %>%
         stats::setNames(site.ids[1:length(.)])
      )
    }) %>% stats::setNames(date.obs)
  
  
  
  obs.cov <-
    names(point_list$stdv_AGB)[3:length(point_list$median_AGB)] %>%
    purrr::map(function(namesl) {
      ((point_list$stdv_AGB)[[namesl]] %>%
         purrr::map(~ (.x) ^ 2 %>% as.matrix()) %>%
         stats::setNames(site.ids[1:length(.)]))
      
    }) %>% stats::setNames(date.obs)
  
  
  
  return(list(obs.mean = obs.mean,
              obs.cov = obs.cov))
}



#' SDA_remote_launcher
#'
#' @param settingPath The Path to the setting that will run SDA
#' @param ObsPath  Path to the obs data which is expected to be an .Rdata.
#'
#' @export
#' @return This function returns a list of two pieces of information. One the remote path that SDA is running and the PID of the active run.
#' @examples 
#' \dontrun{
#'  # This example can be found under inst folder in the package
#'  library(PEcAn.all)
#'  library(purrr)
#'  
#'  run.bash.args <- c(
#'   "#$ -l h_rt=48:00:00",
#'   "#$ -pe omp 28 # Request a parallel environment with 4 cores",
#'   "#$ -l mem_per_core=1G # and 4G memory for each",
#'   "#$ -l buyin",
#'   "module load R/3.5.2",
#'   "module load python/2.7.13"
#'   )
#'  settingPath <-"pecan.SDA.4sites.xml"
#'
#'  ObsPath <- "Obs/LandTrendr_AGB_output50s.RData"
#'    
#'  SDA_remote_launcher(settingPath, ObsPath, run.bash.args)
#'}
#'
SDA_remote_launcher <-function(settingPath, 
                               ObsPath,
                               run.bash.args){
  
  future::plan(future::multisession)
  #---------------------------------------------------------------
  # Reading the settings
  #---------------------------------------------------------------
  settings <- PEcAn.settings::read.settings(settingPath)
  my_host <- list(name =settings$host$name , tunnel = settings$host$tunnel, user=settings$host$user)
  local_path <-settings$outdir
  if (is.null(run.bash.args)) run.bash.args <-""
  #---------------------------------------------------------------
  # Checking the setting xml
  #---------------------------------------------------------------
  if (is.null(settings$host$folder)) {
    PEcAn.logger::logger.severe("You need to specify the <folder> tag in the <host> tag inside your pecan xml !")
    PEcAn.logger::logger.severe("The <folder> tag is a path which points to where you want to store/run your sda job on the remote machine. ")
  } else if (!PEcAn.remote::test_remote(my_host)) {
    PEcAn.logger::logger.severe("There is something wrong with your tunnel !")
    PEcAn.logger::logger.severe("You can learn more about how to setup your tunnel by checking out the `Remote execution with PEcAn` section in the documentation.")
    
  } 
  #----------------------------------------------------------------
  # SDA xml tag
  #---------------------------------------------------------------
  if (is.null(settings$state.data.assimilation)) {
    PEcAn.logger::logger.severe("Make sure that you have the state data assimilation tag in your xml ! You can learn more about it in the documantation.")
  }
  
  #---------------------------------------------------------------
  # Creating a new folder
  #---------------------------------------------------------------

  fname_p1 <- basename(settings$outdir)

  
  if (!is.null(settings$workflow$id)) {
    fname_p2<-settings$workflow$id
  } else {
    fname_p2<-""
      }
  
  folder_name<-"SDA"
  folder_name <- paste0(c("SDA",fname_p1,fname_p2), collapse = "_")
  #creating a folder on remote
  out <- PEcAn.remote::remote.execute.R(script=paste0("dir.create(\"/",settings$host$folder,"//",folder_name,"\")"),
                         host = my_host,
                         user = my_host$user,
                         scratchdir = ".")
  #---------------------------------------------------------------
  # samples/PFT
  #---------------------------------------------------------------
  # test to see samples.Rdata
  if ("samples.Rdata" %in% list.files(settings$outdir)){
    PEcAn.remote::remote.copy.to(
      my_host,
      paste0(settings$outdir,"//","samples.Rdata"),
      paste0(settings$host$folder,"//",folder_name),
      delete = FALSE,
      stderr = FALSE
    )
  } else if("pft" %in% list.dirs(settings$outdir, full.names=F)) {#  test for PFT folder
    PEcAn.remote::remote.copy.to(
      my_host,
      paste0(settings$outdir,"//pft"),
      paste0(settings$host$folder,"//",folder_name,"//pft"),
      delete = FALSE,
      stderr = FALSE
    )
    
    # change the pft folder inside the setting


    settings$pfts %>%
      purrr::map('outdir') %>%
      purrr::walk(function(pft.dir) {
        settings <<-
          rapply(settings, function(x)
            ifelse(
              x == pft.dir,
              file.path(settings$host$folder,
                        folder_name, "pft") ,
              x
            ),
            how = "replace")
      })
    

    
  } else {
    #
    
    PEcAn.logger::logger.severe("You need to have either PFT folder or samples.Rdata !")
  }
  #----------------------------------------------------------------
  # Obs
  #---------------------------------------------------------------
  # testing the obs path and copying over
  # testing to see if the path exsits on remote if not it should exist on local
  test.remote.obs <- PEcAn.remote::remote.execute.R(
    script = paste0("dir.exists(\"/", ObsPath, "\")"),
    host = my_host,
    user = my_host$user,
    scratchdir = "."
  )
  
  # if path is not remote then check for the local
  if (!test.remote.obs) {
    if (file.exists(ObsPath)) {
      PEcAn.remote::remote.copy.to(
        my_host,
        ObsPath,
        paste0(settings$host$folder, "//", folder_name, "//Obs//"),
        delete = FALSE,
        stderr = FALSE
      )
    } else{
      PEcAn.logger::logger.severe("I don't have access to your obs path !")
    }
  }
  
  #----------------------------------------------------------------
  # Model binary check
  #---------------------------------------------------------------

  model.binary.path <- PEcAn.remote::remote.execute.R(
    script = paste0("file.exists(\"/", settings$model$binary, "\")"),
    host = my_host,
    user = my_host$user,
    scratchdir = "."
  )
  if(!model.binary.path) {
    PEcAn.logger::logger.severe("Model binary path is missing on the remote machine !")
  }
  #----------------------------------------------------------------
  # met check
  #---------------------------------------------------------------
  # Finding all the met paths in your settings
  if (PEcAn.settings::is.MultiSettings(settings)){
    input.paths <-settings$run %>% purrr::map(~.x[['inputs']] %>% purrr::map(~.x[['path']])) %>% unlist()
  } else {
    input.paths <-settings$run$inputs %>% purrr::map(~.x[['path']]) %>% unlist()
  }

  # see if we can find those mets on remote
  missing.inputs <- input.paths %>%
    purrr::map_lgl(function(.x) {
    out <- PEcAn.remote::remote.execute.R(
      script = paste0("file.exists(\"/", .x, "\")"),
      host = my_host,
      user = my_host$user,
      scratchdir = "."
    )
    out
  }) %>%
    unlist()
  
  # if there some missing inputs, lets create a folder and transfer them
  if (!any(missing.inputs)){
    #creating a folder on remote
    out <-PEcAn.remote::remote.execute.R(script=paste0("dir.create(\"/",settings$host$folder,"//",folder_name,"//inputs","\")"),
                           host = my_host,
                           user = my_host$user,
                           scratchdir = ".")
  }
  
  # Do the Rsync to copy all the main dir of the inputs
  need.copy <- input.paths[!missing.inputs]
  
  need.copy.dirs <- dirname(need.copy) %>%
    unique() %>%
    purrr::discard(~ .x %in% c("."))
  
  
  need.copy.dirs %>%
    purrr::walk( ~   #copy over
                   PEcAn.remote::remote.copy.to(
                     my_host,
                     .x,
                     file.path(settings$host$folder, folder_name, "inputs"),
                     delete = FALSE,
                     stderr = FALSE
                   ))

  

  need.copy%>%
    furrr::future_map(function(missing.input){

       tryCatch({
         
         PEcAn.logger::logger.info(paste0("Trying modify the path to the following missing input :", missing.input))
        
          path.break <- strsplit(missing.input, "/")[[1]]
          #since I'm keeping all the inputs in one folder, I have to combine site folder name with file name
          fdir <-path.break[length(path.break) - 1]
          fdir <- ifelse(length(fdir)==0, "", fdir)
          fname <-path.break[length(path.break)]
          

          #replace the path
          settings <<-
            rapply(settings, function(x)
              ifelse(
                x == missing.input,
                file.path(settings$host$folder,
                          folder_name, "inputs", fdir, fname) ,
                x
              ),
              how = "replace")
          
          
        },
        error = function(e) {
          PEcAn.logger::logger.warn("Your input file was missing on the remote, so I tried to copy it over but an error happend.")
          PEcAn.logger::logger.severe(conditionMessage(e))
        }
      )

    })
  #----------------------------------------------------------------
  # Cleaning up the settings and getting it ready
  #---------------------------------------------------------------
  #Create the scratch dir
  remote_settings <- settings
  out <-PEcAn.remote::remote.execute.R(script=paste0("dir.create(\"/",settings$host$folder,"//",folder_name,"//scratch","\")"),
                         host = my_host,
                         user = my_host$user,
                         scratchdir = ".")
  
  remote_settings$outdir <- file.path(settings$host$folder, folder_name)
  
  remote_settings$host$name <- "localhost"
  #setting the new run and out dirs
  remote_settings$host$rundir <- file.path(settings$host$folder, folder_name,"run")
  remote_settings$host$outdir <- file.path(settings$host$folder, folder_name,"out")
  remote_settings$rundir <- file.path(settings$host$folder, folder_name,"run")
  remote_settings$modeloutdir <- file.path(settings$host$folder, folder_name,"out")
  
  
  remote_settings$scratchdir <- file.path(settings$host$folder, folder_name,"scratch")
  
  save.setting.dir <- tempdir()
  PEcAn.settings::write.settings(remote_settings, basename(settingPath), save.setting.dir)
  
  # copying over the settings
  PEcAn.remote::remote.copy.to(
    my_host,
    file.path(save.setting.dir, basename(settingPath)),
    file.path(settings$host$folder, folder_name),
    delete = FALSE,
    stderr = FALSE
  )
  #----------------------------------------------------------------
  # Copying over the luncher and sending the command
  #---------------------------------------------------------------
  # copying over the luncher
  PEcAn.remote::remote.copy.to(
    my_host,
    system.file("RemoteLauncher", "SDA_launcher.R", package = "PEcAnAssimSequential"),
    file.path(settings$host$folder,folder_name),
    delete = FALSE,
    stderr = FALSE
  )
  
  cmd <- paste0("Rscript ",
                remote_settings$outdir,"/SDA_launcher.R ", # remote luncher
                remote_settings$outdir, "/" ,basename(settingPath), # path to settings
                " Obs//", basename(ObsPath)
  )
  


   PEcAn.logger::logger.info("Running this command on your remote: \n")
   PEcAn.logger::logger.info(cmd)
  
   #create the bash file
   bashfile<-readLines(system.file("RemoteLauncher", "Run.bash", package = "PEcAnAssimSequential"))
   tmpdir <- tempdir()
   unlink(paste0(tmpdir,"/Run.bash")) # delete if there is already one exists
   writeLines(c(bashfile, run.bash.args, cmd), paste0(tmpdir, "/Run.bash"))
   #copy over the bash file
   PEcAn.remote::remote.copy.to(
     my_host,
     paste0(tmpdir,"/Run.bash"),
     paste0(settings$host$folder, "/", folder_name, "/RunBash.sh"),
     delete = FALSE,
     stderr = FALSE
   )
   
   
# I'm tricking the remote::start_qsub to let me submit my job
   out.job.id <- PEcAn.remote::start_qsub(
                                     run = "",
                                     qsub_string = settings$host$qsub,
                                     rundir = NULL,
                                     host = settings$host,
                                     host_rundir = paste0(settings$host$folder, "/", folder_name),
                                     host_outdir = "",
                                     stdout_log = "",
                                     stderr_log = "",
                                     job_script = "RunBash.sh"
                                   )

   # Let's see what is the job id of the job doing
   out.job.id<-PEcAn.remote::qsub_get_jobid(out = out.job.id[length(out.job.id)], qsub.jobid = settings$host$qsub.jobid, stop.on.error = stop.on.error)
   
   if (length(out.job.id)==0 | is.null(out.job.id)){
     PEcAn.logger::logger.severe("Something broke the run before it starts!")
     PEcAn.logger::logger.severe(paste0("In order to get a better sense of what happened you can check out ",settings$outdir,"/log.qlog"))
   }

   #This where you can find your SDA
   return(list(Remote.Path = settings$outdir,
               PID = out.job.id))
  
}

#' Remote_Sync_launcher
#'
#' @param settingPath Path to your local setting .
#' @param remote.path Path generated by SDA_remote_launcher which shows the path to your remote SDA run.
#' @param PID PID generated by SDA_remote_launcher which shows the active PID running your SDA job.
#'
#' @export
Remote_Sync_launcher <- function(settingPath, remote.path, PID) {
  
  settings <- PEcAn.settings::read.settings(settingPath)
  
  system(paste0("nohup Rscript ",
                system.file("RemoteLauncher", "Remote_sync.R", package = "PEcAnAssimSequential")," ",
                settingPath, " ", 
                remote.path, " ",
                PID,
                " > ",
                settings$outdir,"/SDA_remote_report.out 2>&1 &"))
}




#' alltocs
#'
#' @param fname string path to where the output needs to be saved as a csv file.
#'
#' @description This function finds all the tic functions called before and estimates the time elapsed for each one saves/appends it to a csv file.
#'
#' @return This function writes down a csv file with three columns: 1- message sepecified in the `tic` 2- Total elapsed time and 3- the execution time
#' @export 
#'
#' 
#' @examples  
#' \dontrun{
#'  library(tictoc)
#'  tic("Analysis")
#'  Sys.sleep(5)
#'  testfunc()
#'  tic("Adjustment")
#'  Sys.sleep(4)
#'  alltocs("timing.csv")  
#'}
alltocs <-function(fname="tocs.csv") {
  # Finding all the tics being resigsterd
  tryCatch({
    get(".Data",
        get(".tictoc", envir = baseenv())) %>%
      seq_along() %>%
      purrr::map_dfr(function(x) {
        s <- tictoc::toc(quiet = T, log = T)
        dfout <- data.frame(
          Task = s$msg %>%  as.character(),
          TimeElapsed = round(s$toc - s$tic, 1),
          stringsAsFactors = F
        )
        return(dfout)
      }) %>%
      dplyr::mutate(ExecutionTimeP = c(min(.data$TimeElapsed), diff(.data$TimeElapsed))) %>%
      utils::write.table(
        file = fname,
        append = T,
        row.names = F,
        col.names = F
      )
  },
  error = function(e) {
    PEcAn.logger::logger.warn("Something happened with the profiling !")
  })

}
