#' @title Write ED2 restart file from SDA results
#' 
#' @author Alexey Shiklomanov, Istem Fer
#' @inheritParams PEcAn.ModelName::write_restart.ModelName
#' @return TRUE if successful
#' @export
write_restart.ED2 <- function(outdir, runid, start.time, stop.time,
                              settings, new.state, RENAME = TRUE, new.params, inputs) {
  
  restart <- new.params$restart
  
  # IMPORTANT NOTE: in the future, things that are passed via "restart" list need to be confined to old states that will be used
  # to carry out deternimistic relationships, no other read/write restart should copy this logic
  old.state <- restart$oldstate # hack: this will probably change in the near future, it's currently just AbvGrndWood 
  histfile  <- restart$histfile # Get history restart file path
  restart   <- restart$restart
  
  # remote or not remote?
  # rundir <- settings$host$rundir
  # mod_outdir <- settings$host$outdir 
  rundir <- settings$rundir
  mod_outdir <- settings$modeloutdir # same as outdir?
  
  sda_datestr  <- gregexpr("-S-", histfile)[[1]]
  sda_suffix   <- paste0("SDA.", substr(histfile, sda_datestr[1] + 3, sda_datestr[1] + 19))
  hyear        <- substr(histfile, sda_datestr[1] + 3, sda_datestr[1] + 6)
  
  # check these dirs for local vs remote
  #### Backup old run files to date directory
  runfiles <- list.files.nodir(file.path(rundir, runid))
  modoutfiles <- list.files.nodir(file.path(mod_outdir, runid), hyear) # copy only current year, otherwise it cumulatively copies everything
  
  dir.create(file.path(rundir, runid, sda_suffix))
  dir.create(file.path(mod_outdir, runid, sda_suffix))
  file.copy(file.path(rundir, runid, runfiles),
            file.path(rundir, runid, sda_suffix, runfiles), 
            overwrite = TRUE)
  file.copy(file.path(mod_outdir, runid, modoutfiles),
            file.path(mod_outdir, runid, sda_suffix, modoutfiles),
            overwrite = TRUE)
  


  # rename history file 
  # (IF: At this point in order not to swamp directories I decided to write annual historu files
  # but this causes mismatches in start-date because ED2 writes them as 1961-01-01 not 1961-12-31
  # then if timeh starts from 1962 it can't find the 1961 files, if you give dates accordingly it starts the simulation early
  # my solution is to copy-rename the history file. Other solutions are to change ED2's naming, or writing daily/monthly history files
  new_basename <- gsub(paste0(hyear, "-12"), paste0(lubridate::year(stop.time),"-01"), basename(histfile))
  new_local_histfile   <- file.path(dirname(histfile), new_basename)
  new_remote_histfile  <- file.path(settings$host$outdir, runid, new_basename)
  
  file.copy(histfile, new_local_histfile, overwrite = TRUE)
  
  #### Get common variables
  # PFT by cohort
  pft_co <- restart$PFT 
   
  # Patch area
  patch_area <- restart$AREA 
   
  #### Create a patch index indicator vector
  paco_n      <- restart$PACO_N  # number of cohorts per patch
  patch_index <- rep(1:length(paco_n), times = paco_n)

  # read xml to extract allometric coeffs later
  configfile <- file.path(rundir, runid,"config.xml")
  pars <- XML::xmlToList(XML::xmlParse(configfile))
  # remove non-pft sublists
  pars[names(pars)!="pft"] <- NULL
  
  #### Write new state to file
  # Default mode of H5File$new is "a", which is read + write and create file if it doesn't exist
  histfile_h5 <- hdf5r::H5File$new(new_local_histfile)
  
  for (var_name in var.names) {
    # var_name <- "AbvGrndWood"
    if (var_name == "AbvGrndWood") {

      #### Disaggregate AbvGrndWood down to cohort vector
      # NOTE: This is currently naive -- it just calculates the 
      # AbvGrndWood ratio between the old and new states and applies it to each 
      # cohort based on its PFT. No patch information is involved because 
      # none is provided in `new.state`.

      new.tmp <- new.state[grep(var_name, names(new.state))]
      old.tmp <- old.state
      agw_ratios <- new.tmp / old.tmp # not ideal, just trying to get the workflow to run
      
      bdead    <- restart$BDEAD
      bstorage <- restart$BSTORAGE
      
      new_bdead    <- bdead * agw_ratios[1,1]
      
      # if you're nudging bdead, update bstorage and dbh too
      new_bstorage <- bstorage * agw_ratios[1,1]
      
      pft_nums <- as.numeric(sapply(pars,`[[`, "num"))
      # use ED2's allometric eqns to dtermine dbh from new bdead
      C2B <- 2
      new_dbh <- new_bdead
      for(pn in seq_along(pft_nums)){
        ind <- pft == pft_nums[pn]
          
        crit <- new_bdead[ind] <= pars[[pn]]$bdead_crit
        new_dbh[ind][crit] = (new_bdead[ind][crit] / as.numeric(pars[[pn]]$b1Bs_small) * C2B)**(1.0/ as.numeric(pars[[pn]]$b2Bs_small))
        new_dbh[ind][!crit] = (new_bdead[ind][!crit] / as.numeric(pars[[pn]]$b1Bs_large) * C2B)**(1.0/as.numeric(pars[[pn]]$b2Bs_large))
        
      }

      # AbvGrndWood in state matrix is not per PFT but total
      # but leaving this bit as a reminder
      
      # new2old.agb_pft <- new.agb_pft / old.agb_pft
      # new2old_pftnames <- gsub(paste0(var_name, ".pft."), '', 
      #                          names(new2old.agb_pft))
      # names(new2old.agb_pft) <- as.character(pftnums[new2old_pftnames])
      # agb_co_ratios <- new2old.agb_pft[as.character(pft_co)]

      #nplant_co_plant <- restart$NPLANT


      # Here, we adjust cohort-level AGB by adjusting the stand density 
      # (NPLANT) proportional to the change in biomass computed above.
      #new.nplant_co_plant <- nplant_co_plant * agb_co_ratios[1,1]
      # An alternative is to modify DBH and BDEAD, which requires solving 
      # the following allometric equation for DBH and then using ED 
      # allometric equations to recalculate BDEAD.
      # 
      # AGB = b1L*DBH^(b2L) * (1 + qsw * agf_bs * 
      #     (h0 + a * (1-exp(b*DBH))) + b1d*DBH^(b2d)


      # The empty brackets (`[]`) indicate the whole vector is replaced.
      # This is necessary to overwrite an existing dataset
      #histfile_h5[["NPLANT"]][] <- new.nplant_co_plant
      histfile_h5[["BDEAD"]][] <- new_bdead
      histfile_h5[["BSTORAGE"]][] <- new_bstorage
      histfile_h5[["DBH"]][] <- new_dbh
      
      # overwrite the keeper you're reading in read.restart
      histfile_h5[["TOTAL_AGB"]][] <- udunits2::ud.convert(new.tmp, "Mg/ha/yr", "kg/m^2/yr")
      
    } else if(var_name == "GWBI"){
      
      # zero cumulative rate keepers, nothing is calculated back from these
      # so zeroing only the rate you're reading back is fine
      histfile_h5[["TOTAL_AGB_GROWTH"]][] <- 0
      #zero both
      histfile_h5[["DDBH_DT"]][] <- rep(0, length(restart$DDBH_DT))
      histfile_h5[["DAGB_DT"]][] <- rep(0, length(restart$DAGB_DT))
      
    } else {
      PEcAn.logger::logger.error("Variable ", var_name,
                                " not currently supported",
                                " by write.restart.ED2")
    }
  }
  
  # This closes the file and all objects related to the file.
  histfile_h5$close_all()
  
  # copy the history file with new states and new timestamp to remote 
  # it's OK, because ED2 doesn't overwrite the history files and produces history-Z- files anyway
  PEcAn.remote::remote.copy.to(settings$host, new_local_histfile, new_remote_histfile)

  ##### Modify ED2IN
  ed2in_path <- file.path(rundir, runid, "ED2IN")
  ed2in_orig <- read_ed2in(ed2in_path)


  ed2in_new <- modify_ed2in(
    ed2in_orig,
    start_date = lubridate::ceiling_date(start.time, "1 day"),
    end_date = lubridate::floor_date(stop.time, "30 minutes"), # floor down to the last half hour so that ED2 doesn't write to next year's file
    RUNTYPE = "HISTORY",
    IED_INIT_MODE = 4,
    SFILIN = file.path(settings$host$outdir, runid, "history")
  )
  

  if(settings$host$name == "localhost") check_ed2in(ed2in_new)
  write_ed2in(ed2in_new, ed2in_path)

  # Remove old history.xml file, which job.sh looks for
  file.remove(file.path(mod_outdir, runid, "history.xml"))  # this is local
  
  # read the jobsh in the rundir
  jobsh <- readLines(file.path(rundir, runid, "job.sh"),-1)
  remote_remove_cmd <- paste0("rm -f ", file.path(settings$host$outdir, runid, "history.xml"))
  jobsh[grep("@REMOVE_HISTXML@", jobsh)+1] <- remote_remove_cmd
  
  # also update mode2netcdf.ED2 call
  mod2cf_line        <- grep("model2netcdf.ED2", jobsh)
  mod2cf_string      <- jobsh[mod2cf_line]
  from_year          <- paste0("'", hyear,"/")  # trying to make sure year is not somewhere else in the path
  to_year            <- paste0("'", lubridate::year(start.time), "/")
  mod2cf_string      <- gsub(from_year, to_year, mod2cf_string)
  jobsh[mod2cf_line] <- mod2cf_string
  
  writeLines(jobsh, file.path(rundir, runid, "job.sh"))

  PEcAn.logger::logger.info("Finished --", runid)
  
  return(TRUE)
} # write_restart.ED2

# Phony dims:
#   0 - All cohorts
#   1 - Sites 
#   2 - Size classes (11)
#   3 - PFTs (17)
#   4 - Patches
#   5 - Months?? 
#   6 - ??? 
#   7 - Something related to height
#   8 - Something to do with disturbance
#   9 - Soil layers (?)
#   10 - Something related to mortality
#   11 - Canopy radiation profile

