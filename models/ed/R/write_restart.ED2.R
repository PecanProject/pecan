#' @title Write ED2 restart file from SDA results
#' 
#' @author Alexey Shiklomanov, Istem Fer
#' @inheritParams PEcAn.ModelName::write_restart.ModelName
#' @return TRUE if successful
#' @export
write_restart.ED2 <- function(outdir, runid, start.time, stop.time,
                              settings, new.state, new.params, inputs, RENAME = TRUE) {
  
  restart <- new.params$restart
  
  old.state <- restart$oldstate # hack: this will probably change in the near future, it's currently just AbvGrndWood 
  histfile  <- restart$histfile # Get history restart file path
  restart   <- restart$restart
  
  # remote or not remote?
  # rundir <- settings$host$rundir
  # mod_outdir <- settings$host$outdir 
  rundir <- settings$rundir
  mod_outdir <- settings$modeloutdir # same as outdir?
  
  sda_datestr <- strftime(start.time, "%Y-%m-%d-%H%M%S", tz = "UTC")
  sda_suffix <- paste0("SDA.", sda_datestr)


  # check these dirs for local vs remote: ARE WE GONNA NEED THIS?
  #### Backup old run files to date directory
  runfiles <- list.files.nodir(file.path(rundir, runid))
  modoutfiles <- list.files.nodir(file.path(mod_outdir, runid))
  other_files <- grep("history-.*.h5", modoutfiles, value = TRUE, invert = TRUE)
  copy_files <- c(basename(histfile), other_files)

  dir.create(file.path(rundir, runid, sda_suffix))
  dir.create(file.path(mod_outdir, runid, sda_suffix))
  file.copy(file.path(rundir, runid, runfiles),
            file.path(rundir, runid, sda_suffix, runfiles), 
            overwrite = TRUE)
  file.copy(file.path(mod_outdir, runid, copy_files),
            file.path(mod_outdir, runid, sda_suffix, copy_files),
            overwrite = TRUE)



  # #### Identify PFTs
  # # This assumes that PFT order is the same between pecan.xml and ED's 
  # # config.xml.  A better solution would set the PFT numbers in the 
  # # pecan.xml, or names in config.xml.
  # pftnums <- sapply(confxml, '[[', 'num')
  # pftnames <- sapply(settings$pfts, '[[', 'name')
  # names(pftnames) <- pftnums
  # names(pftnums) <- pftnames
  # 
  # 
  # nc <- ncdf4::nc_open(histfile)
  # 
  #### Get common variables
  # PFT by cohort
  pft_co <- restart$PFT 
   
  # Patch area
  patch_area <- restart$AREA 
   
  #### Create a patch index indicator vector
  paco_n      <- restart$PACO_N  # number of cohorts per patch
  patch_index <- rep(1:length(paco_n), times = paco_n)

  # 
  # ncdf4::nc_close(nc)
  # 
  # varname_regex <- '(^[^.]*)\\.([^.]*)\\.(.*)$'
  # var.names <- unique(gsub(varname_regex, "\\1", names(new.state)))
  # 
  # old.state <- read.restart.ED2(outdir = outdir,
  #                               runid = runid,
  #                               stop.time = start.time,
  #                               settings = settings,
  #                               var.names = var.names,
  #                               params = NULL) ## TODO: new.params???

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
      agb_co_ratios <- new.tmp / old.tmp # not ideal, just trying to get the workflow to run

      # AbvGrndWood in state matrix is not per PFT but total
      # but leaving this bit as a reminder
      #
      # new2old.agb_pft <- new.agb_pft / old.agb_pft
      # new2old_pftnames <- gsub(paste0(var_name, ".pft."), '', 
      #                          names(new2old.agb_pft))
      # names(new2old.agb_pft) <- as.character(pftnums[new2old_pftnames])
      # agb_co_ratios <- new2old.agb_pft[as.character(pft_co)]

      nplant_co_plant <- restart$NPLANT

      # The only AGB-related state variables read by ED's history restart 
      # subroutine are BDEAD, DBH, and NPLANT. The remaining states 
      # (including cohort and patch-level AGB) are recalculated from 
      # these variables.
      #
      # Here, we adjust cohort-level AGB by adjusting the stand density 
      # (NPLANT) proportional to the change in biomass computed above.
      new.nplant_co_plant <- nplant_co_plant * agb_co_ratios[1,1]
      # An alternative is to modify DBH and BDEAD, which requires solving 
      # the following allometric equation for DBH and then using ED 
      # allometric equations to recalculate BDEAD.
      # 
      # AGB = b1L*DBH^(b2L) * (1 + qsw * agf_bs * 
      #     (h0 + a * (1-exp(b*DBH))) + b1d*DBH^(b2d)

      #### Write new state to file
      # Default mode of H5File$new is "a", which is read + write and create file if it doesn't exist
      histfile_h5 <- hdf5r::H5File$new(histfile)
      # The empty brackets (`[]`) indicate the whole vector is replaced.
      # This is necessary to overwrite an existing dataset
      histfile_h5[["NPLANT"]][] <- new.nplant_co_plant
      # This closes the file and all objects related to the file.
      histfile_h5$close_all()
    } else {
      PEcAn.logger::logger.error("Variable ", var_name,
                                " not currently supported",
                                " by write.restart.ED2")
    }
  }

  ##### Modify ED2IN
  ed2in_path <- file.path(rundir, runid, "ED2IN")
  ed2in_orig <- read_ed2in(ed2in_path)


  ed2in_new <- modify_ed2in(
    ed2in_orig,
    start_date = start.time,
    end_date = stop.time,
    RUNTYPE = "HISTORY",
    IED_INIT_MODE = 4,
    SFILIN = file.path(settings$host$outdir, runid, "history")
  )
  
  # rename hisoty file (IF: At this point in order not to swamp directories I decided to write annual historu files
  # but this causes mismatches in start-date because ED2 writes them as 1961-01-01 not 1961-12-31
  # then if timeh starts from 1962 it can't find the 1961 files, if you give dates accordingly it starts the simulation early
  # my solution is to copy-rename the history file. Other solutions are to change ED2's naming, or writing daily/monthly history files
  index    <- gregexpr("-S-", histfile)[[1]]
  tstamp   <- substr(histfile, index[1] + 3, index[1] + 6)
  from_histfile <- file.path(settings$host$outdir, runid, basename(histfile))
  to_histfile   <- file.path(settings$host$outdir, runid, gsub(tstamp, lubridate::year(start.time), basename(histfile)))
  
  if(settings$host$name == "localhost") check_ed2in(ed2in_new)
  write_ed2in(ed2in_new, ed2in_path)

  # Remove old history.xml file, which job.sh looks for
  file.remove(file.path(mod_outdir, runid, "history.xml"))  # this is local
  # have job.sh delete the old history.xml: this is temporary, this script will eventually run on remote
  jobsh <- readLines(file.path(rundir, runid, "job.sh"),-1)
  jobsh[17] <- paste0("rm -f ", file.path(settings$host$outdir, runid, "history.xml"))
  jobsh[18] <- paste("cp", from_histfile, to_histfile)
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

