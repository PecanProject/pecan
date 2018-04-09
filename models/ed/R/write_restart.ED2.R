#' @title Write ED2 restart file from SDA results
#' 
#' @author Alexey Shiklomanov
#' @inheritParams PEcAn.ModelName::write_restart.ModelName
#' @return TRUE if successful
#' @export
write_restart.ED2 <- function(outdir,
                              runid,
                              start.time,
                              stop.time,
                              settings,
                              new.state) {

  rundir <- settings$host$rundir
  mod_outdir <- settings$host$outdir

  sda_datestr <- strftime(start.time, "%Y-%m-%d-%H%M%S")
  sda_suffix <- paste0("SDA.", sda_datestr)

  # Get history restart file path
  histfile <- get_restartfile.ED2(mod_outdir, runid, start.time)
  if (is.null(histfile)) {
    PEcAn.logger::logger.severe("Failed to find ED2 history restart file.")
  }

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

  confxml <- get_configxml.ED2(rundir, runid)

  #### Identify PFTs
  # This assumes that PFT order is the same between pecan.xml and ED's 
  # config.xml.  A better solution would set the PFT numbers in the 
  # pecan.xml, or names in config.xml.
  pftnums <- sapply(confxml, '[[', 'num')
  pftnames <- sapply(settings$pfts, '[[', 'name')
  names(pftnames) <- pftnums
  names(pftnums) <- pftnames


  nc <- ncdf4::nc_open(histfile)

  #### Read common variables
  # PFT by cohort
  pft_co <- ncdf4::ncvar_get(nc, 'PFT')

  # Patch area
  patch_area <- ncdf4::ncvar_get(nc, 'AREA')

  #### Create a patch index indicator vector
  patch_index <- patch_cohort_index(nc)

  ncdf4::nc_close(nc)

  varname_regex <- '(^[^.]*)\\.([^.]*)\\.(.*)$'
  var.names <- unique(gsub(varname_regex, "\\1", names(new.state)))

  old.state <- read.restart.ED2(outdir = outdir,
                                runid = runid,
                                stop.time = start.time,
                                settings = settings,
                                var.names = var.names,
                                params = NULL) ## TODO: new.params???

  for (var_name in var.names) {
    if (var_name == "AGB") {

      #### Disaggregate AGB down to cohort vector
      # NOTE: This is currently naive -- it just calculates the 
      # AGB ratio between the old and new states and applies it to each 
      # cohort based on its PFT. No patch information is involved because 
      # none is provided in `new.state`.

      new.agb_pft <- new.state[grep(var_name, names(new.state))]
      old.agb_pft <- old.state[grep(var_name, names(old.state))]
      new2old.agb_pft <- new.agb_pft / old.agb_pft
      new2old_pftnames <- gsub(paste0(var_name, ".pft."), '', 
                               names(new2old.agb_pft))
      names(new2old.agb_pft) <- as.character(pftnums[new2old_pftnames])

      agb_co_ratios <- new2old.agb_pft[as.character(pft_co)]

      nc <- ncdf4::nc_open(histfile)
      nplant_co_plant <- ncdf4::ncvar_get(nc, "NPLANT")
      ncdf4::nc_close(nc)

      # The only AGB-related state variables read by ED's history restart 
      # subroutine are BDEAD, DBH, and NPLANT. The remaining states 
      # (including cohort and patch-level AGB) are recalculated from 
      # these variables.
      #
      # Here, we adjust cohort-level AGB by adjusting the stand density 
      # (NPLANT) proportional to the change in biomass computed above.
      new.nplant_co_plant <- nplant_co_plant * agb_co_ratios
      # An alternative is to modify DBH and BDEAD, which requires solving 
      # the following allometric equation for DBH and then using ED 
      # allometric equations to recalculate BDEAD.
      # 
      # AGB = b1L*DBH^(b2L) * (1 + qsw * agf_bs * 
      #     (h0 + a * (1-exp(b*DBH))) + b1d*DBH^(b2d)

      #### Write new state to file
      h5_write <- rhdf5::h5write.default(new.nplant_co_plant, histfile, "NPLANT")
      # Returns NULL on success...?
    } else {
      PEcAn.logger::logger.error("Variable ", var_name,
                                " not currently supported",
                                " by write.restart.ED2")
    }
  }

  ##### Modify ED2IN
  ed2in_path <- file.path(rundir, runid, "ED2IN")
  ed2in_orig <- readLines(ed2in_path)
  ed2in_new <- ed2in_orig

  ## IED_INIT_MODE = 5 --> Run from history.h5 file
  tag_val_list <- list("RUNTYPE" = "history",
                       "IED_INIT_MODE" = 5,
                       "SFILIN" = file.path(mod_outdir, runid, "history"),
                       "IMONTHA" = strftime(start.time, "%m"), 
                       "IDATEA" = strftime(start.time, "%d"),
                       "IYEARA" = strftime(start.time, "%Y"),
                       "ITIMEA" = strftime(start.time, "%H%M"),
                       "IMONTHZ" = strftime(stop.time, "%m"),
                       "IDATEZ" = strftime(stop.time, "%d"),
                       "IYEARZ" = strftime(stop.time, "%Y"),
                       "ITIMEZ" = strftime(stop.time, "%H%M"))

  modstr <- 'Modified by write.restart.ED2'
  ed2in_new <- ed2in_set_value_list(tag_val_list, ed2in_orig, modstr)

  writeLines(ed2in_new, file.path(ed2in_path))

  # Remove old history.xml file, which job.sh looks for
  file.remove(file.path(mod_outdir, runid, "history.xml"))

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

