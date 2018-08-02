#' @title Get ED history restart file path
#'
#' @author Alexey Shiklomanov
#' @param mod_outdir Directory where PEcAn stores ensemble outputs. Usually 
#' \code{<workflowID>/out}
#' @param runid PEcAn run ID
#' @param file.time Start or end time from SDA analysis
#' @export
get_restartfile.ED2 <- function(mod_outdir, runid, file.time) {
    runid <- as.character(runid)
    histfile_path <- file.path(mod_outdir, runid)
    
    # the frequency of history files might change depending on assimilation timestep
    # we can identify this from the timestamps
    history_files <- dir(histfile_path, "-S-")
    # extract time stamps info from the file names
    htimestamps <- sapply(seq_along(history_files), function(f){
      index  <- gregexpr("-S-", history_files[1])[[1]]
      tstamp <- substr(history_files[f], index[1] + 3, index[1] + 12)
    })
    
    # NOTE: ED2 writes annual history files on the same month e.g. in ed_model.F90:
    #
    #           history_time   = new_month .and. isoutput /= 0 .and.                              &
    #                            current_time%month == imontha .and.                              &
    #                            mod(real(current_time%year-iyeara),frqstate) == 0.
    #
    # So the annual history file after starting a run at 1961/01/01 will be "history-S-1962-01-01-000000-g01.h5"
    # this version of code checks for annual -S- files, but putting these flags here to remind that there can be monthly or daily history files 
    annual_check <- monthly_check <- daily_check <- FALSE # this will result in reading the first file as YYYY-01-01 regardless of assimilation time step
    
    # if(length(htimestamps) > 1){
    #   diff_check     <- difftime(htimestamps[2], htimestamps[1], units = c("hours"))
    #   monthly_check  <- ifelse(diff_check > 744, FALSE, TRUE)
    #   daily_check    <- ifelse(diff_check > 24,  FALSE, TRUE)
    #   # if you want to extend this to checks for sub-daily assimilations, also modify timestamp extraction above
    # }
    
    # file.time comes from upstream in the format of "yyyy-12-31 23:59:59 UTC"
    # to match ED2 naming for annual history files, round to next day  "YYYY-01-01 UTC"
    ed.hist.annual <- lubridate::ceiling_date(file.time, "1 day")
     
    file_year     <-  ifelse(annual_check,  lubridate::year(file.time),  lubridate::year(ed.hist.annual))
    file_month    <-  ifelse(monthly_check, lubridate::month(file.time), lubridate::month(ed.hist.annual))
    file_day      <-  ifelse(daily_check,   lubridate::day(file.time),   lubridate::day(ed.hist.annual))
    
    # check how ED2 writes other -S- files
    

    datetime_string <- sprintf("%04d-%02d-%02d-000000", 
                               file_year, 
                               file_month,
                               file_day)
    histfile_string <- paste0("history-S-",
                              datetime_string,
                              ".*\\.h5$")

    histfile <- list.files(histfile_path, 
                           histfile_string, 
                           full.names = TRUE)
    if (length(histfile) > 1) {
        PEcAn.logger::logger.error("Multiple history files found.")
        return(NULL)
    } else if (length(histfile) < 1) {
        PEcAn.logger::logger.error("No history files found.")
        return(NULL)
    } else {
        PEcAn.logger::logger.info("Using history file: ",
                                  histfile)
        return(histfile)
    }
}

#' @title Get ED2 config.xml file
#'
#' @author Alexey Shiklomanov
#' @param rundir Model run directory. Usually \code{<workflowID>/run}
#' @inheritParams get_restartfile.ED2
get_configxml.ED2 <- function(rundir, runid) {
    runid <- as.character(runid)
    confxml_path <- file.path(rundir, runid, "config.xml")
    confxml <- XML::xmlToList(XML::xmlParse(confxml_path))
    return(confxml)
}

#' @title Generate ED2 cohort to patch mapping vector
#' 
#' @author Alexey Shiklomanov
#' @description Generate a vector of integer indices for mapping ED state 
#' cohort vectors onto patches, for instance for use with \code{tapply}.
#' @param nc \code{ncdf4} object for ED history restart file.
patch_cohort_index <- function(nc) {
    # Patch length
    paco_N <- ncdf4::ncvar_get(nc, "PACO_N")
    patch_index <- do.call(c, mapply(rep, seq_along(paco_N), paco_N))
    return(patch_index)
}

