#' @title Get ED history restart file path
#'
#' @author Alexey Shiklomanov
#' @param mod_outdir Directory where PEcAn stores ensemble outputs. Usually 
#' \code{<workflowID>/out}
#' @param runid PEcAn run ID
#' @param file.time Start or end time from SDA analysis
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
    
    # if this is the first year of an annual assimilation, htimestamp will have one element like "1961-01-01"
    # if this is the first year of a monthly assimilation, htimestamp will have 12 elements like "1961-01-01", "1961-02-01", ...
    # and so on...
    # the first timestamp will be like YYYY-01-01 regardless from assimilation time step
    # in that case we want to read both y/d/m from stop.time, otherwise we decide below
    # this is because of ED2's weird file naming, writes out annual history files as "history-S-1960-01-01-000000-g01.h5"
    # not as "history-S-1960-00-00-000000-g01.h5" or "history-S-1960-12-31-000000-g01.h5"
    annual_check <- TRUE
    monthly_check <- daily_check <- FALSE # this will result in reading the first file as YYYY-01-01 regardless of assimilation time step
    if(length(htimestamps) > 1){
      diff_check     <- difftime(htimestamps[2], htimestamps[1], units = c("hours"))
      monthly_check  <- ifelse(diff_check > 744, FALSE, TRUE)
      daily_check    <- ifelse(diff_check > 24,  FALSE, TRUE)
      # if you want to extend this to checks for sub-daily assimilations, also modify timestamp extraction above
    }
    
    file_year  <- lubridate::year(file.time) # always get year
    file_month <- ifelse(monthly_check, lubridate::month(file.time), 1)
    if(daily_check){
      file_day <- lubridate::day(file.time)
    }else{
      # unfortunately ED2 writes monthly files as YYYY-MM-00 
      # need to double check these while working with time steps other than annual
      file_day <- ifelse(monthly_check, 0, 1)
    }
    

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

