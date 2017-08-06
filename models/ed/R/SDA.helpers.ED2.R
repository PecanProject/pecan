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

    file_year <- lubridate::year(file.time)
    file_month <- lubridate::month(file.time)
    file_day <- lubridate::day(file.time)
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

