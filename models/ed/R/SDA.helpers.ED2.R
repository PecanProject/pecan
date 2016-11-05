get_restartfile.ED2 <- function(mod_outdir, runid, file.time) {
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
        PEcAn.utils::logger.error("Multiple history files found.")
        return(NULL)
    } else if (length(histfile) < 1) {
        PEcAn.utils::logger.error("No history files found.")
        return(NULL)
    } else {
        PEcAn.utils::logger.info("Using history file: ",
                                  histfile)
        return(histfile)
    }
}

get_configxml.ED2 <- function(rundir, runid) {
    runid <- as.character(runid)
    confxml_path <- file.path(rundir, runid, "config.xml")
    confxml <- XML::xmlToList(XML::xmlParse(confxml_path))
    return(confxml)
}

patch_cohort_index <- function(nc) {
    # Patch length
    paco_N <- ncdf4::ncvar_get(nc, "PACO_N")
    patch_index <- do.call(c, mapply(rep, seq_along(paco_N), paco_N))
    return(patch_index)
}

ed2in_sub <- function(tag, value, ed2in) {
    regex <- sprintf("(%s[[:blank:]]+=[[:blank:]]+)([^[:blank:]]+)", tag)
    ed2in_out <- gsub(regex, 
                      paste0("\\1 ", value, 
                             "   !! Modified by write.restart.ED2"),
                      ed2in)
    return(ed2in_out)
}
