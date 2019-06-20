##'
##' Rounds a date to the previous 6 hours (0:00, 6:00, 12:00, or 18:00).
##' 
##' @author Luke Dramko
round.to.six.hours <-
  function(date = Sys.time() - lubridate::hours(2)) {
    if (is.character(date)) {
      date <- as.POSIXct(date, tz = "UTC")
    }
    forecast_hour = (lubridate::hour(date) %/% 6) * 6 #Integer division by 6 followed by re-multiplication acts like a "floor function" for multiples of 6
    forecast_hour = sprintf("%04d", forecast_hour * 100)
    date = as.POSIXct(
      paste0(
        lubridate::year(date),
        "-",
        lubridate::month(date),
        "-",
        lubridate::day(date),
        " ",
        substring(forecast_hour, 1, 2),
        ":00:00"
      ),
      tz = "UTC"
    )
    
    return(date)
  }


ploting_fluxes <- function(obs.raw) {
  obs.plot <- obs.raw %>%
    tidyr::gather(Param, Value,-c(Date)) %>%
    filter(!(
      Param %in% c(
        "FjDay",
        "U",
        "Day",
        "DoY",
        "FC",
        "FjFay",
        "Hour",
        "Month",
        "SC",
        "Ustar",
        "Year",
        "H",
        "Flag"
      )
    ),
    Value != -999) %>%
    #filter((Date %>% as.Date) %in% (names(prep.data) %>% as.Date())) %>%
    ggplot(aes(Date, Value)) +
    geom_line(aes(color = Param), lwd = 1) +
    geom_point(aes(color = Param), size = 3) +
    facet_wrap(~ Param, scales = "free", ncol = 1) +
    scale_x_datetime(
      breaks = scales::date_breaks("12 hour"),
      labels = scales::date_format("%m/%d-%H:00")
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 15) +
    geom_hline(yintercept = 0)+
    labs(y = "") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  return(obs.plot)
}

# # Settings file (including path) is passed in on the command line.
# args <- commandArgs(trailingOnly = TRUE)
# if (length(args) < 1) {
#   print("Please include an xml file as a command line argument.")
#   quit("no", status = 10)
# }
# if (!file.exists(args[1])) {
#   print(paste0("File ", args[1], " does not exist."))
#   quit("no", status = 11)
# }
# if (!is.na(args[2])) {
#   source_date <- args[2]
# }

# # These lines from the PEcAn workflow load the settings object.
# # settings <- PEcAn.settings::read.settings(args[1])
# 
# xmloutdir <- regmatches(args[1], regexpr("(~|\\./)?(/)?([^/]*/)*", args[1])) # extract xmloutdir from args[1]
# filename <- basename(args[1]) # Extract file name from args[1]
# 
# if(xmloutdir == "") {xmloutdir <- "."}

# dbparms = list()
# dbparms$dbname = "bety"
# dbparms$host = "128.197.168.114"
# dbparms$user = "bety"
# dbparms$password = "bety"
# 
# #Connection code copied and pasted from met.process
# bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
#                             host     = dbparms$host, 
#                             user     = dbparms$user, 
#                             password = dbparms$password)
# 
# con <- bety$con #Connection to the database.  dplyr returns a list.
# if (is.null(con)) {
#   print("Database connection failed.")
#   quit("no", status=12)
# }

# # Set the run dates
# if (exists("source_date")) {
#   start_date <- round.to.six.hours(source_date)
# } else {
#   start_date <- round.to.six.hours()
# }
# 
# end_date <- start_date + lubridate::days(16)
# settings$run$start.date <- as.character(start_date)
# settings$run$end.date <- as.character(end_date)

#settings$ensemble$start.year <- start_date
#settings$ensemble$end.year <- end_date

# Update the time of the run
# settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), " +0000")
# 
# # Update met.start and met.end for the xml (shouldn't make a difference,
# # but good practice anyway).
# settings$run$site$met.start <- format(Sys.time() - lubridate::days(12), "%Y-%m-%d")
# settings$run$site$met.end <- format(Sys.time() + lubridate::days(16), "%Y-%m-%d")
# 
# settings$ensemble$start.year <- format(start_date, "%Y")
# settings$ensemble$end.year <- as.character(end_date, "%Y")
# 
# # Create new workflow ID and register it with the database
# hostname <- PEcAn.remote::fqdn()
# query <- paste0("INSERT INTO workflows (site_id, model_id, notes, folder, hostname, start_date,", 
#                 "end_date, params, advanced_edit) ", 
#                 "values (", settings$run$site$id, ", ", settings$model$id, ", ", "''", ", '', '", hostname, "', '",
#                 format(start_date, "%Y/%m/%d %H:%M:%S"), "', '", format(end_date, "%Y/%m/%d %H:%M:%S"), "', ", "''", ", ", "true) RETURNING id")
# workflowid <- PEcAn.DB::db.query(query, con = con)
# workflowid <- as.character(unlist(workflowid))
# 
# settings$workflow$id <- workflowid
# 
# #The outdirectory is specific to a particular workflow
# outdir <- settings$outdir
# if (substr(outdir, nchar(outdir), nchar(outdir)) == "/") {
#   outdir <- substr(outdir, 1, nchar(outdir) -1 )
# }

# basedir <- regmatches(outdir, regexpr("(~|\\./)?(/)?([^/]*/)*", outdir))
# outdir <- paste0(basedir, "PEcAn_", workflowid, "/")
# settings$outdir <- outdir
# 
# # Create the output directory.  PEcAn does not do this for you.  It's normally the job of the
# # web interface's php code.
# if (!dir.exists(outdir)) {
#   dir.create(outdir, recursive=TRUE, showWarnings = FALSE)
# }
# 
# 
# # Log workflow ids generated by this system
# write(workflowid, append = TRUE, file = file.path(.wd, "workflow_id_log.txt"))
# 
# PEcAn.DB::db.close(con)
# 
# # Write out the file with updated settings
# PEcAn.settings::write.settings(settings, outputfile = filename, outputdir = xmloutdir)

