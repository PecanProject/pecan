# Grab the last 12 days worth of data
# Defaults to the midnight forecast.
# @author Luke Dramko

cur_date <- as.POSIXct(Sys.time(), tz = "UTC")
print(cur_date)

for (i in 1:1) {
  cur_date_str <- paste0(format(cur_date, "%Y-%m-%d"), " 00:00");
  print(paste("Running PEcAn for NOAA_GEFS for date", cur_date_str))
  
  #Generate the xml
  system(paste0("Rscript generate.gefs.xml.R gefs.sipnet.source.xml ", cur_date_str))
  
  #Run PEcAn
  cmd <- "Rscript"
  args <- c("/home/ldramko/pecan/web/workflow.R", "/home/ldramko/NEFI_tools/pecan_scripts/gefs.sipnet.source.xml")
  cmd_out <- system2(cmd, args, stdout=TRUE, stderr=TRUE)
  # cmd_out <- "Redirected to shell."
  
  #Obtain information about the run
  settings <- PEcAn.settings::read.settings("/home/ldramko/NEFI_tools/pecan_scripts/gefs.sipnet.source.xml")
  workflowid <- settings$workflow$id
  
  #Write workflow output to a file
  basefolder <- paste0("/fs/data3/ldramko/output/PEcAn_", workflowid);
  write(cmd_out, append = TRUE, file = file.path(basefolder, "workflow.log.txt"))
  
  #Run for the previous day
  cur_date <- cur_date - lubridate::days(1)
}