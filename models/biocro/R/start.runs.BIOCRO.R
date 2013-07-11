#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##' 
##' Start biocro model runs on local
##' @title Start run of biocro model
##' @param runid the id of the run (folder in runs) to execute
##' @export
##' @author Rob Kooper, David LeBauer, Deepak Jaiswal
start.runs.BIOCRO <- function(runid) {
  if (settings$run$host$name != "localhost") {
    stop("Only local runs are executed here")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  # run model
  # compute/download weather
  lat <- as.numeric(settings$run$site$lat)
  lon <- as.numeric(settings$run$site$lon)
  start.date <- settings$run$start.date
  end.date <- settings$run$end.date
  site.id = settings$run$site$id
  
  hostname <- system("hostname", intern = TRUE)
  if(settings$run$host$name == "localhost")  settings$run$host$name <- "hostname"
  con <- query.base.con(settings)
  site.info <- query.base(paste0("select * from sites where id = ", site.id, ";"), con = con)
  site.exists <- nrow(site.info) == 1
  # TODO code commented out below, code is not finished
  #if(site.exists){
  #  if(abs(lat - site.info$lat) + abs(lon - site.info(lon)))
  #}
  if(!site.exists){
    query.base(paste0("insert into sites (sitename, lat, lon) values(",
                      vecpaste(c(settings$run$site$name, lat, lon)), ");"), con = con)
  }
  
  ### TODO the following code should be run during write_configs and the file name passed to the start.runs function
  ### the next set of code using queries will be passed to a new function called "query.met"
  metfiles <- db.query(paste("select start_date, end_date, hostname, file_name, file_path ",
                   "from inputs join dbfiles on dbfiles.container_id = inputs.file_id ",
                   "join machines on dbfiles.machine_id = machines.id ",
                   "where start_date <= '", start.date, 
                   "' and end_date >= '", end.date, 
                   "' and site_id =", site.id, ";", sep = ""), con = con)
  
 
  if(nrow(metfiles) == 0){
    metfile.exists <- FALSE
  } else if(nrow(metfiles) >= 1){
    metfile.exists <- TRUE
    ## if > 1, use the first record
    metfile <- file.path(metfiles$file_path[1],
                         metfiles$file_name[1])

    if(!file.exists(metfile)){
      metfile.exists <- FALSE
    }
  }

  if(metfile.exists) {
      weather <- read.csv(metfile)
  } else if(!metfile.exists){
    weather.dir <- file.path(settings$run$dbfiles, "met", paste0(abs(lat),
                                    ifelse(lat>0,"N", "S"), "x",
                                    abs(lon),
                                    ifelse(lon>0, "E", "W")))
    weather <- InputForWeach(lat, lon, year(start.date), year(end.date))
    dir.create(weather.dir, recursive = TRUE, showWarnings = FALSE)
    weather.dir <- path.expand(weather.dir)
    write.csv(weather,
              file = file.path(weather.dir, "weather.csv"), row.names = FALSE)
    machine.id <- db.query(paste0("select id from machines where hostname = '", hostname, "';"), con = con)
    if(nrow(machine.id) == 0){
      machine.id <- db.query("select max(id) + 1 as id from machines;", con = con)
      db.query(paste0("insert into machines (id, hostname, created_at) values(",
                      vecpaste(c(machine.id, hostname, format(Sys.time()))), ");"), con = con)
    }
    file.id <- 1 + db.query(paste0(
      "select 1 + greatest(max(inputs.file_id), max(dbfiles.container_id)) as id ",
      " from inputs right join dbfiles on inputs.file_id =",
      "dbfiles.container_id;"), con = con)
    
    db.query(paste0("insert into dbfiles (file_name, file_path, created_at, machine_id, container_id) ",
                    "values(", vecpaste(c('weather.csv', weather.dir, format(Sys.time()), machine.id, file.id)),");"), con = con)
    db.query(paste0("insert into inputs ",
                    "(notes, created_at, site_id, file_id, start_date, ",
                    "end_date, access_level, format_id) ",
                    "values('downloaded from NCEP', now(),",
                    vecpaste(c(site.id, file.id, start.date, end.date, 4, 28)), ");"), con = con)
    
  }
  query.close(con)
  
  weather2 <- weachNEW(weather, lati = lat, ts = 1, 
                       temp.units="Celsius", rh.units="fraction", 
                       ws.units="mph", pp.units="in")
  colnames(weather2) <- c("year", "doy", "hour", "solarR", "DailyTemp.C", "RH", "WindSpeed", "precip")
  
  # run model
  
  config <- xmlToList(xmlParse(file.path(rundir, "config.xml")))
  
  pp.config <- config$pft$photoParms
  pp <- photoParms(vmax=pp.config$vmax, b0=pp.config$b0, b1 = pp.config$b1,Rd=pp.config$Rd)
  cc <- canopyParms(Sp = config$pft$canopyParms$Sp)

  BioGro_result <- BioGro(weather2, photoControl=pp, canopyControl=cc)
  
  write.csv(with(BioGro_result,
                 data.frame(DayofYear, Hour, ThermalT, Stem, Leaf, Root, Rhizome, Grain, LAI, SoilEvaporation, CanopyTrans)), 
            file=file.path(outdir, "result.csv"))

  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
